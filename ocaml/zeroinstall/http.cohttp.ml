open Support
open Support.Common

(* Possible certificate files; stop after finding one.
   Based on https://golang.org/src/crypto/x509 *)
let cert_files = [
  "/etc/ssl/certs/ca-certificates.crt";                (* Debian/Ubuntu/Gentoo etc. *)
  "/etc/pki/tls/certs/ca-bundle.crt";                  (* Fedora/RHEL 6 *)
  "/etc/ssl/ca-bundle.pem";                            (* OpenSUSE *)
  "/etc/pki/tls/cacert.pem";                           (* OpenELEC *)
  "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"; (* CentOS/RHEL 7 *)
  "/etc/ssl/cert.pem";                                 (* Alpine Linux / OpenBSD *)

  "/var/ssl/certs/ca-bundle.crt";           (* AIX *)

  "/usr/local/etc/ssl/cert.pem";            (* FreeBSD *)
  "/usr/local/share/certs/ca-root-nss.crt"; (* DragonFly *)
  "/etc/openssl/certs/ca-certificates.crt"; (* NetBSD *)

  "/sys/lib/tls/ca.pem";                (* Plan9 *)

  "/etc/certs/ca-certificates.crt";     (* Solaris 11.2+ *)
  "/etc/ssl/cacert.pem";                (* OmniOS *)
]

(* Possible directories with certificate files; stop after successfully
   reading at least one file from a directory.
   Based on https://golang.org/src/crypto/x509/root_unix.go *)
let cert_directories = [
  "/etc/ssl/certs";               (* Debian, SLES10/SLES11, https://golang.org/issue/12139 *)
  "/system/etc/security/cacerts"; (* Android *)
  "/usr/local/share/certs";       (* FreeBSD *)
  "/etc/pki/tls/certs";           (* Fedora/RHEL *)
  "/etc/openssl/certs";           (* NetBSD *)
  "/var/ssl/certs";               (* AIX *)
]

let () =
  Printexc.register_printer @@ function
  | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
  | Tls_lwt.Tls_failure x -> Some ("TLS alert: " ^ Tls.Engine.string_of_failure x)
  | _ -> None

let is_file path =
  log_debug "Checking for certificate file %S" path;
  match Unix.stat path with
  | x -> x.Unix.st_kind = Unix.S_REG;
  | exception _ -> false

let is_dir path =
  log_debug "Checking for certificate directory %S" path;
  match Unix.stat path with
  | x -> x.Unix.st_kind = Unix.S_DIR;
  | exception _ -> false

let authenticator = lazy (
  match List.find_opt is_file cert_directories with
  | Some cert_file -> `Ca_file cert_file
  | None ->
    match List.find_opt is_dir cert_directories with
    | Some dir -> `Ca_dir dir
    | None ->
      log_warning "@[<v2>No certificates found! I tried these files:@,%a@,and these directories:@,%a@]"
        Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) cert_directories
        Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) cert_files;
      `Ca_dir (List.hd cert_directories)
)

module Net = struct
  (* Cohttp silently skips certificate validation, so provide our own connection code.
     See https://github.com/mirage/ocaml-conduit/issues/153 *)

  module IO = struct
    type 'a t = 'a Lwt.t

    let (>>=) = Lwt.bind
    let return = Lwt.return

    type ic = Lwt_io.input_channel
    type oc = Lwt_io.output_channel
    type conn = unit

    exception IO_error of exn

    let wrap_errors fn =
      Lwt.catch fn (fun ex -> Lwt.fail (IO_error ex))

    let read_line ic =
      if Lwt_io.is_closed ic then Lwt.return_none
      else wrap_errors (fun () -> Lwt_io.read_line_opt ic)

    let read ic count =
      if Lwt_io.is_closed ic then Lwt.return "" else (
        let count = min count Sys.max_string_length in
        Lwt.catch
          (fun () -> Lwt_io.read ~count ic)
          (function
            | End_of_file -> Lwt.return ""
            | ex -> Lwt.fail (IO_error ex)
          )
      )

    let write oc x = wrap_errors (fun () -> Lwt_io.write oc x)

    let flush oc = wrap_errors (fun () -> Lwt_io.flush oc)

    type error = exn

    let catch f =
      Lwt.try_bind f
        (fun x -> Lwt.return (Ok x))
        (function
          | IO_error e -> Lwt.return (Error e)
          | e -> Lwt.fail e
        )

    let pp_error f ex = Format.pp_print_string f (Printexc.to_string ex)
  end

  type ctx = Cohttp_lwt_unix.Net.ctx
  let sexp_of_ctx = Cohttp_lwt_unix.Net.sexp_of_ctx

  let default_ctx = Cohttp_lwt_unix.Net.default_ctx

  let close c = Lwt.catch
    (fun () -> Lwt_io.close c)
    (fun e ->
      Logs.warn (fun f -> f "Closing channel failed: %s" (Printexc.to_string e));
      Lwt.return_unit
    )

  let close_in ic = Lwt.ignore_result (close ic)

  let close_out oc = Lwt.ignore_result (close oc)

  let close ic oc = Lwt.ignore_result (close ic >>= fun () -> close oc)

  let ip_of_host host =
    Lwt_unix.getaddrinfo host "" [Lwt_unix.AI_SOCKTYPE SOCK_STREAM] >|= fun addrs ->
    match List.find_opt (fun i -> i.Unix.ai_family = Unix.PF_INET) addrs with
    | Some { Unix.ai_addr = Unix.ADDR_INET (ipv4_addr, _port); _ } -> Ipaddr_unix.of_inet_addr ipv4_addr
    | _ ->
      match List.find_opt (fun i -> i.Unix.ai_family = Unix.PF_INET6) addrs with
      | Some { Unix.ai_addr = Unix.ADDR_INET (addr, _port); _ } -> Ipaddr_unix.of_inet_addr addr
      | _ -> Safe_exn.failf "No IP address found for hostname %S" host

  let log_sexp x =
    log_debug "TLS: %a" Sexplib.Sexp.pp_hum x

  let connect_tls host sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
        X509_lwt.authenticator (Lazy.force authenticator) >>= fun authenticator ->
        let config = Tls.Config.client ~authenticator () in
        Lwt_unix.connect fd sa >>= fun () ->
        let trace = if Support.Logging.will_log Support.Logging.Debug then Some log_sexp else None in
        Tls_lwt.Unix.client_of_fd config ?trace ~host fd >|= fun t ->
        let ic, oc = Tls_lwt.of_t t in
        (fd, ic, oc)
      )

  let connect_uri ~ctx uri =
    match Uri.host uri with
    | None -> Safe_exn.failf "Missing host in URI %a" Uri.pp uri
    | Some host ->
      ip_of_host host >>= fun ip ->
      match Uri.scheme uri with
      | Some "http" ->
        let port = Uri.port uri |> default 80 in
        Conduit_lwt_unix.connect ~ctx:ctx.Cohttp_lwt_unix.Net.ctx (`TCP (`IP ip, `Port port)) >|= fun (_flow, ic, oc) ->
        ((), ic, oc)
      | Some "https" ->
        let port = Uri.port uri |> default 443 in
        let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip ,port) in
        connect_tls host sa >|= fun (_fd, ic, oc) ->
        ((), ic, oc)
      | Some s -> Safe_exn.failf "Unsupported scheme %S in %a" s Uri.pp uri
      | None -> Safe_exn.failf "Missing URI scheme in %a" Uri.pp uri
end

module Client = Cohttp_lwt.Make_client(Net.IO)(Net)

let next s =
  Lwt.catch
    (fun () -> Lwt_stream.next s >|= fun x -> Ok x)
    (function
      | Lwt_stream.Empty -> Lwt.return (Error `End_of_stream)
      | ex -> Lwt.fail ex
    )

(* Drop (up to) the first [n] bytes from [str], updating [n] in the process. *)
let drop n str =
  if !n = 0L then str
  else (
    let len = Int64.of_int (String.length str) in
    if len <= !n then (
      n := Int64.sub !n len;
      ""
    ) else (
      let str = XString.tail str (Int64.to_int !n) in
      n := 0L;
      str
    )
  )

let day_name tm =
  match tm.Unix.tm_wday with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | x -> failwith (Printf.sprintf "Invalid day number %d!" x)

let month_name tm =
  match tm.Unix.tm_mon with
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | x -> failwith (Printf.sprintf "Invalid month number %d!" x)

let if_modified_since time headers =
  match time with
  | None -> headers
  | Some time ->
    let tm = Unix.gmtime time in
    let date = Printf.sprintf "%s, %02d %s %d %2d:%2d:%2d GMT"
        (day_name tm) (tm.Unix.tm_mday) (month_name tm) (tm.Unix.tm_year + 1900)
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Cohttp.Header.add headers "If-Modified-Since" date

let set_user_agent headers =
  Cohttp.Header.prepend_user_agent headers ("0install/" ^ About.version)

module Proxy = struct
  (* http_proxy's format doesn't seem to be specified anywhere. Try to do what libcurl does. *)

  let re_scheme = Str.regexp {|^[A-Za-z][-A-Za-z0-9+.]\+://|}

  let name_of_scheme = function
    | `Http -> "http"
    | `Https -> "https"

  let try_var var =
    match Unix.getenv var with
    | exception Not_found -> None
    | "" -> None
    | proxy ->
      log_info "Found %s=%S" var proxy;
      try
        if Str.string_match re_scheme proxy 0 then Some (Uri.of_string proxy)
        else Some (Uri.of_string ("http://" ^ proxy))
      with ex ->
        log_warning ~ex "Failed to parse $%s value %S (should be e.g. 'http://host:port')" var proxy;
        None

  let get scheme =
    match try_var (name_of_scheme scheme ^ "_proxy") with
    | Some _ as proxy -> proxy
    | None -> try_var "all_proxy"
end

let http ?(body=Cohttp_lwt.Body.empty) ~headers meth url fn =
  let req = Cohttp.Request.make_for_client ~headers ~chunked:false meth (Uri.of_string url) in
  let req, proxy =
    match Proxy.get `Http with
    | None -> req, Uri.of_string url
    | Some proxy -> {req with Cohttp.Request.resource = url}, proxy
  in
  Lwt.catch
    (fun () ->
       (* We only want to make one call, but only [callv] lets us pass in a
          Request, which we need to do in order to support proxies. *)
       Client.callv proxy (Lwt_stream.of_list [req, body]) >>= fun resps ->
       Lwt_stream.map_s fn resps |> Lwt_stream.to_list >|= function
       | [] -> failwith "callv didn't return any responses!"
       | [x] -> x
       | _ -> failwith "callv returned multiple responses!"
    )
    (fun ex ->
       let m = Cohttp.Code.string_of_method meth in
       log_warning ~ex "HTTP %s of %S failed" m url;
       Lwt.return (`Network_failure (Format.asprintf "@[<h>HTTP %s of %S failed: %s@]" m url (Printexc.to_string ex)))
    )

(** Download the contents of [url] into [ch].
 * This runs in a separate (either Lwt or native) thread. *)
let download_no_follow ~cancelled ?size ?modification_time ~start_offset ~progress ch url =
  let skip_bytes = ref start_offset in
  let headers = Cohttp.Header.init ()
                |> if_modified_since modification_time
                |> set_user_agent
  in
  log_info "HTTP GET %S" url;
  http `GET ~headers url @@ fun (resp, body) ->
    let body = Cohttp_lwt.Body.to_stream body in
    let headers = Cohttp.Response.headers resp in
    match Cohttp.Response.status resp, Cohttp.Header.get headers "location" with
    | #Cohttp.Code.redirection_status, Some target ->
      let url = Uri.of_string url in
      let rel_target = Uri.of_string target in
      let target = Uri.resolve "http" url rel_target in
      log_info "Redirect from '%a' to '%a' (%a)" Uri.pp url Uri.pp rel_target Uri.pp target;
      Lwt.return (`Redirect (Uri.to_string target))
    | `Not_modified, _ ->
      Lwt.return `Unmodified
    | `OK, _ ->
      begin
        let progress_total_size =
          match size with
          | Some _ -> size
          | None -> (* We don't know the expected length, but maybe the server told us in the headers: *)
            match Cohttp.Header.get headers "content-length" with
            | None -> None
            | Some len -> Int64.of_string_opt len
        in
        (* if Support.Logging.will_log Support.Logging.Debug then Curl.set_verbose connection true; *)
        let rec copy total =
          if !cancelled then Lwt.return `Aborted_by_user
          else (
            next body >>= function
            | Error `End_of_stream -> Lwt.return (`Success total)
            | Ok data ->
              let total = Int64.add total (Int64.of_int (String.length data)) in
              match size with
              | Some limit when total > limit ->
                Lwt.return (`Network_failure "Download exceeded expected size!")
              | _ ->
                progress (total, progress_total_size, false);
                let data = drop skip_bytes data in
                begin
                  try
                    output_string ch data;
                  with ex ->
                    log_warning ~ex "Failed to write download data to temporary file";
                    Safe_exn.failf "Failed to write download data to temporary file: %s" (Printexc.to_string ex);
                end;
                copy total
          )
        in
        copy 0L >|= function
        | `Network_failure _ | `Aborted_by_user as e -> e
        | `Success actual_size ->
          match size with
          | Some expected when expected <> actual_size ->
            `Network_failure (Format.asprintf
                                "@[<v>Downloaded archive has incorrect size.@,\
                                 URL: %s@,\
                                 Expected: %Ld bytes@,\
                                 Received: %Ld bytes@]" url expected actual_size)
          | _ ->
            log_info "Download '%s' completed successfully (%Ld bytes)" url actual_size;
            `Success
      end
    | status, _ ->
      let msg = Format.asprintf "@[<h>Error downloading '%s': The requested URL returned error: %d@]"
          url
          (Cohttp.Code.code_of_status status)
      in
      Lwt.return (`Network_failure msg)

module Ftp = struct
  let read_reply from_server =
    Lwt_io.read_line from_server >>= fun line ->
    log_info "FTP: <<< %S" line;
    match line.[3] with
    | ' ' -> Lwt.return line
    | '-' ->
      let end_pattern = String.sub line 0 3 ^ " " in
      let rec aux () =
        Lwt_io.read_line from_server >>= fun extra ->
        log_info "FTP: <<< %S" extra;
        if XString.starts_with extra end_pattern then Lwt.return line
        else aux ()
      in
      aux ()
    | _ -> Safe_exn.failf "Invalid FTP response %S" line

  let read_complete_reply from_server =
    read_reply from_server >|= fun line ->
    if line.[0] <> '2' then Safe_exn.failf "Error from FTP server: %S" line

  let send sock cmd =
    log_info "FTP: >>> %S" cmd;
    if String.contains cmd '\n' || String.contains cmd '\r' then
      Safe_exn.failf "Newline in FTP command %S!" cmd;
    let cmd = cmd ^ "\r\n" in
    let rec aux start =
      let len = String.length cmd - start in
      if len = 0 then Lwt.return ()
      else  (
        Lwt_unix.write_string sock cmd start len >>= fun sent ->
        assert (sent > 0);
        aux (start + sent)
      )
    in
    aux 0

  (* https://tools.ietf.org/html/rfc1123#page-31 says:
     "The format of the 227 reply to a PASV command is not well standardized." *)
  let re_passive_response = Str.regexp {|.*[0-9]+,[0-9]+,[0-9]+,[0-9]+,\([0-9]+\),\([0-9]+\)|}

  (* Request a passive-mode transmission, and return the new port number. *)
  let initiate_passive sock from_server =
      send sock "PASV" >>= fun () ->
      read_reply from_server >|= fun data_addr ->
      if not (XString.starts_with data_addr "227 ") then
        Safe_exn.failf "Expected 227 reply to PASV, but got %S" data_addr;
      if Str.string_match re_passive_response data_addr 0 then (
        let port_high = Str.matched_group 1 data_addr |> int_of_string in
        let port_low = Str.matched_group 2 data_addr |> int_of_string in
        (port_high lsl 8) + port_low
      ) else (
        Safe_exn.failf "Failed to parse %S as a passive address" data_addr
      )

  let download_data ~host ~port ch =
    let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt.finalize
      (fun () ->
         Lwt_unix.connect sock (Unix.ADDR_INET (host, port)) >>= fun () ->
         let buf = Bytes.create 4096 in
         let rec aux () =
           Lwt_unix.recv sock buf 0 (Bytes.length buf) [] >>= fun got ->
           if got = 0 then Lwt.return_unit
           else (
             output ch buf 0 got;
             aux ()
           )
         in
         aux ()
      )
      (fun () -> Lwt_unix.close sock)

  let await_completion from_server =
    read_reply from_server >>= fun line ->
    match line.[0] with
    | '1' -> read_complete_reply from_server
    | _ -> Safe_exn.failf "Invalid FTP response %S (expected '1xx' code) " line

  let get ~cancelled ?size ?modification_time ~start_offset ~progress ch uri =
    ignore (cancelled, size, modification_time, start_offset, progress, ch);
    match Uri.host uri with
    | None -> Safe_exn.failf "Missing host in URL %a" Uri.pp uri
    | Some host ->
      let path = Uri.path uri in
      Lwt_unix.gethostbyname host >>= fun host_entry ->
      let addr = host_entry.Unix.h_addr_list.(0) in
      log_info "FTP: resolved host %S to address %s" host (Unix.string_of_inet_addr addr);
      let port = Uri.port uri |> default 21 in
      let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
      Lwt.finalize
        (fun () ->
           Lwt_unix.connect sock (Unix.ADDR_INET (addr, port)) >>= fun () ->
           let from_server = Lwt_io.(of_fd ~mode:input) sock in
           read_complete_reply from_server >>= fun () ->
           send sock "USER anonymous" >>= fun () ->
           read_reply from_server >>= fun line ->
           begin match line.[0] with
             | '2' -> Lwt.return ()
             | '3' ->
               send sock "PASS anonymous@" >>= fun () ->
               read_complete_reply from_server
             | _ -> Safe_exn.failf "Anonymous FTP login failed: %S" line
           end >>= fun () ->
           send sock "TYPE I" >>= fun () ->
           read_complete_reply from_server >>= fun () ->
           initiate_passive sock from_server >>= fun pasv_port ->
           let thread = download_data ~host:addr ~port:pasv_port ch in
           send sock ("RETR " ^ path) >>= fun () ->
           await_completion from_server >>= fun () ->
           thread >>= fun () ->
           Lwt.return `Success
        )
        (fun () -> Lwt_unix.close sock)
end

module Connection = struct
  type t = unit

  let create () = ()

  let release () = ()

  let get ~cancelled ?size ?modification_time ?(start_offset=Int64.zero) ~progress () ch url =
    let parsed = Uri.of_string url in
    match Uri.scheme parsed with
    | Some "http" | Some "https" -> download_no_follow ~cancelled ?size ?modification_time ~start_offset ~progress ch url
    | Some "ftp" -> Ftp.get ~cancelled ?size ~start_offset ~progress ch parsed
    | Some x -> Safe_exn.failf "Unsupported URI scheme %S in %S" x url
    | None -> Safe_exn.failf "Missing URI scheme in %S" url
end

let escape x = Uri.pct_encode ~component:`Path x

let variant = "cohttp (OCaml)"

let post ~data url =
  let body = Cohttp_lwt.Body.of_string data in
  let headers = Cohttp.Header.init ()
                |> set_user_agent in
  http `POST ~body ~headers url (fun (resp, body) ->
      Cohttp_lwt.Body.to_string body >|= fun body ->
      match Cohttp.Response.status resp with
      | `OK -> `Success body
      | status ->
        let msg = Format.asprintf "@[<h>Error posting to '%s': The requested URL returned error: %d@]"
            url
            (Cohttp.Code.code_of_status status) in
        `Failed (msg, body)
    ) >|= function
  | `Success body -> Ok body
  | `Failed e -> Error e
  | `Network_failure e -> Error (e, "")
