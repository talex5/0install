(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Keeps track of download progress. *)

open Support.Common
open Gtk_common
open Zeroinstall

let make_watcher solver_box backend reqs =
  let config = Gui.config backend in
  let distro = Gui.distro backend in
  let feed_provider = ref (new Feed_provider_impl.feed_provider config distro) in
  let original_solve = Solver.solve_for config !feed_provider reqs in
  let original_selections =
    match original_solve with
    | (false, _) -> None
    | (true, results) -> Some (Solver.selections results) in

  object (_ : #Progress.watcher)
    val mutable n_completed_downloads = 0
    val mutable size_completed_downloads = 0L
    val mutable downloads = []
    val mutable pulse = None

    val mutable results = original_solve

    method feed_provider = !feed_provider
    method results = results
    method original_selections = original_selections

    method update (new_results, new_fp) =
      feed_provider := new_fp;
      results <- new_results;

      Gtk_utils.async (fun () ->
        lwt box = solver_box in
        box#update;
        Lwt.return ()
      )

    method report feed_url msg =
      let msg = Printf.sprintf "Feed '%s': %s" (Feed_url.format_url feed_url) msg in
      Gtk_utils.async (fun () ->
        lwt box = solver_box in
        box#report_error (Safe_exception (msg, ref []));
        Lwt.return ()
      )

    method monitor dl =
      log_debug "start_monitoring %s" dl.Downloader.url;
      downloads <- dl :: downloads;

      if pulse = None then (
        pulse <- Some (
          try_lwt
            while_lwt downloads <> [] do
              lwt () = Lwt_unix.sleep 0.2 in
              downloads <- downloads |> List.filter (fun dl ->
                if Downloader.is_in_progress dl then true
                else (
                  log_debug "stop_monitoring %s" dl.Downloader.url;
                  let progress = Lwt_react.S.value dl.Downloader.progress in
                  n_completed_downloads <- n_completed_downloads + 1;
                  size_completed_downloads <- Int64.add size_completed_downloads progress.Downloader.bytes_so_far;
                  false
                )
              );
              lwt box = solver_box in
              box#update_download_status ~n_completed_downloads ~size_completed_downloads downloads;
              Lwt.return ()
            done
          with ex ->
            log_warning ~ex "GUI update failed";
            Lwt.return ()
          finally
            pulse <- None;
            (* We do this here, rather than in [stop_monitoring], to introduce a delay,
             * since we often start a new download immediately after another one finished and
             * we don't want to reset in that case. *)
            n_completed_downloads <- 0;
            size_completed_downloads <- 0L;
            Lwt.return ()
        )
      )

    method impl_added_to_store =
      Gtk_utils.async (fun () ->
        lwt box = solver_box in
        box#impl_added_to_store;
        Lwt.return ()
      )

    method confirm_keys feed_url infos =
      lwt box = solver_box in
      lwt parent = box#ensure_main_window in
      let trust_db = Gui.trust_db backend in
      Trust_box.confirm_keys ~parent config trust_db feed_url infos

    method confirm message =
      lwt box = solver_box in
      lwt parent = box#ensure_main_window in

      let box = GWindow.message_dialog
        ~parent
        ~message_type:`QUESTION
        ~title:"Confirm"
        ~message
        ~buttons:GWindow.Buttons.ok_cancel
        () in
      let result, set_result = Lwt.wait () in
      box#set_default_response `OK;
      box#connect#response ==> (fun response ->
        box#destroy ();
        Lwt.wakeup set_result (
          match response with
          | `OK -> `ok
          | `CANCEL | `DELETE_EVENT -> `cancel
        )
      );
      box#show ();
      result

    method abort_all_downloads =
      downloads |> List.iter (fun dl ->
        Gtk_utils.async dl.Downloader.cancel
      )
  end
