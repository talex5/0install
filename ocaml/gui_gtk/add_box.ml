(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** The dialog for adding a new app (used by "0desktop") *)

open Support.Common
open Gtk_common
open Zeroinstall

module F = Zeroinstall.Feed
module U = Support.Utils
module FC = Zeroinstall.Feed_cache

let create ~(gui:Zeroinstall.Ui.ui_handler) backend initial_uri =
  let finished, set_finished = Lwt.wait () in

  let dialog = GWindow.dialog ~title:"Add New Application" () in
  (* Missing from lablgtk: dialog#set_keep_above true; *)

  let frame = GBin.frame ~packing:(dialog#vbox#pack ~expand:true) ~shadow_type:`NONE ~border_width:8 () in
  let title = GMisc.label ~markup:"<b>Application to install</b>" () in
  frame#set_label_widget (Some (title :> GObj.widget));
  let vbox = GPack.vbox ~packing:frame#add ~border_width:12 ~spacing:12 () in
  GMisc.label
    ~packing:vbox#pack
    ~xalign:0.0
    ~line_wrap:true
    ~markup:"<i>Enter the URI of the application you want to install, \
             or drag its link from a web-browser into this window.</i>"
    () |> ignore_widget;

  let hbox = GPack.hbox ~packing:vbox#pack ~spacing:4 () in
  GMisc.label ~packing:hbox#pack ~text:"URI:" () |> ignore_widget;
  let entry = GEdit.entry ~packing:(hbox#pack ~expand:true) ~activates_default:true ~text:initial_uri () in

  (* Buttons *)
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `ADD `ADD;
  dialog#set_default_response `ADD;

  let set_uri_ok () =
    dialog#set_response_sensitive `ADD (entry#text <> "") in
  entry#connect#changed ==> set_uri_ok;
  set_uri_ok ();

  let add () =
    let iface = entry#text in
    Gtk_utils.sanity_check_iface iface;
    dialog#misc#set_sensitive false;
    try_lwt
      let reqs = Zeroinstall.Requirements.default_requirements iface in
      match_lwt gui#run_solver `Download_only reqs ~refresh:false with
      | `Aborted_by_user -> Lwt.return ()
      | `Success _ ->
          Gui.xdg_add_to_menu backend iface;
          dialog#destroy ();
          Lwt.wakeup set_finished ();
          Lwt.return ()
    finally
      dialog#misc#set_sensitive true;
      Lwt.return () in

  (* Drag-and-drop *)

  Gtk_utils.make_iface_uri_drop_target dialog (fun iface ->
    log_info "URI dropped: %s" iface;
    entry#set_text iface;
    Gtk_utils.async ~parent:dialog add;
    true
  );

  dialog#connect#response ==> (function
    | `DELETE_EVENT | `CANCEL -> dialog#destroy (); Lwt.wakeup set_finished ()
    | `ADD -> Gtk_utils.async ~parent:dialog add;
  );
  dialog#show ();


  finished
