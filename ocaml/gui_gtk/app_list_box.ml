(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** The app browser dialog *)

open Zeroinstall.General
open Zeroinstall
open Support.Common
open Gtk_common

module F = Zeroinstall.Feed
module U = Support.Utils
module FC = Zeroinstall.Feed_cache
module Feed_url = Zeroinstall.Feed_url
module Basedir = Support.Basedir

let by_name_ignore_case (n1, p1, u1) (n2, p2, u2) =
  let r = String.compare (String.lowercase n1) (String.lowercase n2) in
  if r <> 0 then r
  else compare (p1, u1) (p2, u2)

let get_selections backend ~(gui:Ui.ui_handler) uri =
  let reqs = Requirements.default_requirements uri in
  match Gui.quick_solve backend reqs with
  | Some sels -> Lwt.return (`Success sels)
  | None ->
      (* Slow path: program isn't cached yet *)
      gui#run_solver `Download_only reqs ~refresh:false

let show_help_for_iface backend ~gui uri : unit Lwt.t =
  match_lwt get_selections backend ~gui uri with
  | `Aborted_by_user -> Lwt.return ()
  | `Success sels ->
      let sel = Selections.(get_selected_ex {iface = uri; source = false} sels) in
      Gui.show_help_exn backend sel;
      Lwt.return ()

let confirm_deletion ~parent name =
  let box = GWindow.dialog
    ~parent
    ~title:"Confirm"
    () in
  let markup = Printf.sprintf "Remove <b>%s</b> from the applications list?" (Gtk_utils.pango_escape name) in
  GMisc.label ~packing:box#vbox#pack ~xpad:20 ~ypad:20 ~markup () |> ignore_widget;
  box#add_button_stock `CANCEL `CANCEL;
  box#add_button_stock `DELETE `DELETE;
  let result, set_result = Lwt.wait () in
  box#set_default_response `DELETE;
  box#connect#response ==> (fun response ->
    box#destroy ();
    Lwt.wakeup set_result (
      match response with
      | `DELETE -> `delete
      | `CANCEL | `DELETE_EVENT -> `cancel
    )
  );
  box#show ();
  result

let run backend dialog gui uri =
  Gtk_utils.async ~parent:dialog (fun () ->
    Gdk.Window.set_cursor dialog#misc#window (Lazy.force Gtk_utils.busy_cursor);
    try_lwt
      match_lwt get_selections backend ~gui uri with
      | `Aborted_by_user -> Lwt.return ()
      | `Success sels ->
          Gui.spawn backend sels;
          Lwt_unix.sleep 0.5
    finally
      Gdk.Window.set_cursor dialog#misc#window (Lazy.force Gtk_utils.default_cursor);
      Lwt.return ()
  )

let create backend ~gui ~add_app =
  let finished, set_finished = Lwt.wait () in

  let dialog = GWindow.dialog ~title:"0install Applications" () in

  let swin = GBin.scrolled_window
    ~packing:(dialog#vbox#pack ~expand:true)
    ~hpolicy:`NEVER
    ~vpolicy:`AUTOMATIC
    () in

  (* Model *)
  let cols = new GTree.column_list in
  let uri_col = cols#add Gobject.Data.string in
  let name_col = cols#add Gobject.Data.string in
  let icon_col = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let path_col = cols#add Gobject.Data.string in

  let model = GTree.list_store cols in

  (* View *)
  let view = GTree.icon_view
    ~model
    ~packing:swin#add
    () in
  view#set_text_column name_col;
  view#set_pixbuf_column icon_col;

  (* Buttons *)
  dialog#add_button "Show Cache" `SHOW_CACHE;
  let actions = dialog#action_area in
  let cache_button = List.hd actions#children in
  cache_button#misc#set_tooltip_text "Show all 0install software currently stored on this computer \
    (i.e. those programs which can be run without a network connection). \
    This can be useful if you're running out of disk space and need to delete something.";
  dialog#action_area#set_child_secondary cache_button true;

  dialog#add_button_stock `ADD `ADD;
  let add_button = List.hd actions#children in
  add_button#misc#set_tooltip_text "Add a new application. You can also just drag a 0install feed URL from \
    your web-browser to this window.";

  dialog#add_button_stock `CLOSE `CLOSE;

  (* Menu *)
  let menu = GMenu.menu () in

  let menu_iface = ref None in
  let run_item = GMenu.menu_item ~packing:menu#add ~label:"Run" () in
  let help_item = GMenu.menu_item ~packing:menu#add ~label:"Show help" () in
  let edit_item = GMenu.menu_item ~packing:menu#add ~label:"Choose versions" () in
  let delete_item = GMenu.menu_item ~packing:menu#add ~label:"Delete" () in

  run_item#connect#activate ==> (fun () ->
    run backend dialog gui (!menu_iface |? lazy (raise_safe "BUG: no selected item!"))
  );

  help_item#connect#activate ==> (fun () ->
    let uri = !menu_iface |? lazy (raise_safe "BUG: no selected item!") in
    Gtk_utils.async ~parent:dialog (fun () -> show_help_for_iface backend ~gui uri)
  );

  edit_item#connect#activate ==> (fun () ->
    let uri = !menu_iface |? lazy (raise_safe "BUG: no selected item!") in
    let reqs = Requirements.default_requirements uri in
    Gtk_utils.async ~parent:dialog (fun () ->
      lwt _ = gui#run_solver `Download_only reqs ~refresh:false in
      Lwt.return ()
    )
  );

  delete_item#connect#activate ==> (fun () ->
    match view#get_selected_items with
    | [path] ->
        let row = model#get_iter path in
        let name = model#get ~row ~column:name_col in
        let path = model#get ~row ~column:path_col in
        dialog#misc#set_sensitive false;
        Gtk_utils.async ~parent:dialog (fun () ->
          try_lwt
            match_lwt confirm_deletion ~parent:dialog name with
            | `delete ->
                log_info "rm %s" path;
                begin
                  try (Gui.system backend)#unlink path
                  with Unix.Unix_error (Unix.EACCES, _, _) ->
                    raise_safe "Permission denied. You may be able to remove the entry manually with:\n\
                                sudo rm '%s'" path
                end;
                model#remove row |> ignore;
                Lwt.return ()
            | `cancel -> Lwt.return ()
          finally
            dialog#misc#set_sensitive true;
            Lwt.return ()
        )
    | _ -> log_warning "Invalid selection!"
  );

  view#event#connect#button_press ==> (fun bev ->
    let module B = GdkEvent.Button in
    let path_unsafe = view#get_path_at_pos (B.x bev |> truncate) (B.y bev |> truncate) in
    (* (a bug in lablgtk means the "option" part is missing) *)
    let path : Gtk.tree_path option = Obj.magic path_unsafe in
    match GdkEvent.get_type bev, B.button bev, path with
    | `TWO_BUTTON_PRESS, 1, Some path ->
        let row = model#get_iter path in
        run backend dialog gui (model#get ~row ~column:uri_col);
        true
    | `BUTTON_PRESS, 3, Some path ->
        view#select_path path;
        let row = model#get_iter path in
        menu_iface := Some (model#get ~row ~column:uri_col);
        menu#popup ~button:(B.button bev) ~time:(B.time bev);
        true
    | _ ->
        false
  );

  let default_icon = view#misc#render_icon ~size:`DIALOG `EXECUTE in

  (* We're missing gtk_icon_size_lookup, but can get it this way instead... *)
  let width = GdkPixbuf.get_width default_icon in
  let height = GdkPixbuf.get_height default_icon in

  (* Populate model *)
  let populate () =
    model#clear ();
    Gui.discover_existing_apps backend
    |> List.sort by_name_ignore_case
    |> List.iter (fun (name, path, uri) ->
      let row = model#append () in
      model#set ~row ~column:name_col name;
      model#set ~row ~column:uri_col uri;
      model#set ~row ~column:path_col path;
      let url = Feed_url.master_feed_of_iface uri in

      let config = Gui.config backend in
      FC.get_cached_icon_path config url
      |> pipe_some (Gtk_utils.load_png_icon config.system ~width ~height)
      |> default default_icon
      |> model#set ~row ~column:icon_col;
    ) in
  populate ();

  let add_and_repopulate uri =
    Gtk_utils.async ~parent:dialog (fun () ->
      lwt () = add_app uri in
      populate ();
      Lwt.return ()
    ) in

  (* Drag-and-drop *)

  Gtk_utils.make_iface_uri_drop_target dialog (fun iface ->
    log_info "URI dropped: %s" iface;
    Gtk_utils.sanity_check_iface iface;
    add_and_repopulate iface;
    true
  );

  dialog#connect#response ==> (function
    | `DELETE_EVENT | `CLOSE -> dialog#destroy (); Lwt.wakeup set_finished ()
    | `SHOW_CACHE -> Gtk_utils.async (fun () -> Cache_explorer_box.open_cache_explorer backend)
    | `ADD -> add_and_repopulate ""
  );

  dialog#set_default_size
    ~width:(Gdk.Screen.width () / 3)
    ~height:(Gdk.Screen.height () / 3);

  dialog#show ();

  finished
