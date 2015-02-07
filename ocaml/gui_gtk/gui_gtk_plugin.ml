(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** A GTK GUI plugin *)

open Support.Common
open Zeroinstall

let make_gtk_ui backend =
  let batch_ui = lazy (Gui.batch_ui backend) in

  object (self : Ui.ui_handler)
    val mutable preferences_dialog = None
    val mutable solver_boxes : Solver_box.solver_box list = []

    method private recalculate () =
      solver_boxes |> List.iter (fun box -> box#recalculate)

    method private show_preferences_internal =
      let config = Gui.config backend in
      let trust_db = Gui.trust_db backend in
      match preferences_dialog with
      | Some (dialog, result) -> dialog#present (); result
      | None ->
          let dialog, result = Preferences_box.show_preferences config trust_db ~recalculate:self#recalculate in
          preferences_dialog <- Some (dialog, result);
          dialog#show ();
          Gtk_utils.async (fun () -> result >> (preferences_dialog <- None; Lwt.return ()));
          result

    method show_preferences = Some (self#show_preferences_internal)

    method run_solver ?test_callback ?systray mode reqs ~refresh =
      let solver_promise, set_solver = Lwt.wait () in
      let watcher = Gui_progress.make_watcher solver_promise backend reqs in
      let show_preferences () = self#show_preferences_internal in
      let box = Solver_box.run_solver ~show_preferences backend ?test_callback ?systray mode reqs ~refresh watcher in
      Lwt.wakeup set_solver box;
      solver_boxes <- box :: solver_boxes;
      try_lwt
        box#result
      finally
        solver_boxes <- solver_boxes |> List.filter ((<>) box);
        Lwt.return ()

    method open_app_list_box =
      App_list_box.create backend ~gui:self ~add_app:self#open_add_box

    method open_add_box url = Add_box.create ~gui:self backend url 

    method open_cache_explorer = Cache_explorer_box.open_cache_explorer backend

    method watcher =
      log_info "GUI download not in the context of any window";
      (Lazy.force batch_ui)#watcher
  end

(* If this raises an exception, gui.ml will log it and continue without the GUI. *)
let try_get_gtk_gui backend =
  log_info "Switching to GLib mainloop...";

  (* Install Lwt<->Glib integration.
   * LWT <= 2.4.4 is buggy (https://github.com/ocsigen/lwt/issues/25) so we have
   * to be careful... *)
  if (Gui.system backend)#platform.Platform.os = "Linux" then (
     (* On Linux:
      * - lwt_into_glib mode hangs for LWT <= 2.4.4
      * - glib_into_lwt works on all versions, so use that *)
    Lwt_glib.install ~mode:`glib_into_lwt ()
  ) else (
    (* Otherwise, glib_into_lwt never works, so use lwt_into_glib (and require LWT > 2.4.4). *)
    Lwt_glib.install ~mode:`lwt_into_glib ()
  );

  (* Initializes GTK. *)
  ignore (GMain.init ());
  Some (make_gtk_ui backend)

let () =
  log_info "Initialising GTK GUI";
  Gui.register_plugin try_get_gtk_gui
