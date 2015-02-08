(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Manage the GUI sub-process. *)

open Support.Common

type t

type feed_description = {
  times : (string * float) list;
  summary : string option;
  description : string list;
  homepages : string list;
  signatures : [
    | `Valid of Support.Gpg.fingerprint * Support.Gpg.timestamp * string option * [`Trusted | `Not_trusted]
    | `Invalid of string
  ] list;
}

(** The GUI plugin registers itself here. *)
val register_plugin : (t -> Ui.ui_handler option) -> unit

val download_icon : Fetch.fetcher -> Feed_provider.feed_provider -> Feed_url.non_distro_feed -> unit Lwt.t

(** Create an in-memory icon cache that will load from disk or from the network when needed. *)
val make_icon_cache : t ->
  fetcher:Fetch.fetcher ->
  load_png_icon:(filepath -> 'a option) ->
  < get : update:(unit -> unit) ->
          feed_provider:Feed_provider.feed_provider ->
          General.iface_uri -> 'a option;
    set_update_icons : bool -> unit >

(** Should we use the GUI?
 * The input says what the user requested:
 * No -> we never want to use the GUI
 * Yes -> we always want to use the GUI, and throw an exception if it's not available
 * Maybe -> we want to use the GUI iff it's available
 *
 * Returns a suitable GUI handler if so, or None if we should use a non-GUI handler.
 *)
val try_get_gui :
  General.config -> Distro.distribution Lazy.t -> (Progress.watcher -> Fetch.fetcher) ->
  Trust.trust_db Lazy.t -> use_gui:yes_no_maybe -> Ui.ui_handler option

(** Download the feed and add it as an extra feed of the interface. *)
val add_remote_feed : t ->
  #Progress.watcher -> General.iface_uri -> Feed_url.remote_feed -> unit Lwt.t

(** Add a local feed to an interface. *)
val add_feed : t -> General.iface_uri -> Feed_url.local_feed -> unit
val remove_feed : t -> General.iface_uri -> Feed_url.non_distro_feed -> unit
val compile : t -> Feed_provider.feed_provider -> General.iface_uri -> autocompile:bool -> unit Lwt.t

(** Try to guess whether we have source for this interface.
 * Returns true if we have any source-only feeds, or any source implementations
 * in our regular feeds. However, we don't look inside the source feeds (so a
 * source feed containing no implementations will still count as true).
 * This is used in the GUI to decide whether to shade the Compile button.
 *)
val have_source_for : Feed_provider.feed_provider -> General.iface_uri -> bool

(** List the implementations of this interface in the order they should be shown in the GUI.
 * @return (selected_version, implementations and user-set stability). *)
val list_impls : t -> Solver.Model.t -> Solver.role -> (Impl.generic_implementation option *
  (Impl.generic_implementation * Impl_provider.rejection_reason option * General.stability_level option) list
)

(* Returns (fetch-size, fetch-tooltip) *)
val get_fetch_info : t -> Impl.generic_implementation -> (string * string)

(** Set a user-override stability rating. *)
val set_impl_stability : t -> Feed_url.global_id -> General.stability_level option -> unit

(** Get the initial text for the bug report dialog box. *)
val get_bug_report_details : t -> role:Solver.role -> (bool * Solver.Model.t) -> string

(** Submit a bug report for this interface.
 * @return the response from the server (on success).
 * @raise Safe_exception on failure. *)
val send_bug_report : General.iface_uri -> string -> string Lwt.t

val run_test : t -> (Selections.t -> string Lwt.t) -> (bool * Solver.Model.t) -> string Lwt.t

(* (ideally, get rid of this at some point) *)
val system : t -> system
val config : t -> General.config
val trust_db : t -> Trust.trust_db
val distro : t -> Distro.distribution

(** The summary for the current language. *)
val get_summary : t -> Feed.feed -> string option

(** The text of the description element for the current language, formatted as a list of paragraphs.
 * If there is no description, the returned list will be empty. *)
val get_description : t -> Feed.feed -> string list

val generate_feed_description : t -> Feed.feed -> Feed.feed_overrides -> feed_description Lwt.t

val get_user_stability : t -> _ Impl.t -> General.stability_level option
val get_stability_policy : t -> General.iface_uri -> General.stability_level option
val set_stability_policy : t -> General.iface_uri -> General.stability_level option -> unit
val lookup_path : t -> Impl.generic_implementation -> filepath option
val explain_decision :
  t -> feed_provider:Feed_provider.feed_provider ->
  Requirements.t -> Solver.role -> _ Impl.t -> string      

val download_selections : t ->
  Fetch.fetcher Lazy.t ->
  feed_provider:Feed_provider.feed_provider ->
  Solver.Model.t -> [ `aborted_by_user | `success ] Lwt.t

val quick_solve : t -> Requirements.t -> Selections.t option

val solve_with_downloads : t ->
  Fetch.fetcher ->
  watcher:#Progress.watcher ->
  Requirements.t ->
  force:bool -> (bool * Solver.Model.t * Feed_provider.feed_provider) Lwt.t

val make_fetcher : t -> Progress.watcher -> Fetch.fetcher

(** This is used if the GUI gets asked to do a type of UI it doesn't support (e.g. search).
 * (todo: would be good to find a better way to do this) *)
val batch_ui : t -> Ui.ui_handler

(** Write a .desktop file for this application.
 * The master feed for the interface must be cached before calling this. *)
val xdg_add_to_menu : t -> General.iface_uri -> unit

(** Search through the configured XDG datadirs looking for .desktop files created by us. *)
val discover_existing_apps : t -> (string * filepath * General.iface_uri) list

(** Use [xdg-open] to show the help files for this implementation.
 * The selection must already be cached.
 * @raise Safe_exception if there is any problem. *)
val show_help_exn : t -> Selections.selection -> unit

(** Execute the selections in a sub-process and return without waiting.
 * The selections must already be cached. *)
val spawn : t -> Selections.t -> unit

(** An event that fires whenever a new implementation is added to the cache. *)
val impl_added_to_store_event : t -> unit React.E.t
