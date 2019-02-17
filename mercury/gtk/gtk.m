%----------------------------------------------------------------------------%
% FILENAME : gtk.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Sat Feb 16, 2019  12:07PM
% MODIFIED : Sun Feb 17, 2019  03:14AM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module gtk.

:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%

% TDB: declare predicates and functions

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module term_to_xml.

:- pragma foreign_decl("C", "#include <gtk/gtk.h>").
:- pragma foreign_decl("C", "#include <epoxy/gl.h>").

%--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- %
% GTK BINDINGS

:- type gtk_window_toplevel ---> gtk_window_toplevel.

:- type gtk_widget.
:- type pango_language.
:- type gtk_text_direction.
:- type gpointer.
:- type gdk_event_key.
:- type snoop_id.
:- type gtk_key_snoop_pred
  ---> snoop_pred(pred(gtk_widget, gdk_event_key, gpointer, io, io)).

:- impure pred gtk_init(io::di, io::uo) is det.
:- impure pred gtk_init_check(io::di, io::uo) is det.
:- impure pred
  gtk_window_new( gtk_window_toplevel::in
                , gtk_widget::out
                , io::di
                , io::uo
                ) is det.
:- impure pred gtk_widget_show_all(gtk_widget::in, io::di, io::uo) is det.
:- impure pred gtk_main(io::di, io::uo) is erroneous. % partial loop
:- impure pred gtk_disable_setlocale(io::di, io::uo) is det.
:- impure pred
  gtk_get_default_language(pango_language::out, io::di, io::uo) is det.
:- impure pred
  gtk_get_locale_direction(gtk_text_direction::out, io::di, io::uo) is det.
:- impure pred gtk_events_pending(bool::out, io::di, io::uo) is det.
:- impure pred
  gtk_key_snooper_install( gtk_key_snoop_pred::in
                         , snoop_id::out
                         , io::di
                         , io::uo
                         ) is det.
:- impure pred
  gtk_key_snooper_remove( snoop_id::in
                        , io::di
                        , io::uo
                        ) is det.

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

:- pragma foreign_enum("C", gtk_window_toplevel/0,
[
  gtk_window_toplevel - "GTK_WINDOW_TOPLEVEL"
]).

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

:- pragma foreign_type("C", gtk_widget, "GtkWidget*", [word_aligned_pointer]).

:- pragma foreign_type("C"
                      , pango_language
                      , "PangoLanguage*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gtk_text_direction
                      , "GtkTextDirection*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gpointer
                      , "gpointer"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gdk_event_key
                      , "GdkEventKey*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gtk_key_snoop_pred
                      , "GtkKeySnoopFunc"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , snoop_id
                      , "guint"
                      , [can_pass_as_mercury_type]
                      ).

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

:- pragma foreign_proc("C",
  gtk_init(I::di, O::uo),
  [will_not_call_mercury],
"
  gtk_init(NULL, NULL); O = I;
").

:- pragma foreign_proc("C",
  gtk_init_check(I::di, O::uo),
  [will_not_call_mercury],
"
  gtk_init_check(NULL, NULL); O = I;
").

:- pragma foreign_proc("C",
  gtk_window_new(WFlag::in, Win::out, I::di, O::uo),
  [will_not_call_mercury],
"
  GtkWidget *window = gtk_window_new(WFlag); O = I;
  Win = window;
").

:- pragma foreign_proc("C",
  gtk_widget_show_all(Win::in, I::di, O::uo),
  [will_not_call_mercury],
"
  gtk_widget_show_all(Win); O = I;
").

:- pragma foreign_proc("C",
  gtk_main(I::di, O::uo),
  [will_not_call_mercury],
"
  gtk_main(); O = I;
").

:- pragma foreign_proc("C",
  gtk_disable_setlocale(I::di, O::uo),
  [will_not_call_mercury],
"
  gtk_disable_setlocale(); O = I;
").

:- pragma foreign_proc("C",
  gtk_get_default_language(PL::out, I::di, O::uo),
  [will_not_call_mercury],
"
  PangoLanguage * pan = gtk_get_default_language();
  PL = pan; O = I;
").

:- pragma foreign_proc("C",
  gtk_get_locale_direction(Dir::out, I::di, O::uo),
  [will_not_call_mercury],
"
  GtkTextDirection *dir = gtk_get_locale_direction();
  Dir = dir; O = I;
").

:- pragma foreign_proc("C",
  gtk_events_pending(B::out, I::di, O::uo),
  [will_not_call_mercury],
"
  gint res = gtk_events_pending();
  B = res; O = I;
").

:- pragma foreign_proc("C",
  gtk_key_snooper_install(SNF::in, R::out, I::di, O::uo),
  [may_call_mercury],
"
  guint res = gtk_key_snooper_install(SNF, NULL);
  R = res; O = I;
").

:- pragma foreign_proc("C",
  gtk_key_snooper_remove(R::in, I::di, O::uo),
  [may_call_mercury],
"
  gtk_key_snooper_remove(R);
  O = I;
").

%--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- %
% GTK BUILDER BINDINGS

:- type gtk_builder.
:- type gtk_builder_connect_pred.
:- type gobject.
:- type gtk_gconnected_flags.

:- type gtk_builder_error
  ---> gtk_builder_error_invalid_type_function
  ;    gtk_builder_error_unhandled_tag
  ;    gtk_builder_error_missing_attribute
  ;    gtk_builder_error_invalid_tag
  ;    gtk_builder_error_missing_property_value
  ;    gtk_builder_error_invalid_value.

:- impure pred
  gtk_builder_new(gtk_builder::uo, bool::out, io::di, io::uo) is det.

:- impure pred
  gtk_builder_add_from_file( gtk_builder::di
                           , gtk_builder::uo
                           , string::in
                           , io::di
                           , io::uo) is det.

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

:- pragma foreign_type("C"
                      , gtk_builder
                      , "GtkBuilder*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gtk_builder_connect_pred
                      , "GtkBuilderConnectFunc"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gobject
                      , "gobject"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , gtk_gconnected_flags
                      , "GtkGconnectedFlags"
                      , [can_pass_as_mercury_type]
                      ).

:- pragma foreign_enum("C", gtk_builder_error/0,
[
  gtk_builder_error_invalid_type_function  - "GTK_BUILDER_ERROR_INVALID_TYPE_FUNCTION",
  gtk_builder_error_unhandled_tag          - "GTK_BUILDER_ERROR_UNHANDLED_TAG",
  gtk_builder_error_missing_attribute      - "GTK_BUILDER_ERROR_MISSING_ATTRIBUTE",
  gtk_builder_error_invalid_tag            - "GTK_BUILDER_ERROR_INVALID_TAG",
  gtk_builder_error_missing_property_value - "GTK_BUILDER_ERROR_MISSING_PROPERTY_VALUE",
  gtk_builder_error_invalid_value          - "GTK_BUILDER_ERROR_INVALID_VALUE"
]).

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

%--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- %
% EDITOR

%--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- %
% MAIN

:- pragma no_determinism_warning(main/2).
main -->
  impure gtk_init,
  impure gtk_window_new(gtk_window_toplevel, Win),
  impure gtk_widget_show_all(Win),
  impure gtk_main.

%----------------------------------------------------------------------------%
:- end_module gtk.
%----------------------------------------------------------------------------%
