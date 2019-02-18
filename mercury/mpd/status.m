%----------------------------------------------------------------------------%
% FILENAME : status.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Sun Feb 17, 2019  05:22AM
% MODIFIED : Sun Feb 17, 2019  12:59PM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module status.

:- interface.

:- import_module io.
:- import_module bool.
:- import_module uint.
:- import_module float.

%----------------------------------------------------------------------------%

:- type mpd_state
  ---> mpd_state_unknown
  ;    mpd_state_stop
  ;    mpd_state_play
  ;    mpd_state_pause.

:- type mpd_connection.
:- type mpd_pair.
:- type mpd_audio_format.
:- type mpd_status.

:- pred
  mpd_status_begin( mpd_status::uo
                  , io::di
                  , io::uo
                  ) is det.

:- pred
  mpd_status_feed( mpd_status::di
                 , mpd_status::uo
                 , mpd_pair::di
                 , io::di
                 , io::uo
                 ) is det.

:- pred
  mpd_send_status( bool::out
                 , {mpd_connection, io}::di
                 , {mpd_connection, io}::uo
                 ) is det.

:- pred
  mpd_recv_status( mpd_status::uo
                 , {mpd_connection, io}::di
                 , {mpd_connection, io}::uo
                 ) is det.

:- pred
  mpd_run_status( mpd_status::uo
                , {mpd_connection, io}::di
                , {mpd_connection, io}::uo
                ) is det.

:- pred
  mpd_status_free( mpd_status::di
                 , io::di
                 , io::uo
                 ) is det.

:- pred
  mpd_status_get_volume( int::out
                       , {mpd_status, io}::di
                       , {mpd_status, io}::uo
                       ) is det.

:- pred
  mpd_status_get_volume( int::out
                       , mpd_status::di
                       , mpd_status::uo
                       , io::di
                       , io::uo
                       ) is det.

:- pred
  mpd_status_get_repeat( bool::out
                       , {mpd_status, io}::di
                       , {mpd_status, io}::uo
                       ) is det.

:- pred
  mpd_status_get_repeat( bool::out
                       , mpd_status::di
                       , mpd_status::uo
                       , io::di
                       , io::uo
                       ) is det.

:- pred
  mpd_status_get_random( bool::out
                       , {mpd_status, io}::di
                       , {mpd_status, io}::uo
                       ) is det.

:- pred
  mpd_status_get_random( bool::out
                       , mpd_status::di
                       , mpd_status::uo
                       , io::di
                       , io::uo
                       ) is det.

:- pred
  mpd_status_get_single( bool::out
                       , {mpd_status, io}::di
                       , {mpd_status, io}::uo
                       ) is det.

:- pred
  mpd_status_get_single( bool::out
                       , mpd_status::di
                       , mpd_status::uo
                       , io::di
                       , io::uo
                       ) is det.

:- pred
  mpd_status_get_consume( bool::out
                        , {mpd_status, io}::di
                        , {mpd_status, io}::uo
                        ) is det.

:- pred
  mpd_status_get_consume( bool::out
                        , mpd_status::di
                        , mpd_status::uo
                        , io::di
                        , io::uo
                        ) is det.

:- pred
  mpd_status_get_queue_length( uint::out
                             , {mpd_status, io}::di
                             , {mpd_status, io}::uo
                             ) is det.

:- pred
  mpd_status_get_queue_length( uint::out
                             , mpd_status::di
                             , mpd_status::uo
                             , io::di
                             , io::uo
                             ) is det.

:- pred
  mpd_status_get_queue_version( uint::out
                              , {mpd_status, io}::di
                              , {mpd_status, io}::uo
                              ) is det.

:- pred
  mpd_status_get_queue_version( uint::out
                              , mpd_status::di
                              , mpd_status::uo
                              , io::di
                              , io::uo
                              ) is det.

:- pred
  mpd_status_get_state( mpd_state::out
                      , {mpd_status, io}::di
                      , {mpd_status, io}::uo
                      ) is det.

:- pred
  mpd_status_get_state( mpd_state::out
                      , mpd_status::di
                      , mpd_status::uo
                      , io::di
                      , io::uo
                      ) is det.

:- pred
  mpd_status_get_crossfade( uint::out
                          , {mpd_status, io}::di
                          , {mpd_status, io}::uo
                          ) is det.

:- pred
  mpd_status_get_crossfade( uint::out
                          , mpd_status::di
                          , mpd_status::uo
                          , io::di
                          , io::uo
                          ) is det.

:- pred
  mpd_status_get_mixrampdb( float::out
                          , {mpd_status, io}::di
                          , {mpd_status, io}::uo
                          ) is det.

:- pred
  mpd_status_get_mixrampdb( float::out
                          , mpd_status::di
                          , mpd_status::uo
                          , io::di
                          , io::uo
                          ) is det.

:- pred
  mpd_status_get_song_pos( int::out
                         , {mpd_status, io}::di
                         , {mpd_status, io}::uo
                         ) is det.

:- pred
  mpd_status_get_song_pos( int::out
                         , mpd_status::di
                         , mpd_status::uo
                         , io::di
                         , io::uo
                         ) is det.

:- pred
  mpd_status_get_song_id( int::out
                        , {mpd_status, io}::di
                        , {mpd_status, io}::uo
                        ) is det.

:- pred
  mpd_status_get_song_id( int::out
                        , mpd_status::di
                        , mpd_status::uo
                        , io::di
                        , io::uo
                        ) is det.

:- pred
  mpd_status_get_next_song_pos( int::out
                              , {mpd_status, io}::di
                              , {mpd_status, io}::uo
                              ) is det.

:- pred
  mpd_status_get_next_song_pos( int::out
                              , mpd_status::di
                              , mpd_status::uo
                              , io::di
                              , io::uo
                              ) is det.

:- pred
  mpd_status_get_next_song_id( int::out
                             , {mpd_status, io}::di
                             , {mpd_status, io}::uo
                             ) is det.

:- pred
  mpd_status_get_next_song_id( int::out
                             , mpd_status::di
                             , mpd_status::uo
                             , io::di
                             , io::uo
                             ) is det.

:- pred
  mpd_status_get_elapsed_time( uint::out
                             , {mpd_status, io}::di
                             , {mpd_status, io}::uo
                             ) is det.

:- pred
  mpd_status_get_elapsed_time( uint::out
                             , mpd_status::di
                             , mpd_status::uo
                             , io::di
                             , io::uo
                             ) is det.

:- pred
  mpd_status_get_elapsed_ms( uint::out
                           , {mpd_status, io}::di
                           , {mpd_status, io}::uo
                           ) is det.

:- pred
  mpd_status_get_elapsed_ms( uint::out
                           , mpd_status::di
                           , mpd_status::uo
                           , io::di
                           , io::uo
                           ) is det.

:- pred
  mpd_status_get_total_time( uint::out
                           , {mpd_status, io}::di
                           , {mpd_status, io}::uo
                           ) is det.

:- pred
  mpd_status_get_total_time( uint::out
                           , mpd_status::di
                           , mpd_status::uo
                           , io::di
                           , io::uo
                           ) is det.

:- pred
  mpd_status_get_kbit_rate( uint::out
                          , {mpd_status, io}::di
                          , {mpd_status, io}::uo
                          ) is det.

:- pred
  mpd_status_get_kbit_rate( uint::out
                          , mpd_status::di
                          , mpd_status::uo
                          , io::di
                          , io::uo
                          ) is det.

:- pred
  mpd_status_get_audio_format( mpd_audio_format::uo
                             , {mpd_status, io}::di
                             , {mpd_status, io}::uo
                             ) is det.

:- pred
  mpd_status_get_audio_format( mpd_audio_format::uo
                             , mpd_status::di
                             , mpd_status::uo
                             , io::di
                             , io::uo
                             ) is det.

:- pred
  mpd_status_get_update_id( mpd_audio_format::uo
                          , {mpd_status, io}::di
                          , {mpd_status, io}::uo
                          ) is det.

:- pred
  mpd_status_get_update_id( mpd_audio_format::uo
                          , mpd_status::di
                          , mpd_status::uo
                          , io::di
                          , io::uo
                          ) is det.

:- pred
  mpd_status_get_error( string::out
                      , {mpd_status, io}::di
                      , {mpd_status, io}::uo
                      ) is det.

:- pred
  mpd_status_get_error( string::out
                      , mpd_status::di
                      , mpd_status::uo
                      , io::di
                      , io::uo
                      ) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <mpd/status.h>").

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

:- pragma foreign_enum("C", mpd_state/0,
[
  mpd_state_unknown - "MPD_STATE_UNKNOWN",
  mpd_state_stop    - "MPD_STATE_STOP",
  mpd_state_play    - "MPD_STATE_PLAY",
  mpd_state_pause   - "MPD_STATE_PAUSE"
]).

:- pragma foreign_type("C"
                      , mpd_connection
                      , "struct mpd_connection*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , mpd_pair
                      , "struct mpd_pair*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , mpd_audio_format
                      , "struct mpd_audio_format*"
                      , [word_aligned_pointer]
                      ).

:- pragma foreign_type("C"
                      , mpd_status
                      , "struct mpd_status*"
                      , [word_aligned_pointer]
                      ).

%--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  %

mpd_status_get_volume(Int, {M, I}, {N, O}) :-
  mpd_status_get_volume(Int, M, N, I, O).

:- pragma foreign_proc("C",
  mpd_status_get_volume(Int::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  int res = mpd_status_get_volume(SI);
  SO = SI; O = I; Int = res;
").

mpd_status_get_repeat(Bool, {M, I}, {N, O}) :-
  mpd_status_get_repeat(Bool, M, N, I, O).

% s/%\([a-z_]*\)( *\([a-zA-Z_]\)t\n.*\n.*\n.*\n.*\n.*/

%:- pragma foreign_proc("C",\r   \1(\2, SI, SO, I, O),\r  \[will_not_call_mercury\],\r"\r  \l\2 res = \1(SI);\r  SO = SI; O = I; \2 = res;\r")./
:- pragma foreign_proc("C",
   mpd_status_get_repeat(Bool::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  bool res = mpd_status_get_repeat(SI);
  SO = SI; O = I; Bool = res;
").

mpd_status_get_random(Bool, {M, I}, {N, O}) :-
  mpd_status_get_random(Bool, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_random(Bool::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  bool res = mpd_status_get_random(SI);
  SO = SI; O = I; Bool = res;
").

mpd_status_get_single(Bool, {M, I}, {N, O}) :-
  mpd_status_get_single(Bool, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_single(Bool::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  bool res = mpd_status_get_single(SI);
  SO = SI; O = I; Bool = res;
").

mpd_status_get_consume(Bool, {M, I}, {N, O}) :-
  mpd_status_get_consume(Bool, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_consume(Bool::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  bool res = mpd_status_get_consume(SI);
  SO = SI; O = I; Bool = res;
").

mpd_status_get_queue_length(Uint, {M, I}, {N, O}) :-
  mpd_status_get_queue_length(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_queue_length(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_queue_length(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_queue_version(Uint, {M, I}, {N, O}) :-
  mpd_status_get_queue_version(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_queue_version(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_queue_version(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_state(Mpd_state, {M, I}, {N, O}) :-
  mpd_status_get_state(Mpd_state, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_state(Mpd_state::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  mpd_state res = mpd_status_get_state(SI);
  SO = SI; O = I; Mpd_state = res;
").

mpd_status_get_crossfade(Uint, {M, I}, {N, O}) :-
  mpd_status_get_crossfade(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_crossfade(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_crossfade(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_mixrampdb(Float, {M, I}, {N, O}) :-
  mpd_status_get_mixrampdb(Float, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_mixrampdb(Float::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  float res = mpd_status_get_mixrampdb(SI);
  SO = SI; O = I; Float = res;
").

mpd_status_get_song_pos(Int, {M, I}, {N, O}) :-
  mpd_status_get_song_pos(Int, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_song_pos(Int::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  int res = mpd_status_get_song_pos(SI);
  SO = SI; O = I; Int = res;
").

mpd_status_get_song_id(Int, {M, I}, {N, O}) :-
  mpd_status_get_song_id(Int, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_song_id(Int::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  int res = mpd_status_get_song_id(SI);
  SO = SI; O = I; Int = res;
").

mpd_status_get_next_song_pos(Int, {M, I}, {N, O}) :-
  mpd_status_get_next_song_pos(Int, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_next_song_pos(Int::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  int res = mpd_status_get_next_song_pos(SI);
  SO = SI; O = I; Int = res;
").

mpd_status_get_next_song_id(Int, {M, I}, {N, O}) :-
  mpd_status_get_next_song_id(Int, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_next_song_id(Int::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  int res = mpd_status_get_next_song_id(SI);
  SO = SI; O = I; Int = res;
").

mpd_status_get_elapsed_time(Uint, {M, I}, {N, O}) :-
  mpd_status_get_elapsed_time(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_elapsed_time(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_elapsed_time(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_elapsed_ms(Uint, {M, I}, {N, O}) :-
  mpd_status_get_elapsed_ms(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_elapsed_ms(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_elapsed_ms(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_total_time(Uint, {M, I}, {N, O}) :-
  mpd_status_get_total_time(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_total_time(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_total_time(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_kbit_rate(Uint, {M, I}, {N, O}) :-
  mpd_status_get_kbit_rate(Uint, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_kbit_rate(Uint::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  uint res = mpd_status_get_kbit_rate(SI);
  SO = SI; O = I; Uint = res;
").

mpd_status_get_audio_format(Mpd_audio_forma, {M, I}, {N, O}) :-
  mpd_status_get_audio_format(Mpd_audio_forma, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_audio_format(Mpd_audio_format::uo, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  mpd_audio_forma res = mpd_status_get_audio_format(SI);
  SO = SI; O = I; Mpd_audio_format = res;
").

:- pragma foreign_proc("C",
   mpd_status_get_update_id(Mpd_audio_forma::uo, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  mpd_audio_forma res = mpd_status_get_update_id(SI);
  SO = SI; O = I; Mpd_audio_forma = res;
").

mpd_status_get_error(String, {M, I}, {N, O}) :-
  mpd_status_get_error(String, M, N, I, O).

:- pragma foreign_proc("C",
   mpd_status_get_error(String::out, SI::di, SO::uo, I::di, O::uo),
  [will_not_call_mercury, promise_pure],
"
  string res = mpd_status_get_error(SI);
  SO = SI; O = I; String = res;
").

%----------------------------------------------------------------------------%
:- end_module status.
%----------------------------------------------------------------------------%
