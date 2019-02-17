%----------------------------------------------------------------------------%
% FILENAME : status.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Sun Feb 17, 2019  05:22AM
% MODIFIED : Sun Feb 17, 2019  11:28AM
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

%mpd_status_get_volume( Intt
%                     , Mpd_status
%                     , Mpd_status
%                     , Io
%                     , Io
%                     ) :-

mpd_status_get_repeat(Bool, {M, I}, {N, O}) :-
  mpd_status_get_repeat(Bool, M, N, I, O).

%mpd_status_get_repeat( Boolt
%                     , Mpd_status
%                     , Mpd_status
%                     , Io
%                     , Io
%                     ) :-

mpd_status_get_random(Bool, {M, I}, {N, O}) :-
  mpd_status_get_random(Bool, M, N, I, O).

%mpd_status_get_random( Boolt
%                     , Mpd_status
%                     , Mpd_status
%                     , Io
%                     , Io
%                     ) :-

mpd_status_get_single(Bool, {M, I}, {N, O}) :-
  mpd_status_get_single(Bool, M, N, I, O).

%mpd_status_get_single( Boolt
%                     , Mpd_status
%                     , Mpd_status
%                     , Io
%                     , Io
%                     ) :-

mpd_status_get_consume(Bool, {M, I}, {N, O}) :-
  mpd_status_get_consume(Bool, M, N, I, O).

%mpd_status_get_consume( Boolt
%                      , Mpd_status
%                      , Mpd_status
%                      , Io
%                      , Io
%                      ) :-

mpd_status_get_queue_length(Uint, {M, I}, {N, O}) :-
  mpd_status_get_queue_length(Uint, M, N, I, O).

%mpd_status_get_queue_length( Uintt
%                           , Mpd_status
%                           , Mpd_status
%                           , Io
%                           , Io
%                           ) :-

mpd_status_get_queue_version(Uint, {M, I}, {N, O}) :-
  mpd_status_get_queue_version(Uint, M, N, I, O).

%mpd_status_get_queue_version( Uintt
%                            , Mpd_status
%                            , Mpd_status
%                            , Io
%                            , Io
%                            ) :-
%

mpd_status_get_state(Mpd_state, {M, I}, {N, O}) :-
  mpd_status_get_state(Mpd_state, M, N, I, O).

%mpd_status_get_state( Mpd_statet
%                    , Mpd_status
%                    , Mpd_status
%                    , Io
%                    , Io
%                    ) :-
%

mpd_status_get_crossfade(Uint, {M, I}, {N, O}) :-
  mpd_status_get_crossfade(Uint, M, N, I, O).
%
%mpd_status_get_crossfade( Uintt
%                        , Mpd_status
%                        , Mpd_status
%                        , Io
%                        , Io
%                        ) :-
%
mpd_status_get_mixrampdb(Float, {M, I}, {N, O}) :-
  mpd_status_get_mixrampdb(Float, M, N, I, O).

%mpd_status_get_mixrampdb( Floatt
%                        , Mpd_status
%                        , Mpd_status
%                        , Io
%                        , Io
%                        ) :-
%

mpd_status_get_song_pos(Int, {M, I}, {N, O}) :-
  mpd_status_get_song_pos(Int, M, N, I, O).

%mpd_status_get_song_pos( Intt
%                       , Mpd_status
%                       , Mpd_status
%                       , Io
%                       , Io
%                       ) :-
%

mpd_status_get_song_id(Int, {M, I}, {N, O}) :-
  mpd_status_get_song_id(Int, M, N, I, O).

%mpd_status_get_song_id( Intt
%                      , Mpd_status
%                      , Mpd_status
%                      , Io
%                      , Io
%                      ) :-
%

mpd_status_get_next_song_pos(Int, {M, I}, {N, O}) :-
  mpd_status_get_next_song_pos(Int, M, N, I, O).

%mpd_status_get_next_song_pos( Intt
%                            , Mpd_status
%                            , Mpd_status
%                            , Io
%                            , Io
%                            ) :-
%

mpd_status_get_next_song_id(Int, {M, I}, {N, O}) :-
  mpd_status_get_next_song_id(Int, M, N, I, O).

%mpd_status_get_next_song_id( Intt
%                           , Mpd_status
%                           , Mpd_status
%                           , Io
%                           , Io
%                           ) :-
%

mpd_status_get_elapsed_time(Uint, {M, I}, {N, O}) :-
  mpd_status_get_elapsed_time(Uint, M, N, I, O).

%mpd_status_get_elapsed_time( Uintt
%                           , Mpd_status
%                           , Mpd_status
%                           , Io
%                           , Io
%                           ) :-
%

mpd_status_get_elapsed_ms(Uint, {M, I}, {N, O}) :-
  mpd_status_get_elapsed_ms(Uint, M, N, I, O).

%mpd_status_get_elapsed_ms( Uintt
%                         , Mpd_status
%                         , Mpd_status
%                         , Io
%                         , Io
%                         ) :-
%
mpd_status_get_total_time(Uint, {M, I}, {N, O}) :-
  mpd_status_get_total_time(Uint, M, N, I, O).

%mpd_status_get_total_time( Uintt
%                         , Mpd_status
%                         , Mpd_status
%                         , Io
%                         , Io
%                         ) :-
%
mpd_status_get_kbit_rate(Uint, {M, I}, {N, O}) :-
  mpd_status_get_kbit_rate(Uint, M, N, I, O).

%mpd_status_get_kbit_rate( Uintt
%                        , Mpd_status
%                        , Mpd_status
%                        , Io
%                        , Io
%                        ) :-
%
mpd_status_get_audio_format(Mpd_audio_forma, {M, I}, {N, O}) :-
  mpd_status_get_audio_format(Mpd_audio_forma, M, N, I, O).

%mpd_status_get_audio_format( Mpd_audio_format
%                           , Mpd_status
%                           , Mpd_status
%                           , Io
%                           , Io
%                           ) :-
%

%mpd_status_get_update_id( Mpd_audio_format
%                        , Mpd_status
%                        , Mpd_status
%                        , Io
%                        , Io
%                        ) :-

mpd_status_get_error(String, {M, I}, {N, O}) :-
  mpd_status_get_error(String, M, N, I, O).

%mpd_status_get_error( Stringt
%                    , Mpd_status
%                    , Mpd_status
%                    , Io
%                    , Io
%                    ) :-

%----------------------------------------------------------------------------%
:- end_module status.
%----------------------------------------------------------------------------%
