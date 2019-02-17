:- module control.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module maybe.

main --> write_string("hi").

% Use of subtypes here mainly enables slighly more generic handling of
% devices while still treating it as a closed set and avoiding typeclasses.

% Devices
:- type devices --->
  impact;
  motion;
  tamper; % door bolt depth
  flood;
  smoke;
  ac;
  heating;
  door;
  light;
  assistance;
  gate.

:- inst sensor   == bound(impact; motion; tamper; flood; smoke; assistance; door; light; gate).
:- mode sensor == sensor >> sensor.

:- inst actuator == bound(ac; heating; door; light; gate).
:- mode actuator == actuator >> actuator.

% Messages
:- type message(Ident, Payload) --->
  command(Ident, Payload);
  ack(Ident).

:- inst response(Ident) == bound(ack(Ident)).
