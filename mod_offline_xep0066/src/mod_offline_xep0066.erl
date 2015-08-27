-module(mod_offline_xep0066).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([start/2, stop/1, log_user_send/4]).

start(Host, _Opts) ->
  %application:start(ssl), % todo: for https
  case inets:start() of 
    {error, {already_started, _}} -> ok;
    ok -> ok
  end,
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_user_send, 1),
  ok.

stop(Host) ->
  ?INFO_MSG("Stopping xep0066", []),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, log_user_send, 1),
  ok.

log_user_send(C2SState, From, To, Packet) ->
  ?INFO_MSG(C2SState, []),
  ?INFO_MSG(From, []),
  ?INFO_MSG(To, []),
  ?INFO_MSG(Packet, []),
  ok.                             
