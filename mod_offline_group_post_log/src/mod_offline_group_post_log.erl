-module(mod_offline_group_post_log).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-include("mod_muc_room.hrl").

-export([start/2, stop/1, foo/3, muc_filter_message/5]).

start(Host, _Opts) ->
  ?INFO_MSG("mod_offline_group_post_log started", []),
  case inets:start() of
    {error, {already_started, _}} -> ok;
    ok -> ok
  end,
  %% ejabberd_hooks:add(filter_packet, Host, ?MODULE, foo, 1),
  ejabberd_hooks:add(muc_filter_message, Host, ?MODULE,
    muc_filter_message, 1),
  ok.

stop(Host) ->
  %% ejabberd_hooks:delete(filter_packet, Host, ?MODULE, foo, 1),
  ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, muc_filter_message, 1),
  ok.

foo(a,b,c) ->
  ?INFO_MSG(a, []),
  ?INFO_MSG(b, []),
  ?INFO_MSG(c, []),
  ok.

muc_filter_message(Pkt, MUCState, RoomJID, From, FromNick) ->
  %%?INFO_MSG(Pkt, []),
  %%Items = ejabberd_hooks:run_fold(roster_get_subscription_list, "localhost", {[], []}, []),
  %%?INFO_MSG(State, []),

 %% ?INFO_MSG(RoomJID, []),
 %% ?INFO_MSG(From, []),
 %% ?INFO_MSG(FromNick, []),

  _LISTUSERS = lists:map(
    fun({_LJID, Info}) ->
      binary_to_list(Info#user.jid#jid.luser) ++ ".."
    end,
    dict:to_list(MUCState#state.users)
  ),
  ?INFO_MSG(" #########    GROUPCHAT _LISTUSERS = ~p~n  #######   ", [_LISTUSERS]),

  _AFILLIATIONS = lists:map(
    fun({{Uname, _Domain, _Res}, _Stuff}) ->
      binary_to_list(Uname) ++ ".."
    end,
    dict:to_list(MUCState#state.affiliations)
  ),
  ?INFO_MSG(" #########    GROUPCHAT _AFILLIATIONS = ~p~n  #######   ", [_AFILLIATIONS]),

  _OFFLINE = lists:subtract(_AFILLIATIONS, _LISTUSERS),
  ?INFO_MSG(" #########    GROUPCHAT _OFFLINE = ~p~n  #######   ", [_OFFLINE]),

  %%?INFO_MSG(Items, []),
  Pkt.

post_result({_ReqId, _Result}) ->
  ok.

get_opt(Opt) ->
      get_opt(Opt, undefined).

get_opt(Opt, Default) ->
      F = fun(Val) when is_binary(Val) -> binary_to_list(Val);
             (Val)                     -> Val
           end,
      gen_mod:get_module_opt(global, ?MODULE, Opt, F, Default).

%% Erlang now()-style timestamps are in UTC by definition, and we are
%% assuming ISO 8601 dates should be printed in UTC as well, so no
%% conversion necessary
%%
%% Example:
%%   {1385,388790,334905}
%%     -becomes-
%%   2013-11-25T14:13:10.334905Z
-spec to_iso_8601_date(erlang:timestamp()) -> string().
to_iso_8601_date(Timestamp) when is_tuple(Timestamp) ->
     {{Y, Mo, D}, {H, M, S}} = calendar:now_to_universal_time(Timestamp),
         {_, _, US} = Timestamp,
         lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                                      [Y, Mo, D, H, M, S, US])).
