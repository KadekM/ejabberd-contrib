-module(mod_offline_group_post_log).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-include("mod_muc_room.hrl").

-export([start/2, stop/1, muc_filter_message/5]).

start(Host, _Opts) ->
  ?INFO_MSG("mod_offline_group_post_log started", []),
  case inets:start() of
    {error, {already_started, _}} -> ok;
    ok -> ok
  end,
  ejabberd_hooks:add(muc_filter_message, Host, ?MODULE,
    muc_filter_message, 1),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, muc_filter_message, 1),
  ok.

muc_filter_message(Stanza, MUCState, RoomJID, From, FromNick) ->
  Body = xml:get_path_s(Stanza, [{elem, list_to_binary("body")}, cdata]),
  Timestamp = to_iso_8601_date(os:timestamp()),
  UsersOnline = dict:to_list(MUCState#state.users),
  UsersSubscribed = dict:to_list(MUCState#state.affiliations),


  %% Json has this structure:
  %{
  %"room": {
  %  "jid": "testroom@conference.localhost",
  %  "users": {
  %    "subscribed": ["foo@localhost", "foo2@localhost"],
  %    "online": ["foo@localhost"]
  %  }
  %},
  %"from": {
  %  "jid": "marek@localhost",
  %  "nick": "marecek"
  %  }
  % "message": "hello world",
  % "timestamp": "123",
  % "xurls": []
  %}

  % Room json:
  JsonRoomJid = jlib:jid_to_string(RoomJID),
  JsonRoomUsersOnline = json_from_users(UsersOnline),
  JsonRoomUsersSubscribed = json_from_users_subscribed(UsersSubscribed),
  JsonRoomUsersField = io_lib:format("{\"subscribed\":~s,\"online\":~s}",
    [JsonRoomUsersSubscribed, JsonRoomUsersOnline]),
  JsonRoom = io_lib:format("{\"jid\":\"~s\",\"users\":~s}",[JsonRoomJid, JsonRoomUsersField]),

  % From json:
  JsonFrom = json_from_jidnick(From, FromNick),

  % XUrls json:
  JsonXUrls = get_x_urls(Stanza#xmlel.children),

  % Final json:
  JsonBody = unicode:characters_to_binary(io_lib:format("{\"room\":~s,\"from\":~s,\"message\":\"~ts\",\"timestamp\":\"~s\",\"xurls\":~s}",
    [JsonRoom, JsonFrom, Body, Timestamp, list_to_json(JsonXUrls)])),

  % Send request:
  Url = get_opt(url),
  ?INFO_MSG(" Contacing ~s with body ~s~n", [Url, JsonBody]),

  BasicAuthUsername = get_opt(username),
  BasicAuthPassword = get_opt(password),
  BasicAuth = basic_auth_header(BasicAuthUsername, BasicAuthPassword),
  case httpc:request(post, {Url, [BasicAuth, {"te", "deflate"}], "application/json", JsonBody},
          [], []) of
  {error, Reason} ->
      ?ERROR_MSG("Error while accessing messaging endpoint. Reason: ~p.", [Reason]);
  {_, _} -> ignore
  end,

  Stanza.

% Get online users, which is Map from Jid -> Informations
json_from_users(Users) ->
  Mapped = lists:map(
    fun({_LJID, Info}) ->
      unicode:characters_to_list(jlib:jid_to_string(Info#user.jid))
    end, Users
  ),
  list_to_json(Mapped).

% Get subscribed users (which is Map from Jid -> ???) = so basically read keys of map
json_from_users_subscribed(Users) ->
    Mapped = lists:map(
      fun({Jid, _Stuff}) ->
        unicode:characters_to_list(jlib:jid_to_string(Jid))
      end, Users),
  list_to_json(Mapped).

json_from_jidnick(Jid, Nick) ->
  FormattedJid = jlib:jid_to_string(Jid),
  io_lib:format("{\"jid\":\"~s\",\"nick\":\"~s\"}", [FormattedJid, Nick]).

%% -----------------
%% UTILITY FUNCTIONS
%% -----------------
get_x_urls(Els) ->
  OnlyXTags = [El || El <- Els, is_record(El, xmlel) andalso element(#xmlel.name, El) =:= <<"x">>,
    lists:keyfind(<<"xmlns">>, 1, element(#xmlel.attrs, El)) =:= {<<"xmlns">>, <<"jabber:x:oob">>}],
  FlattenChildren = [Children || El <- OnlyXTags, Children <- element(#xmlel.children, El), is_record(Children, xmlel), element(#xmlel.name, Children) =:= <<"url">> ],
  [binary_to_list(element(2, UrlNode)) || U <- FlattenChildren, UrlNode <- element(#xmlel.children, U)].


list_to_json(List) ->
  io_lib:format("~p", [List]).

basic_auth_header(Username, Password) ->
  {"Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password)}.

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
     {{Y, Mo, D}, {H, M, S}} = calendar:now_to_local_time(Timestamp),
         {_, _, US} = Timestamp,
         lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                                      [Y, Mo, D, H, M, S, US])).
