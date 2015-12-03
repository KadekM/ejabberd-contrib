-module(mod_offline_post_log).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([start/2, stop/1, log_user_send/3]).

% TODO: ssl is disabled ???

start(Host, _Opts) ->
  ?INFO_MSG("mod_offline_post_log started", []),
  case inets:start() of
    {error, {already_started, _}} -> ok;
    ok -> ok
  end,
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, log_user_send, 1),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, log_user_send, 1),
  ok.

log_user_send(From, To, Packet) ->
  process_packet(From, To, Packet),
  ok.

process_packet(From, To, #xmlel{name = <<"message">>} = Packet) ->
  ok = process_message(From, To, Packet);

process_packet(_From, _To, _Packet) ->
  ok.

process_message(From, To, #xmlel{attrs = Attrs} = Packet) ->
  case lists:keyfind(<<"type">>, 1, Attrs) of
    {_, _} = Type -> process_message_filter(From, To, Type, Packet);
             false -> process_message_filter(From, To, {<<"type">>, <<"">>}, Packet)
  end,
  ok;

process_message(_From, _To, _Packet) ->
  ok.

process_message_filter(From, To, {<<"type">>, Type}, Packet)
  when Type =:= <<"chat">> orelse Type =:= <<"">> ->
  log_message(From, To, Packet),
  ok;

process_message_filter(_From, _To, _Type, _Packet) ->
  ok.

log_message(From, To, #xmlel{children = Els} = _Packet) ->
  case get_body(Els) of
    no_body ->
      ok;
    {ok, Body} ->
      XUrls = get_x_urls(Els),
      post_message(From, To, Body, XUrls),
      ok
  end;

log_message(_From, _To, _Packet) ->
  ok.

get_body(Els) ->
  XmlEls = [ El || El <- Els, is_record(El, xmlel) ],
  case lists:keyfind(<<"body">>, #xmlel.name, XmlEls) of
    false ->
      no_body;
    #xmlel{children = InnerEls} ->
      case lists:keyfind(xmlcdata, 1, InnerEls) of
        false ->
          no_body;
        {xmlcdata, Body} ->
          {ok, unicode:characters_to_list(Body)}
      end
  end.

post_message(From, To, Body, XUrls) ->
    Timestamp = to_iso_8601_date(os:timestamp()),
    JsonBody = unicode:characters_to_binary(to_json(From, To, Body, Timestamp, XUrls)),
    Url = get_opt(url),
    ?DEBUG("Contacing ~s with body ~s", [Url, JsonBody]),
    BasicAuthUsername = get_opt(username),
    BasicAuthPassword = get_opt(password),
    BasicAuth = basic_auth_header(BasicAuthUsername, BasicAuthPassword),
    case httpc:request(post, {Url, [BasicAuth, {"te", "deflate"}], "application/json", JsonBody},
            [], []) of
      {Error, Reason} ->
        ?ERROR_MSG("Error while accessing messaging endpoint. Error: ~p. Reason: ~p.",
          [Error, Reason])
    end,
    ok.

%% -----------------
%% UTILITY FUNCTIONS
%% -----------------

get_x_urls(Els) ->
  OnlyXTags = [El || El <- Els, is_record(El, xmlel) andalso element(#xmlel.name, El) =:= <<"x">>,
                     lists:keyfind(<<"xmlns">>, 1, element(#xmlel.attrs, El)) =:= {<<"xmlns">>, <<"jabber:x:oob">>}],
  FlattenChildren = [Children || El <- OnlyXTags, Children <- element(#xmlel.children, El), is_record(Children, xmlel), element(#xmlel.name, Children) =:= <<"url">> ],
  [binary_to_list(element(2, UrlNode)) || U <- FlattenChildren, UrlNode <- element(#xmlel.children, U)].

to_json(From, To, Body, Timestamp, XUrls) ->
  io_lib:format("{\"from\":\"~s\",\"to\":\"~s\",\"message\":\"~ts\",\"timestamp\":\"~s\",\"xurls\":~s}",
                [jlib:jid_to_string(From), jlib:jid_to_string(To), Body, Timestamp, list_to_json(XUrls)]).


% this will create JSON such as ["url1", "url2"...]
list_to_json(XUrls) ->
  io_lib:format("~p", [XUrls]).
  

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
     {{Y, Mo, D}, {H, M, S}} = calendar:now_to_universal_time(Timestamp),
         {_, _, US} = Timestamp,
         lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                                      [Y, Mo, D, H, M, S, US])).
