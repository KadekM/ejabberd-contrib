-module(mod_offline_xep0066).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([start/2, stop/1, log_user_send/4, log_user_receive_packet/5, log_user_offline_hook/3]).

%% UNFINISHED ------------- at this point we are going just going to send message

%% TODO: 
%% - enable the https
%% - cleanup (unused methods etc..)
%% - contact endpoint only if user is offline [probably BIG task]
%% - track requests


start(Host, _Opts) ->
  %application:start(ssl), % todo: for https
  case inets:start() of 
    {error, {already_started, _}} -> ok;
    ok -> ok
  end,
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, log_user_offline_hook, 1),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_user_send, 1),
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, log_user_receive_packet, 1),
  ok.

stop(Host) ->
  ?INFO_MSG("Stopping", []),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, log_user_offline_hook, 1),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, log_user_send, 1),
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, log_user_receive_packet, 1),
  ok.

log_user_offline_hook(From, To, Packet) ->
  ?INFO_MSG("!!!#OFFFFFFFFFFFLIIIIIIIIIIINEHOOOOOOOOOOOOOOOOOOOOOK", []),
  %?INFO_MSG(Packet, []),
  ok.

%log_user_send(State, From, To, Packet) ->
log_user_send(Packet, C2SState, From, To) ->
%  ?INFO_MSG(C2SState, []),
%  ?INFO_MSG(From, []),
%  ?INFO_MSG(To, []),
   ?INFO_MSG("!!!SENDING~~~~~~~~~~~~~~", []),
   ?INFO_MSG(Packet, []),
  ?INFO_MSG("~~~~~~~~~~~~~~~~~~~~~~~~~~~", []),
   %process_packet(From, To, Packet),
   Packet.
                                         
log_user_receive_packet(Packet, C2SState, JID, From, To) ->
  ?INFO_MSG("!!!RECEIVING~~~~~~~~~~~", []),
  %%?INFO_MSG(C2SState, []),
  %%?INFO_MSG(JID, []),
  ?INFO_MSG(Packet, []),
  ?INFO_MSG("~~~~~~~~~~~~~~~~~~~~~~~~~~~", []),
  Packet.

process_packet(From, To, #xmlel{name = <<"iq">>} = Packet) ->
  ok = process_message(From, To, Packet);

process_packet(_From, _To, _Packet) ->
  ok.

process_message(From, To, #xmlel{attrs = Attrs} = Packet) ->
  Type = lists:keyfind(<<"type">>, 1, Attrs),
  Id = lists:keyfind(<<"id">>, 1, Attrs),
  Query = xml:get_subtag(Packet, <<"query">>),
  process_message_filter(From, To, Type, Id, Query),
  ok;

process_message(_From, _To, _Packet) ->
  ok.

process_message_filter(From, To, {<<"type">>, Type}, {<<"id">>, Id}, #xmlel{name = <<"query">>} = Query)
  when Type =:= <<"set">> andalso Id =/= <<"">> ->
  Url = xml:get_subtag(Query, <<"url">>),
  Desc = xml:get_subtag(Query, <<"desc">>),
  process_message_filter_data(From, To, Type, Id, Url, Desc),
  ok;

process_message_filter(_From, _To, _Type, _Id, _Packet) ->
  ok.

process_message_filter_data(From, To, Type, Id, #xmlel{name = <<"url">>} = Url, #xmlel{name = <<"desc">>} = Desc) ->
  UrlCData = xml:get_tag_cdata(Url),
  DescCData = xml:get_tag_cdata(Desc),

  ?INFO_MSG(UrlCData, []),
  ok;

process_message_filter_data(_From, _To, _Type, _Id, _Url, _Desc) ->
  ok.

% I didnt want to introduce dependency on json serializer just for this case
jid_to_json(#jid{luser = Username, lserver = Server, lresource = Resource}) ->
  io_lib:format("{\"username\":\"~s\",\"server\":\"~s\",\"resource\":\"~s\"}",
                [Username, Server, Resource]);
jid_to_json(_) ->
  "unknown".

basic_auth_header(Username, Password) ->
  {"Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password)}.
