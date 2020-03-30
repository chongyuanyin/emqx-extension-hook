%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_extension_hook).

-include("emqx_extension_hook.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-logger_header("[ExHook]").

-export([ load/0
        , unload/0
        ]).

-export([ on_client_connect/2
        , on_client_connack/3
        , on_client_connected/2
        , on_client_disconnected/3
        , on_client_authenticate/2
        , on_client_check_acl/4
        , on_client_subscribe/3
        , on_client_unsubscribe/3
        ]).

%% Session Lifecircle Hooks
-export([ on_session_created/2
        , on_session_subscribed/3
        , on_session_unsubscribed/3
        , on_session_resumed/2
        , on_session_discarded/2
        , on_session_takeovered/2
        , on_session_terminated/3
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/1
        , on_message_delivered/2
        , on_message_acked/2
        , on_message_dropped/3
        ]).

%%--------------------------------------------------------------------
%% Load/Unload
%%--------------------------------------------------------------------

load() ->
    emqx:hook('client.connect',      {?MODULE, on_client_connect,       []}),
    emqx:hook('client.connack',      {?MODULE, on_client_connack,       []}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected,     []}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected,  []}),
    emqx:hook('client.authenticate', {?MODULE, on_client_authenticate,  []}),
    emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl,     []}),
    emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe,     []}),
    emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe,   []}),
    emqx:hook('session.created',     {?MODULE, on_session_created,      []}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed,   []}),
    emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, []}),
    emqx:hook('session.resumed',     {?MODULE, on_session_resumed,      []}),
    emqx:hook('session.discarded',   {?MODULE, on_session_discarded,    []}),
    emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered,   []}),
    emqx:hook('session.terminated',  {?MODULE, on_session_terminated,   []}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish,      []}),
    emqx:hook('message.delivered',   {?MODULE, on_message_delivered,    []}),
    emqx:hook('message.acked',       {?MODULE, on_message_acked,        []}),
    emqx:hook('message.dropped',     {?MODULE, on_message_dropped,      []}).

unload() ->
    emqx:unhook('client.connect',      {?MODULE, on_client_connect}),
    emqx:unhook('client.connack',      {?MODULE, on_client_connack}),
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    emqx:unhook('client.check_acl',    {?MODULE, on_client_check_acl}),
    emqx:unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
    emqx:unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
    emqx:unhook('session.created',     {?MODULE, on_session_created}),
    emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    emqx:unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
    emqx:unhook('session.resumed',     {?MODULE, on_session_resumed}),
    emqx:unhook('session.discarded',   {?MODULE, on_session_discarded}),
    emqx:unhook('session.takeovered',  {?MODULE, on_session_takeovered}),
    emqx:unhook('session.terminated',  {?MODULE, on_session_terminated}),
    emqx:unhook('message.publish',     {?MODULE, on_message_publish}),
    emqx:unhook('message.delivered',   {?MODULE, on_message_delivered}),
    emqx:unhook('message.acked',       {?MODULE, on_message_acked}),
    emqx:unhook('message.dropped',     {?MODULE, on_message_dropped}).


-spec send(atom(), map()) -> {ok, map()} | {error, term()}.
send(Name, Args) ->
    send(Name, Args, emqx_extension_hook_app:drivers()).

send(_, _, []) -> ok;
send(Name, Args, [Driver|More]) ->
    emqx_extension_hook:call(Name, Args, Driver),
    send(Name, Args, More).

%%--------------------------------------------------------------------
%% Clients
%%--------------------------------------------------------------------

on_client_connect(ConnInfo = #{clientid := ClientId, username := Username, peername := {Peerhost, _}}, Props) ->
    Args = #{clientid => ClientId,
             username => Username,
             ipaddress => iolist_to_binary(ntoa(Peerhost)),
             keepalive => maps:get(keepalive, ConnInfo),
             proto_ver => maps:get(proto_ver, ConnInfo)
            },
    send('client_connect', Args),
    {ok, Props};
on_client_connect(_ConnInfo, _ConnProp) ->
    ok.

on_client_connack(ConnInfo = #{clientid := ClientId, username := Username, peername := {Peerhost, _}}, Rc, Props) ->
    Args = #{ clientid => ClientId
            , username => Username
            , ipaddress => iolist_to_binary(ntoa(Peerhost))
            , keepalive => maps:get(keepalive, ConnInfo)
            , proto_ver => maps:get(proto_ver, ConnInfo)
            , conn_ack => Rc
            },
    send('client_connack', Args),
    {ok, Props};
on_client_connack(_ConnInfo, _Rc, _AckProps) ->
    ok.

on_client_connected(#{clientid := ClientId, username := Username, peerhost := Peerhost}, ConnInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            , ipaddress => iolist_to_binary(ntoa(Peerhost))
            , keepalive => maps:get(keepalive, ConnInfo)
            , proto_ver => maps:get(proto_ver, ConnInfo)
            , connected_at => maps:get(connected_at, ConnInfo)
            },
    send('client_connected', Args),
    ok;
on_client_connected(_ClientInfo, _ConnInfo) ->
    ok.

on_client_disconnected(ClientInfo, {shutdown, Reason}, ConnInfo) when is_atom(Reason) ->
    on_client_disconnected(ClientInfo, Reason, ConnInfo);
on_client_disconnected(#{clientid := ClientId, username := Username}, Reason, _ConnInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            , reason => printable(Reason)
            },
    send('client_disconnected', Args),
    ok.

%% @private
printable(Term) when is_atom(Term); is_binary(Term) ->
    Term;
printable(Term) when is_tuple(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

on_client_authenticate(#{clientid := ClientId, username := Username}, AuthResult) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('client_authenticate', Args),
    {ok, AuthResult}.

on_client_check_acl(_ClientInfo = #{clientid := ClientId, username := Username}, Topic, PubSub, Result) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('client_check_acl', Args),
    {ok, Result}.

on_client_subscribe(#{clientid := ClientId, username := Username}, _Properties, TopicFilters) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('client_subscribe', Args),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId, username := Username}, _Properties, TopicFilters) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('client_unsubscribe', Args),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId, username := Username}, SessInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_created', Args),
    ok.

on_session_subscribed(#{clientid := ClientId, username := Username}, Topic, SubOpts) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_subscribed', Args),
    ok.

on_session_unsubscribed(#{clientid := ClientId, username := Username}, Topic, Opts) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_unsubscribed', Args),
    ok.

on_session_resumed(#{clientid := ClientId, username := Username}, SessInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_resumed', Args),
    ok.

on_session_discarded(_ClientInfo = #{clientid := ClientId, username := Username}, SessInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_discarded', Args),
    ok.

on_session_takeovered(_ClientInfo = #{clientid := ClientId, username := Username}, SessInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_takeovered', Args),
    ok.

on_session_terminated(_ClientInfo = #{clientid := ClientId, username := Username}, Reason, SessInfo) ->
    Args = #{ clientid => ClientId
            , username => Username
            },
    send('session_terminated', Args),
    ok.

%%--------------------------------------------------------------------
%% Message
%%--------------------------------------------------------------------

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}) ->
    {ok, Message};
on_message_publish(Message) ->
    {FromClientId, FromUsername} = format_from(Message),
    Args = #{ clientid => FromClientId
            , username => FromUsername
            },
    send('message_publish', Args),
    {ok, Message}.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason) ->
    {FromClientId, FromUsername} = format_from(Message),
    Args = #{ clientid => FromClientId
            , username => FromUsername
            },
    send('message_dropped', Args),
    ok.

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message) ->
    {FromClientId, FromUsername} = format_from(Message),
    Args = #{ clientid => FromClientId
            , username => FromUsername
            },
    send('message_delivered', Args),
    {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message) ->
    {FromClientId, FromUsername} = format_from(Message),
    Args = #{ clientid => FromClientId
            , username => FromUsername
            },
    send('message_acked', Args),
    ok.

%%--------------------------------------------------------------------
%% Types

format_from(#message{from = ClientId, headers = #{username := Username}}) ->
    {a2b(ClientId), a2b(Username)};
format_from(#message{from = ClientId, headers = _HeadersNoUsername}) ->
    {a2b(ClientId), <<"undefined">>}.

ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
    inet_parse:ntoa(IP).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.
