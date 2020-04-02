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

-spec cast(atom(), list()) -> ok.
cast(Name, Args) ->
    cast(Name, Args, emqx_extension_hook_app:drivers()).

cast(_, _, []) -> ok;
cast(Name, Args, [Driver|More]) ->
    emqx_extension_hook_driver:run_hook(Name, Args, Driver),
    cast(Name, Args, More).

-spec call_fold(atom(), list(), term()) -> ok | {stop, term()}.
call_fold(Name, InfoArgs, AccArg) ->
    call_fold(Name, InfoArgs, AccArg, emqx_extension_hook_app:drivers()).

call_fold(_, _, _, []) ->
    ok;
call_fold(Name, InfoArgs, AccArg, [Driver|More]) ->
    case emqx_extension_hook_driver:run_hook(Name, InfoArgs ++ [AccArg], Driver) of
        ok         -> call_fold(Name, InfoArgs, AccArg, More);
        {error, _} -> call_fold(Name, InfoArgs, AccArg, More);
        {ok, NAcc} ->
            case validate_acc_arg(Name, NAcc) of
                true ->
                    {stop, NAcc};
                _ ->
                    ?LOG(error, "Got invalid return type for calling ~p on ~p",
                         [Name, emqx_extension_hook_driver:name(Driver)]),
                    call_fold(Name, InfoArgs, AccArg, More)
            end
    end.

validate_acc_arg('client_authenticate', N) when is_boolean(N) -> true;
validate_acc_arg('client_check_acl',    N) when is_boolean(N) -> true;
validate_acc_arg('message_publish',     L) when is_list(L) -> validate_msg(L, true);
validate_acc_arg(_,                     _) -> false.

validate_msg(_, false) -> false;
validate_msg([{topic, T} | More], _) ->
    validate_msg(More, is_binary(T));
validate_msg([{payload, P} | More], _) ->
    validate_msg(More, is_binary(P));
validate_msg([{qos, Q} | More], _) ->
    validate_msg(More, Q =< 2 andalso Q >= 0).

%%--------------------------------------------------------------------
%% Clients
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, _Props) ->
    cast('client_connect', [conninfo(ConnInfo), props(_Props)]).

on_client_connack(ConnInfo, Rc, _Props) ->
    cast('client_connack', [conninfo(ConnInfo), Rc, props(_Props)]).

on_client_connected(ClientInfo, _ConnInfo) ->
    cast('client_connected', [clientinfo(ClientInfo)]).

on_client_disconnected(ClientInfo, {shutdown, Reason}, ConnInfo) when is_atom(Reason) ->
    on_client_disconnected(ClientInfo, Reason, ConnInfo);
on_client_disconnected(ClientInfo, Reason, _ConnInfo) ->
    cast('client_disconnected', [clientinfo(ClientInfo), stringfy(Reason)]).

on_client_authenticate(ClientInfo, AuthResult) ->
    AccArg = maps:get(auth_result, AuthResult, undefined) == success,
    case call_fold('client_authenticate', [clientinfo(ClientInfo)], AccArg) of
        {stop, Bool} when is_boolean(Bool) ->
            Result = case Bool of true -> success; _ -> not_authorized end,
            {stop, AuthResult#{auth_result => Result, anonymous => false}};
        _ ->
            {ok, AuthResult}
    end.

on_client_check_acl(ClientInfo, Topic, PubSub, Result) ->
    AccArg = Result == allow,
    case call_fold('client_check_acl', [clientinfo(ClientInfo), Topic, PubSub], AccArg) of
        {stop, Bool} when is_boolean(Bool) ->
            NResult = case Bool of true -> allow; _ -> deny end,
            {stop, NResult};
        _ -> {ok, Result}
    end.

on_client_subscribe(ClientInfo, Props, TopicFilters) ->
    cast('client_subscribe', [clientinfo(ClientInfo), props(Props), topicfilters(TopicFilters)]).

on_client_unsubscribe(Clientinfo, Props, TopicFilters) ->
    cast('client_unsubscribe', [clientinfo(Clientinfo), props(Props), topicfilters(TopicFilters)]).

%%--------------------------------------------------------------------
%% Session
%%--------------------------------------------------------------------

on_session_created(ClientInfo, _SessInfo) ->
    cast('session_created', [clientinfo(ClientInfo)]).

on_session_subscribed(Clientinfo, Topic, SubOpts) ->
    cast('session_subscribed', [clientinfo(Clientinfo), Topic, props(SubOpts)]).

on_session_unsubscribed(ClientInfo, Topic, _SubOpts) ->
    cast('session_unsubscribed', [clientinfo(ClientInfo), Topic]).

on_session_resumed(ClientInfo, _SessInfo) ->
    cast('session_resumed', [clientinfo(ClientInfo)]).

on_session_discarded(ClientInfo, _SessInfo) ->
    cast('session_discarded', [clientinfo(ClientInfo)]).

on_session_takeovered(ClientInfo, _SessInfo) ->
    cast('session_takeovered', [clientinfo(ClientInfo)]).

on_session_terminated(ClientInfo, Reason, _SessInfo) ->
    cast('session_terminated', [clientinfo(ClientInfo), stringfy(Reason)]).

%%--------------------------------------------------------------------
%% Message
%%--------------------------------------------------------------------

on_message_publish(#message{topic = <<"$SYS/", _/binary>>}) ->
    ok;
on_message_publish(Message = #message{headers = Headers}) ->
    case call_fold('message_publish', [], message(Message)) of
        {stop, false} ->
            {stop, Message#message{headers = Headers#{allow_publish := false}}};
        {stop, NMessage} ->
            {ok, assign_to_message(NMessage, Message)};
        _ ->
            {ok, Message}
    end.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason) ->
    ok;
on_message_dropped(Message, _By, Reason) ->
    cast('message_dropped', [message(Message), stringfy(Reason)]).

on_message_delivered(_ClientInfo, #message{topic = <<"$SYS/", _/binary>>}) ->
    ok;
on_message_delivered(ClientInfo, Message) ->
    cast('message_delivered', [clientinfo(ClientInfo), message(Message)]).

on_message_acked(_ClientInfo, #message{topic = <<"$SYS/", _/binary>>}) ->
    ok;
on_message_acked(ClientInfo, Message) ->
    cast('message_acked', [clientinfo(ClientInfo), message(Message)]).

%%--------------------------------------------------------------------
%% Types

props(undefined) -> [];
props(M) when is_map(M) -> maps:to_list(M).

conninfo(_ConnInfo =
         #{clientid := ClientId, username := Username, peername := {Peerhost, _},
           sockname := {_, SockPort}, proto_name := ProtoName, proto_ver := ProtoVer,
           keepalive := Keepalive}) ->
    [{node, node()},
     {clientid, ClientId},
     {username, maybe(Username)},
     {peerhost, ntoa(Peerhost)},
     {sockport, SockPort},
     {proto_name, ProtoName},
     {proto_ver, ProtoVer},
     {keepalive, Keepalive}].

clientinfo(ClientInfo =
           #{clientid := ClientId, username := Username, peerhost := PeerHost,
             sockport := SockPort, protocol := Protocol, mountpoint := Mountpoiont}) ->
    [{node, node()},
     {clientid, ClientId},
     {username, maybe(Username)},
     {password, maybe(maps:get(password, ClientInfo, undefined))},
     {peerhost, ntoa(PeerHost)},
     {sockport, SockPort},
     {protocol, Protocol},
     {mountpoint, maybe(Mountpoiont)},
     {is_superuser, maps:get(is_superuser, ClientInfo, false)},
     {anonymous, maps:get(anonymous, ClientInfo, true)}].

message(#message{id = Id, qos = Qos, from = From, topic = Topic, payload = Payload, timestamp = Ts}) ->
    [{node, node()},
     {id, hexstr(Id)},
     {qos, Qos},
     {from, From},
     {topic, Topic},
     {payload, Payload},
     {timestamp, Ts}].

topicfilters(Tfs = [{_, _}|_]) ->
    [{Topic, Qos} || {Topic, #{qos := Qos}} <- Tfs];
topicfilters(Tfs) ->
    Tfs.

ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    list_to_binary(inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256}));
ntoa(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

maybe(undefined) -> <<"">>;
maybe(B) -> B.

%% @private
stringfy(Term) when is_atom(Term); is_binary(Term) ->
    Term;
stringfy(Term) when is_tuple(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

hexstr(B) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(B)]).

assign_to_message([], Message) ->
    Message;
assign_to_message([{topic, Topic}|More], Message) ->
    assign_to_message(More, Message#message{topic = Topic});
assign_to_message([{qos, Qos}|More], Message) ->
    assign_to_message(More, Message#message{qos = Qos});
assign_to_message([{payload, Payload}|More], Message) ->
    assign_to_message(More, Message#message{payload = Payload});
assign_to_message([_|More], Message) ->
    assign_to_message(More, Message).

