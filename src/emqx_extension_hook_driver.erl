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

-module(emqx_extension_hook_driver).

%% Load/Unload
-export([ load/3
        , unload/1
        , connect/1
        ]).

%% APIs
-export([run_hook/3]).

%% init - main:init/1
%% deinit - main:deinit/1
%% call -> M:F/x

-record(driver, {
          %% Driver name (equal to ecpool name)
          name :: driver_name(),
          %% Driver type
          type :: driver_type(),
          %% Hook Spec
          hookspec :: hook_spec(),
          %% low layer state
          state
       }).

-type driver_name() :: python2 | python3 | webhook | java | lua | atom().
-type driver_type() :: python | webhok | java | atom().
-type driver() :: #driver{}.

-type hook_spec() :: #{hookname() => {callback_m(), callback_f(), spec()}}.
-type hookname() :: client_connect
                  | client_connack
                  | client_connected
                  | client_disconnected
                  | client_authenticate
                  | client_check_acl
                  | client_subscribe
                  | client_unsubscribe
                  | session_created
                  | session_subscribed
                  | session_unsubscribed
                  | session_resumed
                  | session_discarded      %% Should squash to `terminated` ?
                  | session_takeovered     %% Should squash to `terminated` ?
                  | session_terminated
                  | message_publish
                  | message_delivered
                  | message_acked
                  | message_dropped.

-type callback_m() :: atom().

-type callback_f() :: atom().

-type spec() :: #{
        topic => binary()   %% for `message` hook only
       }.

-export_type([driver/0]).

%%--------------------------------------------------------------------
%% Load/Unload APIs
%%
%% - FIXME: start ?? stop ??
%%--------------------------------------------------------------------

-spec load(atom(), list(), hook_spec()) -> ok.
load(Name, Opts, DeftHooks) ->
    Spec = pool_spec(Name, Opts),
    ok = emqx_extension_hook_sup:start_driver_pool(Spec),
    do_init(Name, Opts, DeftHooks).

%% FIXME: Does the port will be released??

-spec unload(driver()) -> ok.
unload(#driver{name = Name}) ->
    emqx_extension_hook_sup:stop_driver_pool(Name).

do_init(Name, _Opts, DeftHooks) ->
    Type = type(Name),
    case raw_call(Type, Name, 'main', 'init', #{}) of
        {ok, {HookSpec, State}} ->
            %% FIXME: Crash here??
            NHookSpec = resovle_hook_spec(HookSpec, DeftHooks),
            %% TODO: Register Metrics
            {ok, #driver{type = Type,
                         name = Name,
                         state = State,
                         hookspec = NHookSpec}};
        {error, Reason} ->
            emqx_extension_hook_sup:stop_driver_pool(Name),
            {error, Reason}
    end.

%% @private
pool_spec(Name, Opts)
  when Name =:= python2;
       Name =:= python3 ->
    ecpool:pool_spec(Name, Name, ?MODULE, [{python, Name} | Opts]);

pool_spec(_, _) ->
    error(not_supported_driver_type).

%% HookSpec = #{hookname() => {callback_m(), callback_f(), spec()}}
%% DeftHooks = [{hookname, map()}]
resovle_hook_spec(_HookSpec, _DeftHooks) ->
    #{client_connected => {main, on_client_connected, #{}}}.

%%--------------------------------------------------------------------
%% ecpool callback
%%--------------------------------------------------------------------

-spec connect(list()) -> {ok, pid()} | {error, any()}.
connect(Opts = [{python, _} | _]) ->
    python:start(Opts).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

-spec run_hook(atom(), list(), driver()) -> ignore | term().
run_hook(Name, Args, Driver = #driver{hookspec = HookSpec}) ->
    case maps:get(Name, HookSpec, null) of
        {M, F, Opts} ->
            case match_topic_filter(Name, maps:get(topic, Args, null), maps:get(topics, Opts, [])) of
                true -> call(M, F, Args, Driver);
                _ -> ignore
            end;
        _ -> ignore
    end.

-compile({inline, [match_topic_filter/3]}).
match_topic_filter(_Name, null, _TopicFilter) ->
    true;
match_topic_filter(Name, TopicName, TopicFilter)
  when Name =:= message_publish;
       Name =:= message_delivered;
       Name =:= message_dropped;
       Name =:= message_acked ->
    lists:any(fun(F) -> emqx_topic:match(TopicName, F) end, TopicFilter);
match_topic_filter(_, _, _) ->
    true.

-spec call(atom(), atom(), list(), driver()) -> {ok, term()} | {error, term()}.
call(Mod, Fun, Args, #driver{name = Name, type = Type, state = State}) ->
    with_pool(Name, fun(C) ->
        %% FIXME: Args?
        raw_call(Type, C, Mod, Fun, Args#{state => State})
    end).

raw_call(python, C, M, F, A) ->
    case python:call(C, M, F, A) of
        {_Ok = 0, Return} -> {ok, Return};
        {_Err = 1, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

with_pool(Name, Fun) ->
    ecpool:with_client(Name, Fun).

type(python3) -> python;
type(python2) -> python.

