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

-include_lib("emqx/include/logger.hrl").

-logger_header("[ExHook Driver]").

%% Load/Unload
-export([ load/3
        , unload/1
        , connect/1
        ]).

%% APIs
-export([run_hook/3]).

%% Infos
-export([ name/1
        , format/1
        ]).

-record(driver, {
          %% Driver name (equal to ecpool name)
          name :: driver_name(),
          %% Driver type
          type :: driver_type(),
          %% Initial Module name
          init :: atom(),
          %% Hook Spec
          hookspec :: hook_spec(),
          %% Metric fun
          incfun :: function(),
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
%%--------------------------------------------------------------------

-spec load(atom(), list(), hook_spec()) -> ok | {error, term()} .
load(Name, Opts0, DeftHooks) ->
    case lists:keytake(init_module, 1, Opts0) of
        false -> {error, not_found_initial_module};
        {value, {_,InitM}, Opts} ->
            Spec = pool_spec(Name, Opts),
            {ok, _} = emqx_extension_hook_sup:start_driver_pool(Spec),
            do_init(Name, InitM, DeftHooks)
    end.

%% FIXME: Does the port will be released??

-spec unload(driver()) -> ok.
unload(#driver{name = Name, init = InitM}) ->
    do_deinit(Name, InitM),
    emqx_extension_hook_sup:stop_driver_pool(Name).

do_deinit(Name, InitM) ->
    _ = raw_call(type(Name), Name, InitM, 'deinit', []),
    ok.

do_init(Name, InitM, DeftHooks) ->
    Type = type(Name),
    case raw_call(Type, Name, InitM, 'init', []) of
        {ok, {HookSpec, State}} ->
            NHookSpec = resovle_hook_spec(HookSpec, DeftHooks),
            %% Reigster metrics
            Prefix = "exhook." ++ atom_to_list(Name) ++ ".",
            metrics_new(Prefix, NHookSpec),
            {ok, #driver{type = Type,
                         name = Name,
                         init = InitM,
                         state = State,
                         hookspec = NHookSpec,
                         incfun = incfun(Prefix) }};
        {error, Reason} ->
            emqx_extension_hook_sup:stop_driver_pool(Name),
            {error, Reason}
    end.

%% @private
pool_spec(Name, Opts)
  when Name =:= python2;
       Name =:= python3 ->
    ecpool:pool_spec(Name, Name, ?MODULE, [{python, atom_to_list(Name)} | Opts]);

pool_spec(_, _) ->
    error(not_supported_driver_type).

resovle_hook_spec([], DeftHooks) ->
    DeftHooks;
resovle_hook_spec(HookSpec, _) ->
    Atom = fun(B) -> list_to_atom(B) end,
    lists:foldr(fun({Name, Module, Func, Spec}, Acc) ->
        Acc#{Atom(Name) => {Atom(Module), Atom(Func), maps:from_list(Spec)}}
    end, #{}, HookSpec).

metrics_new(Prefix, HookSpec) ->
    Keys = [ list_to_atom(Prefix ++ atom_to_list(K)) || K <- maps:keys(HookSpec)],
    lists:foreach(fun emqx_metrics:new/1, Keys).

incfun(Prefix) ->
    fun(Name) ->
        emqx_metrics:inc(list_to_atom(Prefix ++ atom_to_list(Name)))
    end.

format(#driver{name = Name, init = InitM, hookspec = Hooks}) ->
    %% Print env???
    io_lib:format("name=~p, init_module=~p, hooks=~0p", [Name, InitM, maps:keys(Hooks)]).

%%--------------------------------------------------------------------
%% ecpool callback
%%--------------------------------------------------------------------

-spec connect(list()) -> {ok, pid()} | {error, any()}.
connect(Opts) ->
    %% [{python, python3}]
    python:start_link(lists:keydelete(ecpool_worker_id, 1, Opts)).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

name(#driver{name = Name}) ->
    Name.

-spec run_hook(atom(), list(), driver())
  -> ok
   | {ok, term()}
   | {error, term()}.
run_hook(Name, Args, Driver = #driver{hookspec = HookSpec, incfun = IncFun}) ->
    case maps:get(Name, HookSpec, null) of
        {M, F, Opts} ->
            case match_topic_filter(Name, proplists:get_value(topic, Args, null), maps:get(topics, Opts, [])) of
                true ->
                    IncFun(Name),
                    call(M, F, Args, Driver);
                _ -> ok
            end;
        _ -> ok
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

-spec call(atom(), atom(), list(), driver()) -> ok | {ok, term()} | {error, term()}.
call(Mod, Fun, Args, #driver{name = Name, type = Type, state = State}) ->
    with_pool(Name, fun(C) ->
        do_call(Type, C, Mod, Fun, Args ++ [State])
    end).

raw_call(Type, Name, Mod, Fun, Args) ->
     with_pool(Name, fun(C) ->
        do_call(Type, C, Mod, Fun, Args)
    end).

do_call(python, C, M, F, A) ->
    case catch python:call(C, M, F, convert_to_low_level(A)) of
        undefined -> ok;
        {_Ok = 0, Return} -> {ok, Return};
        {_Err = 1, Reason} -> {error, Reason};
        {'EXIT', Reason, Stk} ->
            ?LOG(error, "CALL python ~p:~p(~p), exception: ~p, stacktrace ~0p",
                        [M, F, A, Reason, Stk]),
            {error, Reason};
        _X ->
            ?LOG(error, "CALL python ~p:~p(~p), unknown return: ~0p",
                        [M, F, A, _X]),
            {error, unknown_return_format}
    end.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

with_pool(Name, Fun) ->
    ecpool:with_client(Name, Fun).

type(python3) -> python;
type(python2) -> python.

-compile({inline, [convert_to_low_level/1]}).
convert_to_low_level(Args) when is_map(Args) ->
    maps:to_list(Args);
convert_to_low_level(Args) when is_list(Args) ->
    Args.
