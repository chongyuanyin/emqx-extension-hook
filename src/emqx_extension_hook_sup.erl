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

-module(emqx_extension_hook_sup).

-behaviour(supervisor).

-export([ start_link/0
        , init/1
        ]).

-export([ start_driver_pool/1
        , stop_driver_pool/1
        , put_drivers/1
        , rm_drivers/1
        , drivers/0
        ]).

%%--------------------------------------------------------------------
%%  Supervisor APIs & Callbacks
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_driver_pool(Specs) when is_list(Specs) ->
    [start_driver_pool(Spec) || Spec <- Specs];

start_driver_pool(Spec = #{id := Name}) ->
    {ok, _} = supervisor:start_child(?MODULE, Spec),
    put_drivers(Name).

stop_driver_pool(Name) ->
    ok = supervisor:terminate_child(?MODULE, Name),
    ok = supervisor:delete_child(?MODULE, Name),
    rm_drivers(Name).

put_drivers(Name) ->
    Saved = persistent_term:get(?MODULE, []),
    persistent_term:put(?MODULE, lists:reverse([Name | Saved])).

rm_drivers(Name) ->
    Saved = persistent_term:get(?MODULE, []),
    persistent_term:put(?MODULE, lists:delete(Name, Saved)).

drivers() ->
    persistent_term:get(?MODULE, []).

%%--------------------------------------------------------------------
%%  Callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 10, 100}, []}}.

