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

-module(emqx_extension_hook_app).

-behaviour(application).

-include("emqx_extension_hook.hrl").

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        , parse_hook_rules/1
        ]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_extension_hook_sup:start_link(),

    %% Load all dirvers
    load_all_drivers(),

    %% Register all hooks
    emqx_extension_hook_handler:load(),

    %% Register CLI
    emqx_ctl:register_command(exhook, {emqx_extension_hook_cli, cli}, []),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(exhook),
    unload_all_drivers(),
    emqx_extension_hook_handler:unload(),
    ok.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

load_all_drivers() ->
    load_all_drivers(application:get_env(?APP, drivers, [])).

load_all_drivers([]) ->
    ok;
load_all_drivers([{Name, Opts}|Drivers]) ->
    DeftHooks = parse_hook_rules(Name),
    ok = emqx_extension_hook:enable(Name, Opts, DeftHooks),
    load_all_drivers(Drivers).

unload_all_drivers() ->
    emqx_extension_hook:disable_all().

%% @doc Parse Hookspec from configuratin JSON string
parse_hook_rules(Name) ->
    RawHooks = proplists:get_value(Name,
                 application:get_env(?APP, hooks, []), #{}),

    ParseOpts = fun(#{<<"topics">> := Topics}) when is_list(Topics) ->
                       #{topics => Topics};
                   (_) -> #{}
                end,

    maps:map(fun(_, Spec) ->
        #{<<"module">> := Module,
          <<"callback">> := Function} = E = emqx_json:decode(Spec, [return_maps]),
        {Module, Function, ParseOpts(E)}
    end, RawHooks).
