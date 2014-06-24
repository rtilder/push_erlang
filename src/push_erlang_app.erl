%% @author Mochi Media <dev@mochimedia.com>
%% @copyright push_erlang Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the push_erlang application.

-module(push_erlang_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for push_erlang.
start(_Type, _StartArgs) ->
    push_erlang_deps:ensure(),
    push_erlang_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for push_erlang.
stop(_State) ->
    ok.
