%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc push_erlang.

-module(push_erlang).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the push_erlang server.
start() ->
    push_erlang_deps:ensure(),
    ensure_started(crypto),
    application:start(push_erlang).


%% @spec stop() -> ok
%% @doc Stop the push_erlang server.
stop() ->
    application:stop(push_erlang).
