-module(mchat_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(mchat).

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    mchat_sup:start_link().

stop(_State) ->
    ok.
