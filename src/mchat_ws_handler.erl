-module(mchat_ws_handler).
-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, undefined_state}.

stream(Data, Req, State) ->
    {reply, Data, Req, State}.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

