-module(mchat_ws_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {username, interface = default_interface()}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, #state{}}.

stream({text, Data}, Req, State) ->
    case rjsonrpc2:decode(Data, State#state.interface) of
        {<<"ping">>, [], undefined} ->
            {ok, Req, State}; % keepalive
        {<<"login">>, Params, Id} ->
            Username = lists:keyfind(<<"username">>, 1, Params),
            case mchat_server:is_valid_login(Username) of
                true ->
                    R = {[{<<"success">>, false}]},
                    R1 = rjsonrpc2:encode(R, Id),
                    NewState = State#state{username=Username,
                                           interface=interface()},
                    {reply, R1, Req, NewState};
                false ->
                    R = {[{<<"success">>, false}]},
                    R1 = rjsonrpc2:encode(R, Id),
                    {reply, R1, Req, State}
         end
    end.
info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

default_interface() ->
    [{<<"ping">>, [{params,[]}]},
     {<<"login">>, [{params,
                     [{<<"username">>, binary}]}]}].

interface() ->
    [{<<"ping">>, [{params,[]}]},
     {<<"login">>, [{params,
                     [{<<"username">>, binary}]}]},
     {<<"getUsers">>, [{params,[]}]}
    ].

