-module(mchat_upload_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {send_to_pid=null, interface=interface()}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, #state{}}.

stream({binary, _Data}, Req, State) ->
    io:format("~p~n", ["Data"]),
    {reply, <<"ok">>, Req, State};
stream({text, Data}, Req, State) ->
    case rjsonrpc2:decode(Data, State#state.interface) of
        {<<"sendToPid">>, Params, _Id} ->
            {<<"pid">>, Pid} = lists:keyfind(<<"pid">>, 1, Params),
            % TODO: For now assume valid pid
            %Pid1 = list_to_pid(binary_to_list(Pid)),
            %link(Pid1),
            {reply, <<"sendToPid=ok">>, Req, State#state{send_to_pid=Pid}};
        {error, Msg} ->
            {reply, jiffy:encode(Msg), Req, State} % Important
    end.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

interface() ->
    [{<<"sendToPid">>, [{params,
                        [{<<"pid">>, binary}]}]}
    ].

