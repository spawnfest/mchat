-module(mchat_upload_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {send_to_pid=null, interface=interface()}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, #state{}}.

stream({binary, Data}, Req, State) ->
    Pid = State#state.send_to_pid,
    Pid!{binary, Data},
    {ok, Req, State};
stream({text, Data}, Req, State) ->
    case rjsonrpc2:decode(Data, State#state.interface) of
        {<<"sendToPid">>, Params, _Id} ->
            {<<"pid">>, Pid} = lists:keyfind(<<"pid">>, 1, Params),
            Pid1 = list_to_pid(binary_to_list(Pid)),
            link(Pid1),
            NewState = State#state{send_to_pid=Pid1},
            Pid1!{uploaderPid, self()},
            R = {[{<<"gotDownloadPid">>, true}]},
            R1 = rjsonrpc2:encode(R, <<"_gotDownloadPid">>),
            {reply, R1, Req, NewState};
        {error, Msg} ->
            {reply, jiffy:encode(Msg), Req, State} % Important
    end.

info({continue, Answer}, Req, State) ->
    R = {[{<<"continue">>, Answer}]},
    R1 = rjsonrpc2:encode(R, <<"_continue">>),
    {reply, R1, Req, State};
info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

interface() ->
    [{<<"sendToPid">>, [{params,
                        [{<<"pid">>, binary}]}]}
    ].

