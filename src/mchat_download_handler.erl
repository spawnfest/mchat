-module(mchat_download_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {uploader_pid=null, interface=download_interface()}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, #state{}}.

stream({text, Data}, Req, State) ->
    case rjsonrpc2:decode(Data, State#state.interface) of
        {<<"getPid">>, _Params, Id} ->
            Pid = list_to_binary(pid_to_list(self())),
            R = {[{<<"pid">>, Pid}]},
            R1 = rjsonrpc2:encode(R, Id),
            {reply, R1, Req, State};
        {<<"continue">>, Params, _Id} ->
            {<<"answer">>, Answer} = lists:keyfind(<<"answer">>, 1, Params),
            Pid = State#state.uploader_pid,
            Pid!{continue, Answer},
            {ok, Req, State};
        {error, Msg} ->
            {reply, jiffy:encode(Msg), Req, State} % Important
    end.

info({binary, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
info({uploaderPid, Pid}, Req, State) ->
    NewState = State#state{uploader_pid=Pid},
    {ok, Req, NewState};
info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

download_interface() ->
    [{<<"getPid">>, [{params,[]}]},
     {<<"continue">>, 
      [{params, [{<<"answer">>, boolean}]}]}
    ].

