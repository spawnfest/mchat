-module(mchat_ws_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {username = null, interface = default_interface()}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, #state{}}.

stream({text, Data}, Req, State) ->
    case rjsonrpc2:decode(Data, State#state.interface) of
        {<<"ping">>, [], undefined} ->
            {ok, Req, State}; % keepalive
        {<<"login">>, Params, Id} ->
            {<<"username">>, Username} = lists:keyfind(<<"username">>, 1, Params),
            case mchat_server:is_valid_login(Username) of
                true ->
                    mchat_server:add_user(Username, self()),
                    R = {[{<<"success">>, true}]},
                    R1 = rjsonrpc2:encode(R, Id),
                    NewState = State#state{username=Username,
                                           interface=interface()},
                    {reply, R1, Req, NewState};
                false ->
                    R = {[{<<"success">>, false}]},
                    R1 = rjsonrpc2:encode(R, Id),
                    {reply, R1, Req, State}
            end;
        {<<"logout">>, [], _Id} ->
            mchat_server:delete_user(State#state.username, self()),
            NewState = State#state{username=null,
                                   interface=default_interface()},
            {ok, Req, NewState};
        {<<"getUsers">>, [], Id} ->
            L = mchat_server:get_users(),
            L1 = [{[{<<"username">>, X}, {<<"status">>, Y}]} || {X, Y} <- L],
            R = rjsonrpc2:encode(L1, Id),
            {reply, R, Req, State};
        {<<"sendMsg">>, Params, _Id} ->
            {<<"to">>, Username} = lists:keyfind(<<"to">>, 1, Params),
            {<<"msg">>, Msg} = lists:keyfind(<<"msg">>, 1, Params),
            case mchat_server:get_user_pid(Username) of
                [] -> io:format("Error: User ~p not online.~n", [Username]);
                Pid -> Pid!{msg, State#state.username, Msg}
            end,
            {ok, Req, State};
        {<<"sendFileRequest">>, Params, _Id} ->
            {<<"to">>, Username} = lists:keyfind(<<"to">>, 1, Params),
            {<<"fileName">>, FileName} = lists:keyfind(<<"fileName">>, 1, Params),
            {<<"fileType">>, FileType} = lists:keyfind(<<"fileType">>, 1, Params),
            {<<"fileSize">>, FileSize} = lists:keyfind(<<"fileSize">>, 1, Params),
            case mchat_server:get_user_pid(Username) of
                [] -> io:format("Error: User ~p not online.~n", [Username]);
                Pid -> Pid!{sendFileRequest, State#state.username,
                            {FileName, FileType, FileSize}}
            end,
            {ok, Req, State};
        {<<"sendFileReply">>, Params, _Id} ->
            {<<"to">>, Username} = lists:keyfind(<<"to">>, 1, Params),
            {<<"answer">>, Answer} = lists:keyfind(<<"answer">>, 1, Params),
            {<<"pid">>, FPid} = lists:keyfind(<<"pid">>, 1, Params),
            case mchat_server:get_user_pid(Username) of
                [] -> io:format("Error: User ~p not online.~n", [Username]);
                Pid -> Pid!{sendFileReply, State#state.username, Answer, FPid}
            end,
            {ok, Req, State};
        {error, Msg} ->
            {reply, jiffy:encode(Msg), Req, State} % Important
    end.

info({userStatus, Username, Status}, Req, State) ->
    R = {[{<<"username">>, Username},
          {<<"status">>, Status}]},
    R1 = rjsonrpc2:encode(R, <<"_userStatus">>),
    {reply, R1, Req, State};
info({msg, Username, Msg}, Req, State) ->
    R = {[{<<"from">>, Username},
          {<<"msg">>, Msg}]},
    R1 = rjsonrpc2:encode(R, <<"_sendMsg">>),
    {reply, R1, Req, State};
info({sendFileRequest, From, {FileName, FileType, FileSize}}, Req, State) ->
    R = {[{<<"from">>, From},
          {<<"fileName">>, FileName},
          {<<"fileSize">>, FileSize},
          {<<"fileType">>, FileType}]},
    R1 = rjsonrpc2:encode(R, <<"_sendFileRequest">>),
    {reply, R1, Req, State};
info({sendFileReply, From, Answer, Pid}, Req, State) ->
    R = {[{<<"from">>, From},
          {<<"answer">>, Answer},
          {<<"pid">>, Pid}]},
    R1 = rjsonrpc2:encode(R, <<"_sendFileReply">>),
    {reply, R1, Req, State};
info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, State) ->
    mchat_server:delete_user(State#state.username, self()),
    ok.

default_interface() ->
    [{<<"ping">>, [{params,[]}]},
     {<<"login">>, [{params,
                     [{<<"username">>, binary}]}]}].

interface() ->
    [{<<"ping">>, [{params,[]}]},
     {<<"login">>, [{params,
                     [{<<"username">>, binary}]}]},
     {<<"logout">>, [{params,[]}]},
     {<<"getUsers">>, [{params,[]}]},
     {<<"sendMsg">>, [{params,
                       [{<<"to">>, binary},
                        {<<"msg">>, binary}]}]},
     {<<"sendFileRequest">>, 
      [{params, [{<<"to">>, binary},
                 {<<"fileName">>, binary},
                 {<<"fileType">>, binary},
                 {<<"fileSize">>, integer}]}]},
     {<<"sendFileReply">>,
      [{params, [{<<"to">>, binary},
                 {<<"answer">>, boolean},
                 {<<"pid">>, binary}]}]}
    ].

