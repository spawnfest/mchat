-module(mchat_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_user/2, delete_user/2,
         get_user_pid/1, get_users/0, is_valid_login/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_ID, mchat_users).

-record(state, {}).
-record(user, {username, pid, status}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_user(Username, Pid) ->
    gen_server:cast(?SERVER, {add_user, Username, Pid}).

delete_user(Username, Pid) ->
    gen_server:cast(?SERVER, {delete_user, Username, Pid}).

get_user_pid(Username) ->
    case ets:lookup(?TABLE_ID, Username) of
        []     -> [];
        [User] -> User#user.pid
    end.

get_users() ->
    L = ets:tab2list(?TABLE_ID),
    [{X#user.username, X#user.status} || X <- L].

is_valid_login(Username) ->
    case ets:lookup(?TABLE_ID, Username) of
        []     -> true;
        [_User] -> false
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE_ID, [{keypos, 2},
                        named_table,
                        {read_concurrency, true}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_user, Username, Pid}, State) ->
    link(Pid),
    case ets:lookup(?TABLE_ID, Username) of
        [] ->
            ets:insert(?TABLE_ID,
                         #user{username = Username, 
                               pid = Pid, 
                               status = <<"online">>}),
            multicast({userStatus, Username, <<"online">>});
        [_User] -> % already there so do nothing?
            ok
    end,
    {noreply, State};
handle_cast({delete_user, Username, Pid}, State) ->
    unlink(Pid),
    ets:delete(?TABLE_ID, Username),
    multicast({userStatus, Username, <<"offline">>}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    % TODO take not of pid if reason is something other
    % than normal
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?TABLE_ID),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
multicast(Msg) ->
    L = ets:tab2list(?TABLE_ID),
    Pids = [X#user.pid || X <- L],
    [Pid!Msg || Pid <- Pids].

