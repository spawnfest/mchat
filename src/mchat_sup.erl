
-module(mchat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
   Dispatch = [{'_', [{[<<"mchat-api">>], bullet_handler, 
                        [{handler, mchat_ws_handler}]},
                       {[<<"upload">>], bullet_handler, 
                        [{handler, mchat_upload_handler}]},
                       {[], cowboy_http_static,
                        [{directory, {priv_dir, ?MODULE, [<<"mchat">>]}},
                         {file, <<"index.html">>},
                         {mimetypes, [{<<".html">>, [<<"text/html">>]}]}]},
                       {['...'], cowboy_http_static,
                        [{directory, {priv_dir, ?MODULE, [<<"mchat">>]}},
                         {mimetypes,
                          [{<<".css">> , [<<"text/css">>]},
                           {<<".png">> , [<<"image/png">>]},
                           {<<".jpg">> , [<<"image/jpeg">>]},
                           {<<".jpeg">>, [<<"image/jpeg">>]},
                           {<<".js">>  , [<<"application/javascript">>]}]}]}
                      ]}
               ],
    Port = mchat_utils:confval(mchat, port, 8080),
    WSPool = mchat_utils:confval(mchat, ws_pool_size, 100),
    CowboySpec = cowboy:child_spec(
            my_http_listener, WSPool,
            cowboy_tcp_transport, [{port, Port}],
            cowboy_http_protocol, [{dispatch, Dispatch}]),
    WSServerSpec = ?CHILD(mchat_server, worker),
    ChildSpecs = [CowboySpec, WSServerSpec],
    RestartStrategy = {one_for_all, 3, 30},
    {ok, {RestartStrategy, ChildSpecs}}.

