-module(mchat_utils).

-export([priv_dir/1, confval/3]).

-define(PRIV_DIR, priv_dir(?MODULE)).

priv_dir(ModuleName) ->
    ModPath = code:which(ModuleName),
    AppPath = filename:dirname(filename:dirname(ModPath)),
    filename:join(AppPath, "priv").

confval(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

