%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 2 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(egit_sshd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Host} = application:get_env(egit_sshd, host),
    {ok, Port} = application:get_env(egit_sshd, port),
    {ok, RepoDir} = application:get_env(egit_sshd, repo_dir),
    egit_sshd_sup:start_link(Host, Port, RepoDir).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
