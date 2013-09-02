%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 2 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(egit_sshd_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Host, Port, RepoDir) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Host, Port, RepoDir]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Host, Port, RepoDir]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = [{egit_sshd, {ssh, daemon, [Host, Port
                                            ,[{key_cb, egit_sshd_key_api}
                                             ,{ssh_cli, {egit_sshd_cb, [RepoDir]}}]]},
                   permanent, 2000, supervisor, []}],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
