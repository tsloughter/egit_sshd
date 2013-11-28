%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 2 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(egit_sshd_cb).

-behaviour(ssh_daemon_channel).

%% ssh_channel callbacks
-export([init/1
        ,handle_ssh_msg/2
        ,handle_msg/2
        ,terminate/2]).

%% state
-record(state, {cm
               ,channel
               ,port
               ,repo_dir
               ,repo
               ,cmd
               }).

%%====================================================================
%% ssh_channel callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%%
%% Description: Initiates the CLI
%%--------------------------------------------------------------------
init([RepoDir]) ->
    {ok, #state{repo_dir=RepoDir}}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _ConnectionManager ,{data, _ChannelId, _Type, Data}}
               ,State=#state{port=Port}) ->
    Port ! {self(), {command, Data}},
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionManager
               ,{env, ChannelId, WantReply, _Var, _Value}}
              ,State) ->
    ssh_connection:reply_request(ConnectionManager,
                                 WantReply, failure, ChannelId),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionManager
               ,{exec, ChannelId, _WantReply, Cmd="git-receive-pack "++_}}
              ,State=#state{repo_dir=RepoDir}) ->
    [_, R] = string:tokens(Cmd, " "),
    Repo = string:strip(R, both, $'),
    Port = open_port({spawn, Cmd}, [{cd, RepoDir}, exit_status]),
    {ok, State#state{channel = ChannelId
                    ,cm = ConnectionManager
                    ,port=Port
                    ,cmd=push
                    ,repo=Repo}};
handle_ssh_msg({ssh_cm, ConnectionManager
               ,{exec, ChannelId, _WantReply, Cmd="git-upload-pack "++_}}
              ,State=#state{repo_dir=RepoDir}) ->
    Port = open_port({spawn, Cmd}, [{cd, RepoDir}, exit_status]),
    {ok, State#state{channel = ChannelId
                    ,cm = ConnectionManager
                    ,port=Port}};
handle_ssh_msg({ssh_cm, ConnectionManager
               ,{exec, ChannelId, _WantReply, Cmd}}
              ,State) ->
    {ok, User} = ssh_userreg:lookup_user(ConnectionManager),
    lager:error("at=handle_ssh_msg error=attempt_to_run_unknown_command user=~s cmd=~s~n", [User, Cmd]),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, ChannelId}}
                   ,State=#state{port = _Port}) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    lager:error("Connection closed by peer ~n Error ~p~n", [Error]),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    lager:error("Connection closed by peer ~n Status ~p~n", [Status]),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%
%% Description: Handles other channel messages.
%%--------------------------------------------------------------------

handle_msg({ssh_channel_up, _ChanneId, _ConnectionManager}, State) ->
    {ok, State};
handle_msg({'EXIT', Port, _Reason}, #state{port = Port,
                                           channel = ChannelId} = State) ->
    {ok, State};
handle_msg({Port, {exit_status, Status}}, State=#state{port = Port
                                                      ,channel = ChannelId
                                                      ,cm=ConnectionManager
                                                      ,repo=_Repo
                                                      ,cmd=_Cmd}) ->
    ssh_connection:reply_request(ConnectionManager, true,
                                 success, ChannelId),
    ssh_connection:exit_status(ConnectionManager, ChannelId, Status),
    {ok, State};
handle_msg({Port, {data, Data}}, State=#state{port=Port
                                             ,cm=ConnectionManager
                                             ,channel=ChannelId}) ->
    ssh_connection:send(ConnectionManager, ChannelId, 0, Data),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: Called when the channel process is trminated
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
