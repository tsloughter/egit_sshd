%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 2 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(egit_sshd_key_api).

-behavior(ssh_server_key_api).

-export([host_key/2
        ,is_auth_key/3]).

host_key('ssh-dss', _Options) ->
    decode_host_key(application:get_env(egit_sshd, dsa_private_key));

host_key('ssh-rsa', _Options) ->
    decode_host_key(application:get_env(egit_sshd, rsa_private_key)).

is_auth_key(_Key, User, _Options) ->
    lager:info("at=is_auth_key user=~s", [User]),
    true.

decode_host_key(undefined) ->
    undefined;
decode_host_key({ok, EncodedPrivateKey}) ->
    [PemEntry] = public_key:pem_decode(EncodedPrivateKey),
    PrivateKey = public_key:pem_entry_decode(PemEntry),
    {ok, PrivateKey}.
