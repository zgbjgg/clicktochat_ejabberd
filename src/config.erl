%% -------------------------------------------------------------------
%% config : get config for clicktochat
%% Copyright (c) 2012 All Rights Reserved.
%% Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%% -------------------------------------------------------------------

% module config
-module(config).

% exports
-export([host_client/0, host_helpdesk/0, max_sessions/0]).

% include files
-include("clicktochat.hrl").

%% @doc Loads the configuration file and retrieve the list
%% @spec load(File :: string()) -> list()
-spec load(File :: string()) -> list(). 
load(File) ->
    case file:consult(File) of
        {ok, List} -> List;
	_          -> []
    end.

%% @doc Retrieves the host_client value in config file
%% @spec host_client() -> term()
-spec host_client() -> term().
host_client() ->
    proplists:get_value(host_client, load(?config_file)).

%% @doc Retrieves the host_helpdesk value in config file
%% @spec host_helpdesk() -> term()
-spec host_helpdesk() -> term().
host_helpdesk() ->
    proplists:get_value(host_helpdesk, load(?config_file)).

%% @doc Retrieves the max_sessions value in config file
%% @spec max_sessions() -> term()
-spec max_sessions() -> term().
max_sessions() ->
    proplists:get_value(max_sessions, load(?config_file)).  
