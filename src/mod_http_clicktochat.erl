%% -------------------------------------------------------------------
%% mod_http_clicktochat : http interface for clicktochat
%% Copyright (c) 2012 All Rights Reserved.
%% Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%% -------------------------------------------------------------------

% module http click to chat
-module(mod_http_clicktochat).

% behavior
-behavior(gen_mod).

% gen_mod callbacks
-export([start/2, stop/1, process/2]).

% include files and lib
-include("clicktochat.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include_lib("xmerl/include/xmerl.hrl").

% types
-type jid() :: {jid, Username :: string(), Host :: string(), HostName :: string(),
                Username :: string(), Host :: string(), HostName :: string()}.

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(_Host, _Opts) ->
    ?INFO_MSG("starting on ~p ~n", [self()]),
    ok.

stop(_Host) ->
    ?INFO_MSG("stopping on ~p ~n", [self()]),
    ok.

process(["register"], {request, 'POST', _, _, _, _, _, Req, _, _, _, _, _}) ->
    [ Username, Password ] = parse_req(Req, ["username", "password"]),
    case ejabberd_auth:try_register(Username, config:host_client(), Password) of
	{atomic, ok}     ->
            From = get_jid({Username, config:host_client()}),
	    {ok, To} = mod_clicktochat:request_user(config:host_helpdesk(), From),
	    {201, ?content_type("text/xml"), on_user_registered(From, get_jid(To))};
	{atomic, exists} ->
	    {400, [], "exists"};
	{error, not_allowed} ->
	    {400, [], "not allowed"}
    end;
process(["list"], _Req) ->
    case ejabberd_commands:execute_command(connected_users, []) of
	{error, Error} ->
	    {400, [], Error};
	Result ->
 	    {200, ?content_type("text/xml"), {xmlelement, <<"connected_users">>, [], 
					      on_list_connected_users(Result)}}
    end;    
process(_Path, _Req)         						   ->
    {404, [], ""}.

%%====================================================================
%% Private API
%%====================================================================

%% @doc Parses the request, extract the given tags
%% @spec parse_req(Req :: string(), RequiredTags :: list()) -> list()
-spec parse_req(Req :: string(), RequiredTags :: list()) -> list().
parse_req(Req, RequiredTags) ->
    {Tags, _} = xmerl_scan:string(Req),
    [ begin 
	[ Element ] = xmerl_xpath:string("//" ++ Tag, Tags),
	[ Text ] = Element#xmlElement.content,
        Text#xmlText.value
      end || Tag <- RequiredTags ].  

%% @doc Gets the username as 'user@host'
%% @spec get_jid(JID :: jid() | {Username :: string(), Host :: string()}) -> string()
-spec get_jid(JID :: jid() | {Username :: string(), Host :: string()}) -> string().
get_jid({jid, Username, Host, _, _, _, _}) ->
    get_jid({Username, Host});
get_jid({Username, Host})                  ->
    Username ++ "@" ++ Host. 

%% @doc Retrieves a list containing the from/to users, to be taken as xml
%% @spec on_user_registered(From :: string(), To :: string()) -> list()
-spec on_user_registered(From :: string(), To :: string()) -> list().
on_user_registered(From, To) ->
    MyTags = [{<<"status">>, <<"ok">>}, {<<"from">>, From}, 
	      {<<"to">>, To}],
    {xmlelement, <<"register">>, [], 
    [{xmlelement, Tag, [], [{xmlcdata, Value}]} || {Tag, Value} <- MyTags]}.

%% @Retrieves a list containing all users connected, taken as xml
%% @spec on_list_connected_users(Users :: list()) -> list()
-spec on_list_connected_users(Users :: list()) -> list().
on_list_connected_users([])                               -> [];
on_list_connected_users([ [User, _, Host, _, _] | Rest ]) ->
    [{xmlelement, <<"username">>, [], [{xmlcdata, User ++ "@" ++ Host}]} 
     | on_list_connected_users(Rest) ]. 
