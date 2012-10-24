%% -------------------------------------------------------------------
%% hooksI : hooks interface for clicktochat
%% Copyright (c) 2012 All Rights Reserved.
%% Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%% -------------------------------------------------------------------

% module hooksI 
-module(hooksI).

% events ejabberd (hooks)
-export([on_user_available/1, on_sm_remove_connection/3,
	 on_user_send_packet/3]).

% include files
-include("ejabberd.hrl").

% types
-type jid() :: {jid, Username :: string(), Host :: string(), HostName :: string(),
                Username :: string(), Host :: string(), HostName :: string()}.

%% @doc When a user is available (came online), put in queue
%% @spec on_user_available(JID :: jid()) -> ok
-spec on_user_available(JID :: jid()) -> ok.
on_user_available(JID = {jid, _, Host, _, _, _, _}) ->
    ok = mod_clicktochat:user_online(Host, JID),
    ok.

%% @doc When a user is disconnected (came offline), remove from the queue
%% @spec on_sm_remove_connection(_SID :: tuple(), JID :: jid(), _SessionInfo :: any()) -> ok
-spec on_sm_remove_connection(_SID :: tuple(), JID :: jid(), _SessionInfo :: any()) -> ok.
on_sm_remove_connection(_SID, JID = {jid, Username, Host, _, _, _, _}, _SessionInfo) ->
    ok = mod_clicktochat:user_offline(Host, JID),
    ok = case config:host_client() of
	     Host ->
		ok = ejabberd_auth:remove_user(Username, Host),
    		mod_clicktochat:remove_user(config:host_helpdesk(), Username ++ "@" ++ Host);
	     _    ->
		ok
	  end,
    ok.

%% @doc When a user sends a packet
%% @spec on_user_send_packet(From :: jid(), To :: jid(), Packet :: tuple()) -> ok
-spec on_user_send_packet(From :: jid(), To :: jid(), Packet :: tuple()) -> ok.
on_user_send_packet(_From, _To, Packet) ->
    {ok, _QueueBusy} = mod_clicktochat:get_queue_busy(config:host_helpdesk()),
    % case [ User || {JabberID, User} <- QueueBusy, JabberID =:= From ] of
    %	[]     -> 
    %	    none;
    %	[ To ] ->
    	    case Packet of
		{xmlelement,"message",_, [_, {xmlelement, "body", _, [{xmlcdata, Data}|_]}|_] } ->
		    ?INFO_MSG("on : ~p =  received : ~p ~n", [self(), Data]);
		_                                                                           ->
		    none
	    end,
    % end,
    ok.
