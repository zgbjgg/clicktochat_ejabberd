%% -------------------------------------------------------------------
%% mod_clicktochat : manage the users activity on clicktochat
%% Copyright (c) 2012 All Rights Reserved.
%% Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%% -------------------------------------------------------------------

% module click to chat
-module(mod_clicktochat).

% behaviors
-behavior(gen_server).
-behavior(gen_mod).

% gen mod callbacks
-export([start/2, stop/1, start_link/2]).

% gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% api
-export([get_queue/1, get_queue_busy/1, user_online/2, user_offline/2, request_user/2,
	 remove_user/2]).

% include files
-include("ejabberd.hrl").
-include("clicktochat.hrl").

% records (state for my gen)
-record(state, {host, queue, queue_busy}).


%%====================================================================
%% gen_server callbacks
%%====================================================================


init([Host, _Opts]) ->
    ejabberd_hooks:add(user_available_hook, Host, hooksI, on_user_available, 0),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, hooksI, on_sm_remove_connection, 1),
    ejabberd_hooks:add(user_send_packet, Host, hooksI, on_user_send_packet, 2),
    {ok, #state{host = Host, queue = [], queue_busy = []}}.

handle_call({user_online, JID}, _From, #state{host=Host, queue=Queue, 
					      queue_busy=QueueBusy})  ->
    NewQueue = case config:host_helpdesk() of
	           Host ->
		       [ {JID, config:max_sessions()} ] ++ Queue;
		   _    ->
		       [ JID ] ++ Queue
               end,
    {reply, ok, #state{host=Host, queue = NewQueue, 
		       queue_busy = QueueBusy}};
handle_call({user_offline, JID}, _From, #state{host=Host, queue=Queue, 
					       queue_busy=QueueBusy}) ->
    NewQueue = case config:host_helpdesk() of
	           Host ->
		       [ {JabberId, Ms} || {JabberId, Ms} <- Queue, JabberId =/= JID ];
		   _    ->
		       Queue -- [ JID ]
               end,
    {reply, ok, #state{host=Host, queue = NewQueue, 
		       queue_busy = QueueBusy}};
handle_call(get_queue, _From, State=#state{host=_Host, queue=Queue, 
				           queue_busy=_QueueBusy})    ->
    {reply, {ok, Queue}, State};
handle_call(get_queue_busy, _From, State=#state{host=_Host, queue=_Queue, 
				                queue_busy=QueueBusy})    ->
    {reply, {ok, QueueBusy}, State};
handle_call({request_user, JID}, _From, State = #state{host=Host, queue=[{User, Ms}|Queue], 
						       queue_busy=QueueBusy})  ->
    {Reply, NewState} = case config:host_helpdesk() of 
	     		    Host ->
				NewQueue = case Ms > 0 of
						true  ->
						    Queue ++ [{User, Ms - 1}];
						false ->
						    Queue
					   end,
		    	        {{ok, User}, #state{host=Host, 
						    queue=NewQueue, 
						    queue_busy=QueueBusy ++ [{JID, User}]}};
			    _    ->
			        {{ok, none}, State}
		         end,
    {reply, Reply, NewState};
handle_call(stop, _From, State)                                         ->
    {stop, normal, ok, State}.

handle_cast({remove_user, JID}, State = #state{host=Host, queue=Queue, 
					       queue_busy=QueueBusy}) ->
    NewState = case config:host_helpdesk() of
		   Host ->
		       [{JID, User}] = [ X || {JabberId, _}=X <- QueueBusy, JabberId =:= JID ],
		       #state{host=Host, 
			      queue=Queue ++ case [ {User, Ms + 1} || {U, Ms} <- Queue, U =:= User ] of
					         [] ->
						     [{User, 1}];
						 Q  ->
						     Q
					     end, 
			      queue_busy=QueueBusy -- [{JID, User}]};
		   _    ->
		       State
		end,
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{host=Host, queue=_Queue, queue_busy=_QueueBusy}) ->
    ejabberd_hooks:delete(user_available_hook, Host, hooksI, on_user_available_hook, 0),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, hooksI, on_sm_remove_connection_hook, 1),
    ejabberd_hooks:delete(user_send_packet, Host, hooksI, on_user_send_packet, 2),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API
%%====================================================================

user_online(Host, JID) ->
    gen_server:call(?get_proc(Host, ?MODULE), {user_online, JID}).

user_offline(Host, JID) ->
    gen_server:call(?get_proc(Host, ?MODULE), {user_offline, JID}).
   
get_queue(Host) ->
    gen_server:call(?get_proc(Host, ?MODULE), get_queue).

get_queue_busy(Host) ->
    gen_server:call(?get_proc(Host, ?MODULE), get_queue_busy).

request_user(Host, JID) ->
    gen_server:call(?get_proc(Host, ?MODULE), {request_user, JID}).

remove_user(Host, JID) ->
    gen_server:cast(?get_proc(Host, ?MODULE), {remove_user, JID}).

start_link(Host, Opts) ->
    gen_server:start_link({local, ?get_proc(Host, ?MODULE)}, ?MODULE, [Host, Opts], []).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    Child = {?get_proc(Host, ?MODULE), {?MODULE, start_link, [Host, Opts]}, permanent,
	     1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, Child),
    ?INFO_MSG("starting on ~p ~n", [self()]), 
    ok.

stop(Host) ->
    gen_server:call(?get_proc(Host, ?MODULE), stop),
    supervisor:delete_child(ejabberd_sup, ?get_proc(Host, ?MODULE)),
    ?INFO_MSG("stopping on ~p ~n", [self()]),
    ok. 
