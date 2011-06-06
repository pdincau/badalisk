%%%-------------------------------------------------------------------
%%% File    : badalisk_server.erl
%%% Author  : Paolo  <paolo.dincau@gmail.com>
%%% Description : 
%%%
%%% Created : 27 Apr 2011 by Paolo  <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/1]).

-record(state, {listen_socket,
		port,
		acceptor}).

-include("../include/badalisk.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Port) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    add_file_logger(),
    case gen_tcp:listen(?PORT, [binary, 
				{packet, http},
				{reuseaddr, true},
				{active, false},
				{backlog, 30}]) of
	{ok, Listen} ->
	    Pid = badalisk_socket:start_link(Listen, ?PORT),
	    {ok, #state{listen_socket = Listen,
			port = ?PORT, 
			acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, _Pid}, #state{listen_socket = ListenSocket} = State) ->
    NewPid = badalisk_socket:start_link(ListenSocket, State#state.port),
    {noreply, State#state{acceptor = NewPid}};

handle_cast(stop, State) ->
        {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, #state{acceptor = Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor = Pid} = State) ->
    timer:sleep(2000),
    badalisk_socket:start_link(State#state.listen_socket, State#state.port),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
create(Pid) ->
    gen_server:cast(?SERVER, {create, Pid}).

%--------------------------------------------------------------------
%% Function: add_file_logger() ->
%% Description: Add file logger
%%              
%%--------------------------------------------------------------------
add_file_logger() ->
    {{Y,M,D},{H,Min,S}} = erlang:localtime(),
    LogFile = lists:concat([?LOGFILE, '-', Y, ':', M, ':', D, '-', H, ':', Min, ':', S, ".log"]),
    ok = error_logger:logfile({open, LogFile}).

