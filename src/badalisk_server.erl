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

-record(acceptor, {pid,
		   listen_socket,
		   socket_mode,
		   port}).

-record(state, {acceptors}).

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
    % Load the badalisk_logger module
    badalisk_loglevel:set(badalisk_conf:lookup(loglevel)),
    %% adding the disk_logger handler or creating the text file
    case badalisk_conf:lookup(logmode) of
	disk_log ->
	    badalisk_disk_logger:add_handler([self()]);
	file ->
	    add_file_logger()
    end,
    case gen_tcp:listen(?PORT, [binary, 
				{packet, http},
				{reuseaddr, true},
				{active, false},
				{backlog, 30}]) of
	{ok, Listen} ->
	    Pid = badalisk_socket:start_link(Listen, ?PORT),
	    Acceptor = #acceptor{pid = Pid,
				 listen_socket = Listen,
				 port = ?PORT,
				 socket_mode = http},
	    ?INFO_MSG("badalisk_server started", []),
	    {ok, #state{acceptors = [Acceptor]}};
	{error, Reason} ->
	    ?CRITICAL_MSG("Error during StartUp of badalisk_server", []),
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
handle_call(status, _From, #state{acceptors = Acceptors} = State) ->
    Reply = Acceptors,
    {reply, Reply, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, Pid}, #state{acceptors = Acceptors} = State) ->
    OldAcceptor = lists:keyfind(Pid, #acceptor.pid, Acceptors),
    NewPid = badalisk_socket:start_link(OldAcceptor#acceptor.listen_socket, OldAcceptor#acceptor.port),   
    NewAcceptor = OldAcceptor#acceptor{pid = NewPid},
    error_logger:info_msg("Acceptor pids are: ~p.~n", [[NewAcceptor|Acceptors]]),
    {noreply, State#state{acceptors = [NewAcceptor|Acceptors]}};

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
handle_info({'EXIT', Pid, normal}, #state{acceptors = Acceptors} = State) ->
    error_logger:info_msg("Acceptor ~p: exited normally.~n", [Pid]),
    NewState = State#state{acceptors = lists:keydelete(Pid, #acceptor.pid, Acceptors)},
    error_logger:info_msg("Current acceptors are: ~p~n", [NewState]),
    {noreply, NewState};

handle_info({'EXIT', Pid, _Abnormal}, #state{acceptors = Acceptors} = State) ->
    error_logger:info_msg("Acceptor ~p: exited abnormally.~n", [Pid]),
    OldAcceptor = lists:keyfind(Pid, #acceptor.pid, Acceptors),
    SameTypeAcceptors = [Acceptor || Acceptor <- Acceptors, Acceptor#acceptor.socket_mode =:= OldAcceptor#acceptor.socket_mode],
    NewState = case lists:flatlength(SameTypeAcceptors) of
		   0 ->
		       NewPid = badalisk_socket:start_link(OldAcceptor#acceptor.listen_socket, OldAcceptor#acceptor.port),
		       NewAcceptor = OldAcceptor#acceptor{pid = NewPid},
		       State#state{acceptors = [NewAcceptor|lists:keydelete(Pid, #acceptor.pid, Acceptors)]};
		   _ ->
		       State#state{acceptors = lists:keydelete(Pid, #acceptor.pid, Acceptors)}
	       end,
    error_logger:info_msg("Current acceptors are: ~p~n", [NewState]),
    {noreply, NewState};

handle_info(Info, State) ->
    error_logger:info_msg("Info received: (~p).~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    [gen_tcp:close(Acceptor#acceptor.listen_socket) || Acceptor <- State#state.acceptors],
    case badalisk_conf:lookup(logmode) of
	disk_log ->
	    badalisk_disk_logger:delete_handler(),
	    ok;
	_ -> ok
    end.

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

