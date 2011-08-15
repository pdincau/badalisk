%%%-------------------------------------------------------------------
%%% File    : badalisk_disk_logger.erl
%%% Author  : Mirko Bonadei <mirko.bonadei@gmail.com>
%%% Description : 
%%%
%%% Created : 13 Aug 2011 by Mirko Bonadei <mirko.bonadei@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_disk_logger).

-behaviour(gen_event).
%% API
-export([add_handler/1, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {name, file}).

%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds the event handler to the error_logger
%%--------------------------------------------------------------------
add_handler([Pid]) ->
    error_logger:add_report_handler(?MODULE, [Pid]).

%%--------------------------------------------------------------------
%% Function: delete_hendler() -> 
%% Description: Deletes the event handler from the error_logger
%%--------------------------------------------------------------------
delete_handler() ->
    error_logger:delete_report_handler(?MODULE).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([Pid]) ->
    %% reading settings from the config file
    FileName = badalisk_conf:lookup(logfile),
    MaxNoBytes = badalisk_conf:lookup(logfile_size),
    MaxNoFiles = badalisk_conf:lookup(logfile_num),
    %% Maybe, in the future is it possible to select more options from config
    disk_log:open([
		   {name, ?MODULE},
		   {file, FileName},
		   {linkto, Pid},
		   {repair, true},
		   {type, wrap},
		   {format, internal},
		   {size, {MaxNoBytes, MaxNoFiles}},
		   {notify, false},
		   {mode, read_write}
		  ]),
    {ok, #state{name = ?MODULE, file = FileName}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event({error, _Gleader, {Pid, Format, {Pid, Module, Line}}}, State) ->
    %%ToLog = {error, date(), time(), {Pid, Format, Data}},
    ToLog = {error, date(), time(), io_lib:format(Format, [Pid, Module, Line])},
    disk_log:alog(State#state.name, ToLog),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, {Pid, Module, Line}}}, State) ->
    %%ToLog = {info_msg, date(), time(), {Pid, Format, Data}},
    ToLog = {info_msg, date(), time(), io_lib:format(Format, [Pid, Module, Line])},
    disk_log:alog(State#state.name, ToLog),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    disk_log:close(State#state.name).

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
