%%%-------------------------------------------------------------------
%%% File    : badalisk_loglevel.erl
%%% Author  : Mirko Bonadei <mirko.bonadei@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Aug 2011 by Mirko Bonadei <mirko.bonadei@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_loglevel).
-author("mirko.bonadei@gmail.com").
-export([get/0, set/1]).

-record(loglevel, {ordinal, name, description}).

-include("../include/badalisk.hrl").

-define(LOG_MODULE, "error_logger").
-define(LOG_LEVELS, [
		     #loglevel{ordinal = 0, name = no_log, description = "No Logs (not reccomended)"},
		     #loglevel{ordinal = 1, name = critical, description = "Only critical logs"},
		     #loglevel{ordinal = 2, name = error, description = "Only errors and critical"},
		     #loglevel{ordinal = 3, name = warning, description = "Warnings, errors and critical"},
		     #loglevel{ordinal = 4, name = info, description = "Infos, warnings, errors and critical"},
		     #loglevel{ordinal = 5, name = debug, description = "Debug mode (developer only)"}
		    ]).
%%==============================================================================================
%% Function:
%% Description:
%%==============================================================================================
get() ->
    Level = badalisk_logger:get(),
    case lists:keysearch(Level, #loglevel.ordinal, ?LOG_LEVELS) of
	{value, Result} ->
	    {Result#loglevel.ordinal, Result#loglevel.name, Result#loglevel.description};
	_ ->
	    erlang:error(no_such_level, Level)
    end.

%%===============================================================================================
%% Function:
%% Description:
%%===============================================================================================
set(NewLevel) when is_atom(NewLevel) ->
    set(level_to_integer(NewLevel));
set(NewLevel) when is_integer(NewLevel) ->
    try
	{Module, Code} = dynamic_compile:from_string(badalisk_logger_src(NewLevel)),
	code:load_binary(Module, ?LOG_MODULE ++ ".erl", Code)
    catch
	Type:Error ->
	    ?CRITICAL_MSG("Error compiling logger (~p): ~p", [Type, Error])
    end;
set(_) ->
    exit("Log Level must be an integer").
    
%%================================================================================================
%% Function:
%% Description:
%%================================================================================================
level_to_integer(Level) when is_integer(Level) ->
    Level;
level_to_integer({Module, Level}) ->
    {Module, level_to_integer(Level)};
level_to_integer(Level) ->
    case lists:keysearch(Level, #loglevel.ordinal, ?LOG_LEVELS) of
	{value, #loglevel{ordinal = IntLevel}} ->
	    IntLevel;
	_ ->
	    erlang:error({no_such_level, Level})
    end.

%%-------------------------------------------------------------------------------------------------
%% badalisk_logger code. I decided to chose the way of logging I have seen in ejabberd project.
%% Dinamically compiliing the module, we can reach a great optimization between logging verbosity
%% switch.
%%-------------------------------------------------------------------------------------------------
badalisk_logger_src(LogLevel) ->
    L = integer_to_list(LogLevel),
    "
    -module(badalisk_logger).
    -author('mirko.bonadei@gmail.com').
    
    -export([debug_msg/4, info_msg/4, warning_msg/4, error_msg/4, critical_msg/4, get/0]).

    get() ->
        " ++ L ++".
    
    %% Helper FUnctions

    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
