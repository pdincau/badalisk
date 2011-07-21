%%%-------------------------------------------------------------------
%%% File    : badalisk_sup.erl
%%% Author  : Paolo D'Incau <paolo.dincau@gmail.com>
%%% Description : 
%%%
%%% Created : 16 Apr 2011 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    BadaliskConf = {badalisk_conf, {badalisk_conf, start_link,[]},
		    permanent, 2000, worker, [badalisk_conf]},
    BadaliskServer = {badalisk_server, {badalisk_server, start_link,[]},
		     permanent, 2000, worker, [badalisk_server]},
    {ok,{{one_for_all,0,1}, [BadaliskConf, BadaliskServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
