%%%-------------------------------------------------------------------
%%% File    : badalisk_socket.erl
%%% Author  : Paolo  <paolo.dincau@gmail.com>
%%% Description : 
%%%
%%% Created : 27 Apr 2011 by Paolo  <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_socket).

-export([start_link/2]).

-export([init/1]).

-include("../include/badalisk.hrl").

-record(c, {sock,
	    port,
	    peer_addr,
	    peer_port
	   }).

start_link(ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, [{ListenSocket, ListenPort}]).

%%--------------------------------------------------------------------
%% Function: init({ListenSocket, ListenPort}) ->
%% Description: accept connections on socket
%%              
%%--------------------------------------------------------------------
init({ListenSocket, ListenPort}) ->
    case catch gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    badalisk_server:create(self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
	    error_logger:info_msg("(~p: ~p) : Accepted connection {~p,~p}~n", [self(), ?MODULE, Addr, Port]),
	    C = #c{sock = Socket,
                   port = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port},
	    request(C, #req{});
	Error ->
	    error_logger:error_report([{application, badalisk}, "Accept failed error", io_lib:format("~p",[Error])]),
	    exit({error, accept_failed})
    end.

%%--------------------------------------------------------------------
%% Function: request(C, Req) ->
%% Description: receive http request from socket
%%              
%%--------------------------------------------------------------------
request(C, Req) ->
    case gen_tcp:recv(C#c.sock, 0, ?TIMEOUT) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(C, Req#req{vsn = Version, method = Method, uri = Path}, []);
        {error, {http_error, "\r\n"}} ->
	    request(C, Req);
	{error, {http_error, "\n"}} ->
            request(C, Req);
	_Other ->
	    exit(normal)
    end.

%%--------------------------------------------------------------------
%% Function: headers(C, Req, Headers) ->
%% Description: get headers of given request
%%              
%%--------------------------------------------------------------------
headers(C, Req, Headers) ->
    Received = gen_tcp:recv(C#c.sock, 0, ?TIMEOUT),
    case Received of
        {ok, {http_header, _, 'Content-Length', _, Val}} ->
            Len = list_to_integer(Val),
            headers(C, Req#req{content_length = Len}, [{'Content-Length', Len}|Headers]);
        {ok, {http_header, _, 'Connection', _, Val}} ->
            KeepAlive = keep_alive(Req#req.vsn, Val),
            headers(C, Req#req{connection = KeepAlive}, [{'Connection', Val}|Headers]);
        {ok, {http_header, _, 'Proxy-Connection', _, Val}} ->
            KeepAlive = keep_alive(Req#req.vsn, Val),
            headers(C, Req#req{connection = KeepAlive}, [{'Connection', Val}|Headers]);
        {ok, {http_header, _, Header, _, Val}} ->
            headers(C, Req, [{Header, Val}|Headers]);
        {error, {http_error, "\r\n"}} ->
	    headers(C, Req, Headers);
	{error, {http_error, "\n"}} ->
            headers(C, Req, Headers);
        {ok, http_eoh} ->
            body(C, Req#req{headers = lists:reverse(Headers)});
	_Other ->
	    exit(normal)
    end.

%%--------------------------------------------------------------------
%% Function: body(#c, Req) ->
%% Description: get body of the request
%%              
%%--------------------------------------------------------------------
body(#c{sock = Sock} = C, Req) ->
    case Req#req.method of
        'GET' ->
	    handle_request(C, Req, get);
	'HEAD' ->
	    handle_request(C, Req, head);
	'POST' when is_integer(Req#req.content_length) ->
	    inet:setopts(Sock, [{packet, raw}]),
	    case gen_tcp:recv(Sock, Req#req.content_length, ?TIMEOUT) of
		{ok, Bin} ->
		    handle_request(C, Req#req{body=Bin}, post);
		_Error ->
		    exit(normal)
	    end;
	%% PUT, DELETE, TRACE, OPTIONS ...
        _Other -> 
	    send(C, [badalisk_utility:encode_status(?STATUS_501)]),
	    exit(normal)
    end.    

%%--------------------------------------------------------------------
%% Function: handle_request(C, #req, Method) ->
%% Description: handle http request
%%              
%%--------------------------------------------------------------------
handle_request(C, #req{connection = Conn} = Req, Method) ->
    {Close, Reply} = case Req#req.uri of
			 {absoluteURI, http, Host, _, Path} ->
			     Url = lists:flatten(["http://", Host, Path]),
			     Res = build_response(Method, Url, Req),
			     {Conn, badalisk_utility:encode_reply(Res)};
			 {absoluteURI, _Other_method, _Host, _, _Path} ->
			     {close, [badalisk_utility:encode_status(?STATUS_501)]};
			 {scheme, _Scheme, _RequestString} ->
			     {Conn, [badalisk_utility:encode_status(?STATUS_501)]};
			 {abs_path, Resource} ->
			     Host = get_header_value('Host', Req#req.headers),
			     case Host of
				 undefined ->
				     {close, [badalisk_utility:encode_status(?STATUS_501)]};
				 Value ->
				     Url = lists:flatten(["http://", Value, Resource]),
				     Res = build_response(Method, Url, Req),
				     {Conn, badalisk_utility:encode_reply(Res)}
			     end;
			 Resource ->
			     Host = get_header_value('Host', Req#req.headers),
			     case Host of
				 undefined ->
				     {close, [badalisk_utility:encode_status(?STATUS_501)]};
				 Value ->
				     Url = lists:flatten(["http://", Value, "/", Resource]),
				     Res = build_response(Method, Url, Req),
				     {Conn, badalisk_utility:encode_reply(Res)}
			     end
		     end,
    send(C, Reply),
    case Close of
	close -> 
	    gen_tcp:close(C#c.sock);
	keep_alive ->
	    inet:setopts(C#c.sock, [{packet, http}]),
	    request(C, #req{})
    end.

%%--------------------------------------------------------------------
%% Function: build_response(Method, Url, Req) ->
%% Description: build response to http request
%%              
%%--------------------------------------------------------------------
build_response(Method, Url, Req) ->
    Res = case badalisk_utility:is_in_blacklist(Url) of
	      false ->
		  {ok, {Status, Headers, Data}} = case Method of
						      post ->
							  http:request(post, {Url, [], get_header_value('Content-Type', Req#req.headers), Req#req.body}, [], []);
						      Other ->
							  http:request(Method, {Url, []}, [], [])
						  end,
		  #res{status=Status, headers=Headers, body=Data};
	      true ->
	          error_logger:info_msg("(~p: ~p) : Refused request to url: ~p~n", [self(), ?MODULE, Url]),
		  #res{status=?STATUS_403, headers=[], body=?CONTENTBLOCKED}
	  end,
    PartialRes = badalisk_utility:apply_censorship(Res),
    FinalRes = badalisk_utility:compress(get_header_value('Accept-Encoding', Req#req.headers), PartialRes).

%%--------------------------------------------------------------------
%% Function: send(#c, Data) -> ok
%% Description: Send data to specific socket
%%              
%%--------------------------------------------------------------------
send(#c{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.

%%--------------------------------------------------------------------
%% Function: keep_alive(Version, KeepAlive} -> keep_alive | close
%% Description: Check whether to keep alive the connection or not
%%              
%%--------------------------------------------------------------------
keep_alive({1,1}, "close") -> 
    close;
keep_alive({1,1}, "Close") -> 
    close;
keep_alive({1,1}, Head) ->
	case string:to_upper(Head) of
	    "CLOSE" -> close;
	    _ -> keep_alive
	end;
keep_alive({1,0}, "Keep-Alive") -> 
    keep_alive;
keep_alive({1,0}, Head) ->
    case string:to_upper(Head) of
	"KEEP-ALIVE" -> 
	    keep_alive;
	_ -> 
	    close
    end;
keep_alive({0,9}, _) -> 
    close;
keep_alive(_Vsn, _KA) -> 
    close.

%%--------------------------------------------------------------------
%% Function: get_header_value(Header, Headers) -> Value | undefined
%% Description: Gives the value of a specific header
%%              
%%--------------------------------------------------------------------
get_header_value(Header, Headers) ->
    case lists:keyfind(Header, 1, Headers) of
	{Header, Value} ->
            Value;
        false ->
            undefined
    end.
