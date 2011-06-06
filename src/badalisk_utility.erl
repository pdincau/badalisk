%%%-------------------------------------------------------------------
%%% File    : badalisk_utility.erl
%%% Author  : Paolo D'Incau <paolo.dincau@gmail.com>
%%% Description : 
%%%
%%% Created : 22 Apr 2011 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(badalisk_utility).

%% API
-export([is_in_blacklist/1, get_conf_value/1,
	 has_bad_content/1, apply_censorship/1, 
	 compress/2, encode_headers/1,
	 encode_status/1, encode_reply/1]).

-include("../include/badalisk.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: get_conf_value(What) -> {ok, Value} | {error, instance}
%% Description: Return the value of a specific parameter inside 
%%              configuration file.
%%--------------------------------------------------------------------
get_conf_value(What) ->
    {ok, Conf} = file:consult("priv/badalisk.cfg"),
    case lists:keyfind(What, 1, Conf) of
	{What, Value} ->
	    Value;
	false ->
	    {error, noinstance}
    end.

%%--------------------------------------------------------------------
%% Function: compress(Method, #res) -> #res 
%% Description: Compress response body in the given method
%%              
%%--------------------------------------------------------------------
compress("gzip,deflate", Res) ->
    case ?DEFAULTCOMPRESSION of
	"gzip" ->
	    compress("gzip", Res);
	"deflate" ->
	    compress("deflate", Res)
    end;
compress("gzip", #res{body=Body, headers=Headers}=Res) ->
    Compressed = zlib:gzip(Body),
    Res#res{body=Compressed, headers=[{"Content-Encoding", "gzip"}|Headers]};
compress("deflate", #res{body=Body, headers=Headers}=Res) ->
    Compressed = zlib:compress(Body),
    Res#res{body=Compressed, headers=[{"Content-Encoding", "deflate"}|Headers]};
compress(undefined, Res) ->
    Res;
compress(Other, Res) -> %% for example in chrome we have gzip,deflate,sdch
    Res.

%%--------------------------------------------------------------------
%% Function: encode_reply(Res) ->
%% Description: Encode reply so that it can be sent
%%              
%%------------------------------------------------------------------
encode_reply(Res) ->  
    [encode_status(Res#res.status), encode_headers(Res#res.headers), "\r\n", Res#res.body].

%%--------------------------------------------------------------------
%% Function: encode_headers(Headers) ->
%% Description: Encode response headers so that they can be sent
%%              
%%--------------------------------------------------------------------
encode_headers([{Field, Value}|Headers]) ->
    [Field, ": ", Value, "\r\n" | encode_headers(Headers)];
encode_headers([]) ->
    [].

%%--------------------------------------------------------------------
%% Function: encode_status({HttpVersion, StatusCode, PhraseReason}) ->
%% Description: Encode status so that it can be sent
%%              
%%--------------------------------------------------------------------
encode_status({HttpVersion, StatusCode, PhraseReason}) ->
    [HttpVersion, " ", integer_to_list(StatusCode), " ", PhraseReason, "\r\n"]. 

%%--------------------------------------------------------------------
%% Function: has_bad_content(Data) ->
%% Description: Check whether the body has bad content
%%              
%%--------------------------------------------------------------------
has_bad_content(Data) when is_binary(Data) ->
    has_bad_content(binary_to_list(Data));
has_bad_content(Data) ->
    CountBadWords = fun(Word, Acc) -> 
			    case string:str(Data, Word) of 
				0 ->
				    Acc; 
				Position -> 
				    Acc + 1 
			    end
		    end, 
    lists:foldl(CountBadWords, 0, ?BADWORDS) >= ?CENSORSHIPLEVEL.

%%--------------------------------------------------------------------
%% Function: apply_censorship(#res) ->
%% Description: Apply censorship to response if needed, according to
%%              configuration file.
%%--------------------------------------------------------------------
apply_censorship(#res{body=Body} = Res) ->
    case badalisk_utility:has_bad_content(Body) of
	true ->
	    error_logger:info_msg("(~p: ~p) : Content blocked.~n", [self(), ?MODULE]),
	    Res#res{status=?STATUS_403, headers=[], body=?CONTENTBLOCKED};
	false ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: is_in_blacklist(Url) -> true | false
%% Description: Returns whether Url is blacklisted or not 
%%          
%%--------------------------------------------------------------------
is_in_blacklist(Url) ->
    lists:member(Url, ?BLACKLISTED).
    
