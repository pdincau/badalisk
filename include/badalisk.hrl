%% Badalisk include file

-define(PORT, badalisk_conf:lookup(port)).
-define(SSLPORT, badalisk_conf:lookup(sslport)).
-define(TIMEOUT, badalisk_conf:lookup(timeout)).
-define(DEFAULTCOMPRESSION, badalisk_conf:lookup(def_compression)).
-define(LOGFILE, badalisk_conf:lookup(logfile)).
-define(BADWORDS, badalisk_conf:lookup(badwords)).
-define(BLACKLISTED, badalisk_conf:lookup(blacklist)).
-define(CENSORSHIPLEVEL, badalisk_conf:lookup(censorship_level)).

-define(STATUS_202, {"HTTP/1.1", 202, "OK"}).
-define(STATUS_403, {"HTTP/1.1", 403, "Forbidden"}).
-define(STATUS_404, {"HTTP/1.1", 404, "Not Found"}).
-define(STATUS_501, {"HTTP/1.1", 501, "Not Implemented"}).

-define(CONTENTBLOCKED, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n",
			 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\r\n",
			 "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\r\n",
			 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\r\n",
			 "<head>\r\n",
			 "<title>403 Forbidden</title>\r\n",
			 "</head>\r\n",
			 "<body>\r\n",
			 "<h1>Error 403: Forbidden</h1>\r\n",
			 "<p>Access to required page was blocked by proxy.</p>\r\n",
			 "</body>\r\n",
			 "</html>\n">>).

-record(req, {connection=keep_alive,	        % keep_alive | close
	      content_length,                   % Integer
	      vsn,                              % {Maj,Min}
	      method,                           % 'GET'|'POST'|'HEAD'
	      uri,				% Truncated URI /index.html
              args="",                          % Part of URI after ?
	      headers,				% [{Tag, Val}]
	      body = <<>>}).			% Content Body

-record(res, {content_length,                   % Integer
	      status,                           % {Vsn, Code, Explanation})
	      headers,                          % [{Tag, Val}]
	      body = <<>>}).                    % Content Body

%%----------------------------------------------------------------------
%% LOGGING OPTIONS
%%----------------------------------------------------------------------
-define(DEBUG(Format, Args), badalisk_logger:debug_msg(?MODULE, ?LINE, Format, Args)).
-define(INFO_MSG(Format, Args), badalisk_logger:info_msg(?MODULE, ?LINE, Format, Args)).
-define(WARNING_MSG(Format, Args), badalisk_logger:warning_msg(?MODULE, ?LINE, Format, Args)).
-define(ERROR_MSG(Format, Args), badalisk_logger:error_msg(?MODULE, ?LINE, Format, Args)).
-define(CRITICAL_MSG(Format, Args), badalisk_logger:critical_msg(?MODULE, ?LINE, Format, Args)).
