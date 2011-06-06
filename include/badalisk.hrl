-define(PORT, badalisk_utility:get_conf_value(port)).
-define(TIMEOUT, badalisk_utility:get_conf_value(timeout)).
-define(DEFAULTCOMPRESSION, badalisk_utility:get_conf_value(def_compression)).
-define(LOGFILE, badalisk_utility:get_conf_value(logfile)).
-define(BADWORDS, badalisk_utility:get_conf_value(badwords)).
-define(BLACKLISTED, badalisk_utility:get_conf_value(blacklist)).
-define(CENSORSHIPLEVEL, badalisk_utility:get_conf_value(censorship_level)).

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

-record(res, {content_length,
	      status,                          
	      headers,                         % [{Tag, Val}]
	      body = <<>>}).                   % Content Body
