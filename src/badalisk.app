{application, badalisk,
  [{description, "badalisk proxy."},
  {vsn, "1.0"},
  {modules, [badalisk,
  	     badalisk_sup,
	     badalisk_server, 
	     badalisk_conf, 
	     badalisk_loglevel]},
  {registered, [badalisk_sup, badalisk_server, badalisk_conf]},
  {applications, [kernel, stdlib]},
  {mod, {badalisk, []}}
]}.
