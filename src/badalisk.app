{application, badalisk,
  [{description, "badalisk proxy."},
  {vsn, "1.0"},
  {modules, [badalisk,
  	     badalisk_sup,
	     badalisk_server, 
	     badalisk_conf,
	     badalisk_loglevel,
	     badalisk_disk_logger,
             dynamic_compile]},
  {registered, [badalisk_sup, badalisk_server, badalisk_conf]},
  {applications, [kernel, stdlib]},
  {mod, {badalisk, []}}
]}.
