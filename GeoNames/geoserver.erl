-module(geoserver).
-compile(export_all).

-define(MODULE_LIST, [mod_alias, mod_esi, mod_actions, mod_get, mod_head, mod_log, mod_disk_log]).

start() ->
  case inets:start() of ->
    ok -> inets:start(httpd, [ 
      {modules, ?MODULE_LIST}, 
      {port,8080}, 
      {server_name,"geoserver"}, 
      {server_root,"./"}, 
      {document_root,"./country_data"}, 
      {erl_script_alias, {"/erl", [geodata]}}, 
      {error_log, "error.log"}, 
      {security_log, "security.log"}, 
      {transfer_log, "transfer.log"}, 
      
      {mime_types,[ {"html","text/html"}, {"css","text/css"}, {"js","application/x-javascript"} ]} 
   ]). 
         
stop(Pid) -> inets:stop(httpd, Pid).
