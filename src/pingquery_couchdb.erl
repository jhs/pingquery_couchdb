%% Module to ping a query server.

-module(pingquery_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_pingquery_req/1]).

-include("couch_db.hrl").

handle_pingquery_req(#httpd{method='POST'}=Req)
    -> ?LOG_DEBUG("Received ping request: ~p", [Req])
    , couch_httpd:validate_ctype(Req, "application/json")
    , ok = couch_httpd:verify_is_server_admin(Req)
    , couch_httpd:send_json(Req, 500, {[{error, <<"not_implemented">>}]})
    ;

handle_pingquery_req(Req)
    -> couch_httpd:send_method_not_allowed(Req, "POST")
    .

% vim: sw=4 sts=4 et
