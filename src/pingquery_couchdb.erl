%% Module to ping a query server.

-module(pingquery_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_pingquery_req/1]).

-include("couch_db.hrl").

%% TODO:
%% * Reading couch_config at query time is "bad." Instead, be a gen_server and register for config changes.

handle_pingquery_req(Req=#httpd{method='POST', path_parts=PathParts})
    -> ?LOG_DEBUG("Received ping request: ~p", [Req])
    , couch_httpd:validate_ctype(Req, "application/json")
    , ok = couch_httpd:verify_is_server_admin(Req)
    , case PathParts
        of [_HandlerTrigger, Language]
            -> ?LOG_DEBUG("Requested query language: ~s", [Language])
            % The idea is to create a fresh design doc for every query to guarantee that the view server has to evaluate/compile the
            % code and run it.
            , Uuid = couch_uuids:random()
            , Id = <<"_design/", Uuid/binary>>
            , Revs = {1, [<<"12345">>]}
            , Code = <<"function() { return 'You betcha\\n'; };">>
            , DDocBody = {[ {<<"_id">>, Id}
                          , {<<"_rev">>, <<"1-12345">>}
                          , {<<"language">>, <<"javascript">>}
                          , {<<"shows">>, {[ {<<"pingquery">>, Code} ]}}
                          ]}
            , DDoc = #doc{id=Id, revs=Revs, body=DDocBody}
            , ?LOG_DEBUG("DDoc: ~p", [DDoc])
            , JsonReq = stub_req_obj(Req)
            , ?LOG_DEBUG("Req: ~p", [JsonReq])
            , try couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, <<"pingquery">>], [{[]}, JsonReq])
                of [<<"resp">>, {[{<<"body">>, Response}]}]
                    -> couch_httpd:send_response(Req, 200, [], Response)
                ; Else
                    -> ?LOG_ERROR("Unexpected response from view server prompt: ~p", [Else])
                    , send_bad_ping(Req, Else)
                catch throw:{<<"render_error">>, Reason}
                    -> send_bad_ping(Req, Reason)
                ; throw:{<<"compilation_error">>, Reason}
                    -> send_bad_ping(Req, Reason)
                ; Class:Exc
                    -> send_bad_ping(Req, io_lib:format("Erlang exception: ~p:~p", [Class, Exc]))
                end
            %, ?LOG_DEBUG("Result from prompt:\n~p", [Result])
            %, couch_httpd:send_json(Req, 500, {[{error, <<"not_implemented">>}]})
        ; _
            -> couch_httpd:send_json(Req, 400, {[{error, <<"bad_request">>}, {reason, <<"Query server language required">>}]})
        end
    ;

handle_pingquery_req(Req)
    -> couch_httpd:send_method_not_allowed(Req, "POST")
    .

stub_req_obj(_Req)
    % TODO , JsonReq = couch_httpd_external:json_req_obj(Req, #db{})
    -> {[{<<"req">>, <<"not implemented">>}]}
    .

send_bad_ping(Req, Reason)
    -> JsonResponse = {[{<<"error">>, <<"bad_ping">>}, {<<"reason">>, couch_util:to_binary(Reason)}]}
    , couch_httpd:send_json(Req, 500, JsonResponse)
    .

% vim: sw=4 sts=4 et
