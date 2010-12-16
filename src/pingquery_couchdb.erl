%% Module to ping a query server.

-module(pingquery_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_pingquery_req/1]).

-include("couch_db.hrl").

%% TODO:
%% * Reading couch_config at query time is "bad." Instead, be a gen_server and register for config changes.

handle_pingquery_req(Req=#httpd{method='POST'})
    -> ?LOG_DEBUG("Received ping request: ~p", [Req])
    , couch_httpd:validate_ctype(Req, "application/json")
    , ok = couch_httpd:verify_is_server_admin(Req)
    , case get_ping_spec(Req)
        of {error, Reason}
            -> send_bad_query(Req, Reason)
        ; {error, Reason, Extras}
            -> send_bad_query(Req, Reason, Extras)
        ; {ok, Language, Code, ExpectedResult}
            -> ?LOG_DEBUG("Ping in ~s: ~s -> \"~s\"", [Language, Code, ExpectedResult])
            % The idea is to create a fresh design doc for every query to guarantee that the view server has to evaluate/compile the code and run it.
            , Id = <<"_design/pingquery", "(Also, hopefully nobody will ever use this doc id!)">>
            , RevHash = couch_uuids:random()
            , Revs = {1, [RevHash]}
            , DDocBody = {[ {<<"_id">>, Id}
                          , {<<"_rev">>, iolist_to_binary(["1-", RevHash])}
                          , {<<"language">>, Language}
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
                catch throw:{<<"render_error">>, <<"undefined response from show function">>}
                    -> send_bad_ping(Req, "Ping function must return a string")
                ; throw:{<<"render_error">>, Reason}
                    -> send_bad_ping(Req, Reason)
                ; throw:{<<"compilation_error">>, Reason}
                    -> send_bad_query(Req, Reason)
                ; throw:{unknown_query_language, BadLang}
                    -> send_bad_query(Req, [BadLang, " is not a known query language"])
                ; Class:Exc
                    -> ?LOG_ERROR("Uncaught exception handling ddoc prompt: ~p:~p", [Class, Exc])
                    , send_bad_ping(Req, io_lib:format("Erlang exception: ~p:~p", [Class, Exc]))
                end
        end
    ;

handle_pingquery_req(Req)
    -> couch_httpd:send_method_not_allowed(Req, "POST")
    .

stub_req_obj(_Req)
    % TODO , JsonReq = couch_httpd_external:json_req_obj(Req, #db{})
    -> {[{<<"req">>, <<"not implemented">>}]}
    .

get_ping_spec(Req=#httpd{path_parts=PathParts})
    -> Example = {[{<<"example">>, {[{<<"in">>, <<"function() { return 'Hello, world!'; }">>}, {<<"out">>, <<"Hello, world!">>}]}}]}
    , case PathParts
        of [_HandlerTrigger, Language | _Rest]
            -> ?LOG_DEBUG("Requested query language: ~s", [Language])
            , try couch_httpd:json_body(Req)
                of {PingSpec}
                    -> ?LOG_DEBUG("Valid JSON body: ~p", [{PingSpec}])
                    , case couch_util:get_value(<<"in">>, PingSpec)
                        of undefined
                            -> {error, "Required 'in' value; see example", Example}
                        ; BadVal when is_binary(BadVal) =:= false
                            -> {error, "'in' value must be a string; see example", Example}
                        ; Code
                            -> case couch_util:get_value(<<"out">>, PingSpec)
                                of undefined
                                    -> {error, "Required 'out' value; see example", Example}
                                ; BadVal when is_binary(BadVal) =:= false
                                    -> {error, "'out' value must be a string; see example", Example}
                                ; ExpectedResult
                                    % Success!
                                    -> {ok, Language, Code, ExpectedResult}
                                end
                        end
                catch throw:{invalid_json, undefined}
                    -> {error, "JSON body required; see the example", Example}
                ; throw:{invalid_json, _}
                    -> {error, "Invalid JSON; see the example", Example}
                ; Class:Exc
                    -> ?LOG_ERROR("Uncaught exception processing POST body: ~p:~p", [Class, Exc])
                    , {error, io_lib:format("Erlang exception processing query: ~p:~p", [Class, Exc])}
                end
        ; _
            -> {error, "Language required; e.g. /_pingquery/javascript"}
        end
    .

send_bad_query(Req, Reason)
    -> send_bad_query(Req, Reason, {[]})
    .

send_bad_query(Req, Reason, Extras)
    -> send_bad_query(Req, Reason, Extras, {[]})
    .

send_bad_query(Req, Reason, {Json}, {[]})
    -> FinalJson = [{<<"error">>, <<"bad_ping_query">>}, {<<"reason">>, couch_util:to_binary(Reason)}] ++ Json
    , couch_httpd:send_json(Req, 400, {FinalJson})
    ;

send_bad_query(Req, Reason, {Json}, {[KeyVal | Rest]})
    -> send_bad_query(Req, Reason, {Json ++ [KeyVal]}, {Rest})
    .

send_bad_ping(Req, Reason)
    -> JsonResponse = {[{<<"error">>, <<"bad_ping">>}, {<<"reason">>, couch_util:to_binary(Reason)}]}
    , couch_httpd:send_json(Req, 500, JsonResponse)
    .

% vim: sw=4 sts=4 et
