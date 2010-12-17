%% Module to ping a query server.

-module(pingquery_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_pingquery_req/1]).

-include("couch_db.hrl").

handle_pingquery_req(Req=#httpd{method=Method}) when Method =/= 'POST'
    -> couch_httpd:send_method_not_allowed(Req, "POST")
    ;

handle_pingquery_req(Req=#httpd{method='POST'})
    -> ?LOG_DEBUG("Received ping request: ~p", [Req])
    , couch_httpd:validate_ctype(Req, "application/json")
    , ok = couch_httpd:verify_is_server_admin(Req)
    , try get_ping_spec(Req)
        of {ok, Language, Code, ExpectedResult}
            -> ping_query_server(Req, Language, Code, ExpectedResult)
        catch throw:{error, Reason}
            -> send_bad_query(Req, Reason)
        ; throw:{error, Reason, Extras}
            -> send_bad_query(Req, Reason, Extras)
        end
    .

ping_query_server(Req, Language, Code, ExpectedResult)
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
    , try couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, <<"pingquery">>], [{[]}, {[]}])
        of [<<"resp">>, {[{<<"body">>, Response}]}]
            -> ?LOG_DEBUG("Response from ping execution: ~p", [Response])
            , case Response
                of ExpectedResult
                    -> couch_httpd:send_json(Req, 200, {[{<<"ok">>, true}, {<<"match">>, Response}]})
                ; BadResult
                    -> ?LOG_DEBUG("Bad ping match; Expected=~p Result=~p", [ExpectedResult, BadResult])
                    , send_bad_ping(Req, "no_match", {[{<<"expected">>, ExpectedResult}, {<<"received">>, BadResult}]})
                end
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
    .

get_ping_spec(Req=#httpd{path_parts=PathParts})
    -> Example = {[{<<"example">>, {[{<<"in">>, <<"function() { return 'Hello, world!'; }">>}, {<<"out">>, <<"Hello, world!">>}]}}]}
    , Language = case length(PathParts)
        of X when X < 2 -> throw({error, "Language required; e.g. /_pingquery/javascript"})
        ; _             -> lists:nth(2, PathParts)
        end

    , ?LOG_DEBUG("Requested query language: ~s", [Language])
    , PingSpec = try couch_httpd:json_body(Req)
        of {Body} -> Body
        catch throw:{invalid_json, undefined}
            -> throw({error, "JSON body required; see the example", Example})
        ; throw:{invalid_json, _}
            -> throw({error, "Invalid JSON; see the example", Example})
        ; Class:Exc
            -> ?LOG_ERROR("Uncaught exception processing POST body: ~p:~p", [Class, Exc])
            , throw({error, io_lib:format("Erlang exception processing query: ~p:~p", [Class, Exc])})
        end

    , ?LOG_DEBUG("Valid JSON body: ~p", [{PingSpec}])
    , Code = case couch_util:get_value(<<"in">>, PingSpec)
        of undefined
            -> throw({error, "Required 'in' value; see example", Example})
        ; BadCode when is_binary(BadCode) =:= false
            -> throw({error, "'in' value must be a string; see example", Example})
        ; CodeVal
            -> CodeVal
        end

    , ExpectedResult = case couch_util:get_value(<<"out">>, PingSpec)
        of undefined
            -> throw({error, "Required 'out' value; see example", Example})
        ; BadResult when is_binary(BadResult) =:= false
            -> throw({error, "'out' value must be a string; see example", Example})
        ; ER
            -> ER
        end

    % Success!
    , {ok, Language, Code, ExpectedResult}
    .

send_bad_query(Req, Reason)
    -> send_bad_query(Req, Reason, {[]})
    .

send_bad_query(Req, Reason, Extras)
    -> send_json(Req, 400, {[{<<"error">>, <<"bad_query">>}, {<<"reason">>, couch_util:to_binary(Reason)}]}, Extras)
    .

send_bad_ping(Req, Reason)
    -> send_bad_ping(Req, Reason, {[]})
    .

send_bad_ping(Req, Reason, Extras)
    -> send_json(Req, 500, {[{<<"error">>, <<"bad_ping">>}, {<<"reason">>, couch_util:to_binary(Reason)}]}, Extras)
    .

send_json(Req, Status, {Json}, {[]})
    -> couch_httpd:send_json(Req, Status, {Json})
    ;

send_json(Req, Status, {Json}, {[KeyVal | Rest]})
    -> send_json(Req, Status, {Json ++ [KeyVal]}, {Rest})
    .

% vim: sw=4 sts=4 et
