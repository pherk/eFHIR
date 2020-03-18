-module(bundle).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

%% Contains a collection of resources
%% Rule: total only when a search or history
%% Rule: entry.search only when a search
%% Rule: entry.request mandatory for batch/transaction/history, otherwise prohibited
%% Rule: entry.response mandatory for batch-response/transaction-response/history, otherwise prohibited
%% Rule: entry mandatory unless there is a request or response
%% Rule: fullUrl must be unique in a bundle, or else entries with the same fullUrl must have different meta.versionId (except in history bundles)
%% Rule: entry fullUrl cannot be a version specific reference
%% Rule: A document must have an identifier with a system and a value
%% Rule: A document must have a date
%% Rule: A document must have a Composition as the first resource
%% Rule: A message must have a MessageHeader as the first resource

-record('Bundle.Response', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	status :: string(),
	location :: uri() | undefined,
	etag :: string() | undefined,
	lastModified :: instant() | undefined,
	outcome :: resource:'ResourceContainer'() | undefined}).

-type 'Bundle.Response'() :: #'Bundle.Response'{}.


-record('Bundle.Request', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	method :: code(),
	url :: uri(),
	ifNoneMatch :: string() | undefined,
	ifModifiedSince :: instant() | undefined,
	ifMatch :: string() | undefined,
	ifNoneExist :: string() | undefined}).

-type 'Bundle.Request'() :: #'Bundle.Request'{}.


-record('Bundle.Search', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	mode :: code() | undefined,
	score :: decimal() | undefined}).

-type 'Bundle.Search'() :: #'Bundle.Search'{}.


-record('Bundle.Entry', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	link :: ['Bundle.Link'()] | undefined,
	fullUrl :: uri() | undefined,
	resource :: resource:'ResourceContainer'() | undefined,
	search :: 'Bundle.Search'() | undefined,
	request :: 'Bundle.Request'() | undefined,
	response :: 'Bundle.Response'() | undefined}).

-type 'Bundle.Entry'() :: #'Bundle.Entry'{}.


-record('Bundle.Link', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	relation :: string(),
	url :: uri()}).

-type 'Bundle.Link'() :: #'Bundle.Link'{}.


-record('Bundle', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	identifier :: complex:'Identifier'() | undefined,
	type :: code(),
	timestamp :: instant() | undefined,
	total :: unsignedInt() | undefined,
	link :: ['Bundle.Link'()] | undefined,
	entry :: ['Bundle.Entry'()] | undefined,
	signature :: complex:'Signature'() | undefined}).

-type 'Bundle'() :: #'Bundle'{}.



%%
%% API exports
%%-export([]).
%%
%%====================================================================
%% API functions
%%====================================================================
new('Bundle', {<<"batch-response">>, Entries}) ->
    #'Bundle'{type = <<"batch-response">>,
              entry = Entries};
new('Bundle', {<<"searchset">>, Entries}) ->
    #'Bundle'{type = <<"searchset">>,
              entry = Entries};
%% {ok, Uri, Resource} is a response from dao functions
%% automatically generate a response prop for entry
%%       <outcome>
%%        <OperationOutcome>
%%          <issue>
%%             <severity value="warning"/>
%%             <code value="not-found"/>
%%             <details>
%%               <text value="The Managing organization was not known and was deleted"/>
%%             </details>
%%             <expression value="Patient.managingOrganization"/>
%%          </issue>
%%        </OperationOutcome>
new('Bundle.Entry',{Uri, Etag, Resource, Outcome}) ->
    Response = #'Bundle.Response'{status = 200, location = Uri, etag = Etag, lastModified = <<"2001-01-01">>, outcome = Outcome},
    #'Bundle.Entry'{resource = resource:to_resource(Resource), response = Response}.

to_bundle({Props}) -> to_bundle(Props);
to_bundle(Props) ->
  DT = decode:xsd_info(<<"Bundle">>),
  #'Bundle'{
      anyAttribs  = decode:attrs(Props, DT)
    , id          = decode:value(<<"id">>, Props, DT)
    , meta        = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language    = decode:value(<<"language">>, Props, DT)
    , 'identifier' = decode:value(<<"identifier">>, Props, DT)
    , type        = decode:value(<<"type">>, Props, DT) 
    , timestamp   = decode:value(<<"timestamp">>, Props, DT)
    , total       = decode:value(<<"total">>, Props, DT)
    , link        = decode:value(<<"link">>, Props, DT)
    , entry       = decode:value(<<"entry">>, Props, DT)
    , signature   = decode:value(<<"signature">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_bundle_link({Props}) -> to_bundle_link(Props);
to_bundle_link(Props) ->
    DT = decode:xsd_info(<<"Bundle.Link">>),
    #'Bundle.Link'{
        anyAttribs  = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension   = decode:value(<<"extension">>, Props, DT)
      , modifierExtension   = decode:value(<<"modifierExtension">>, Props, DT)
      , relation = decode:value(<<"relation">>, Props, DT)
      , url  = decode:value(<<"url">>, Props, DT)
    }.

to_bundle_entry({Props}) -> to_bundle_entry(Props);
to_bundle_entry(Props) ->
    DT = decode:xsd_info(<<"Bundle.Entry">>),
    #'Bundle.Entry'{
        anyAttribs  = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension   = decode:value(<<"extension">>, Props, DT)
      , modifierExtension   = decode:value(<<"modifierExtension">>, Props, DT)
      , link      = decode:value(<<"link">>,Props, DT)
      , fullUrl   = decode:value(<<"fullUrl">>, Props, DT)
      , resource  = decode:value(<<"resource">>,Props, DT)
      , search    = decode:value(<<"search">>,Props, DT)
      , request   = decode:value(<<"request">>, Props, DT)
      , response  = decode:value(<<"response">>, Props, DT)
    }.

to_bundle_search({Props}) -> to_bundle_search(Props);
to_bundle_search(Props) ->
    DT = decode:xsd_info(<<"Bundle.Search">>),
	#'Bundle.Search'{
        anyAttribs  = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension   = decode:value(<<"extension">>, Props, DT)
      , modifierExtension   = decode:value(<<"modifierExtension">>, Props, DT)
      , mode = decode:value(<<"mode">>, Props, DT)
      , score = decode:value(<<"score">>, Props, DT)
	 }.

to_bundle_request({Props}) -> to_bundle_request(Props);
to_bundle_request(Props) ->
    DT = decode:xsd_info(<<"Bundle.Request">>),
    #'Bundle.Request'{
        anyAttribs  = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension   = decode:value(<<"extension">>, Props, DT)
      , modifierExtension   = decode:value(<<"modifierExtension">>, Props, DT)
      , method        = decode:value(<<"method">>, Props, DT)
      , url           = decode:value(<<"url">>, Props, DT)
      , ifNoneMatch   = decode:value(<<"ifNoneMatch">>, Props, DT)
      , ifModifiedSince = decode:value(<<"ifModifiedSince">>, Props, DT)
      , ifMatch       = decode:value(<<"ifMatch">>, Props, DT)
      , ifNoneExist   = decode:value(<<"ifNoneExist">>, Props, DT)
		}.

to_bundle_response({Props}) -> to_bundle_response(Props);
to_bundle_response(Props) ->
    DT = decode:xsd_info(<<"Bundle.Response">>),
    #'Bundle.Response'{
        anyAttribs  = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension   = decode:value(<<"extension">>, Props, DT)
      , modifierExtension   = decode:value(<<"modifierExtension">>, Props, DT)
      , status        = decode:value(<<"status">>, Props, DT)
      , location      = decode:value(<<"location">>, Props, DT)
      , etag          = decode:value(<<"etag">>, Props, DT)
      , lastModified  = decode:value(<<"lastModified">>, Props, DT)
      , outcome       = decode:value(<<"outcome">>, Props, DT)
	}.
%%
%%
%-spec unmarshal({xml, Doc, Description} | {json, Doc, Description} | {postgres, ...}) -> {ok, 'Bundle'()} | {error, Reason}.
%-spec marshal(Bundle, xml | json | transit) -> Data.
% repr(Bundle, {msgpack, View}) -> msgpack:encode(repr(Bundle, View));
repr(Bundle, {json, View}) -> jiffy:encode(repr(Bundle, View));
repr(#'Bundle'{ type = T, timestamp = TS, total = Total }, summary) ->
    { TS, T, Total };
repr(Bundle, {summary, R}) ->
    repr_summary(Bundle, R).

repr_summary(#'Bundle'{ entry = Es } = Bundle, R) ->
%    io:format("~p~n",[Bundle]),
    lists:filtermap(fun text/1, Es).

resource(#'Bundle.Entry'{resource=Res}) ->
    Res.

id(#'Bundle.Entry'{fullUrl= U, resource=Res, request= Req}) ->
    12345.

text(#'Bundle.Entry'{resource=R}) ->
%    io:format("~p~n",[R]),
    {true, resource:text(R)}.

% Now, suppose you have a list of resources in the Bundle:
%
% Rel = [repr(B, {relation, genre}) || B <- Bs],
% R = sofs:relation(Rel),
% F = sofs:relation_to_family(R),
% sofs:to_external(F).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, bundle:to_bundle(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

bundle_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>},{<<"type">>,<<"searchset">>},
             {<<"entry">>, [{[{<<"fullUrl">>,<<"http://eNahar.org/nabu/patient-test">>},
                              {<<"resource">>,{[
                                 {<<"resourceType">>, <<"Patient">>},
                                 {<<"id">>, <<"p-21666">>}
                                             ]}}]
                           }]}
            ],
            {'Bundle',[],<<"p-21666">>,undefined,undefined,undefined, undefined,<<"searchset">>,undefined,undefined,[],
                     [{'Bundle.Entry',[],undefined,[],[],[],
                          <<"http://eNahar.org/nabu/patient-test">>,
                          {'Patient',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                           [],undefined,[],[], undefined,undefined,undefined,[],
                           undefined,undefined,[],[],[],[], undefined,[]},
                          undefined,undefined,undefined}],
                     undefined}
           ).

bundle_toprop_test() ->
    ?asrtp(
            {'Bundle',[],<<"p-21666">>,undefined,undefined,undefined, undefined,<<"searchset">>,undefined,undefined,[],
                     [{'Bundle.Entry',[],undefined,[],[],[],
                          <<"http://eNahar.org/nabu/patient-test">>,
                          {'Patient',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                           [],undefined,[],[], undefined,undefined,undefined,[],
                           undefined,undefined,[],[],[],[], undefined,[]},
                          undefined,undefined,undefined}],
                     undefined},
         {[{<<"resourceType">>,<<"Bundle">>},
           {<<"id">>,<<"p-21666">>},
           {<<"type">>,<<"searchset">>},
           {<<"entry">>,
               [{[{<<"fullUrl">>, <<"http://eNahar.org/nabu/patient-test">>},
                  {<<"resource">>,
                        {[{<<"resourceType">>,<<"Patient">>},
                          {<<"id">>,<<"p-21666">>}]}}]}]}]}
            ).

bundle_json_test() ->
    ?asrtjson(
            {'Bundle',[],<<"p-21666">>,undefined,undefined,undefined, undefined,<<"searchset">>,undefined,undefined,[],
                     [{'Bundle.Entry',[],undefined,[],[],[],
                          <<"http://eNahar.org/nabu/patient-test">>,
                          {'Patient',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                           [],undefined,[],[], undefined,undefined,undefined,[],
                           undefined,undefined,[],[],[],[], undefined,[]},
                          undefined,undefined,undefined}],
                     undefined},
            <<"{\"resourceType\":\"Bundle\",\"id\":\"p-21666\",\"type\":\"searchset\",\"entry\":[{\"fullUrl\":\"http://eNahar.org/nabu/patient-test\",\"resource\":{\"resourceType\":\"Patient\",\"id\":\"p-21666\"}}]}">>
      ).

-endif.
