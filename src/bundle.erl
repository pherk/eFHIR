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

-record(bundle, {
      id          :: id()
    , meta        :: special:meta()
    , implicitRules :: uri()
    , language    :: code()
    , identifier_ :: complex:identifier()
    , type        :: code() 
    , timestamp   :: instant()
    , total       :: unsignedInt()
    , link        :: [bundle_link()]
    , entry       :: [bundle_entry()]
    , signature   :: complex:signature()
    }).
-type bundle() :: #bundle{}.

-record(bundle_link, {
           relation :: binary()
         , url :: uri()
         }).
-type bundle_link() :: #bundle_link{}.

-record(bundle_entry, {
           link :: [bundle_link()]
         , fullUrl :: uri()
         , resource :: resource:resourceContainer()
         , search   :: bundle_search()
         , request  :: bundle_request()
         , response  :: bundle_response()
         }).
-type bundle_entry() :: #bundle_entry{}.

-record(bundle_search, {
           mode :: code()
         , score :: decimal()
         }).
-type bundle_search() :: #bundle_search{}.

-record(bundle_request, {
           method :: code() 
         , url    :: uri()
         , ifNoneMatch :: binary()
         , ifModifiedSince :: instant()
         , ifMatch :: binary()
         , ifNoneExist :: binary()
         }).
-type bundle_request() :: #bundle_request{}.

-record(bundle_response, {
           status :: binary()
         , location :: uri()
         , etag :: binary()
         , lastModified :: instant()
         , outcome :: resource:resourceContainer()
         }).
-type bundle_response() :: #bundle_response{}.



%%
%% API exports
%%-export([]).
%%
%%====================================================================
%% API functions
%%====================================================================
new(bundle, {<<"batch-response">>, Entries}) ->
    #bundle{type = <<"batch-response">>, entry = Entries};
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
new(entry,{Uri, Etag, Resource, Outcome}) ->
    Response = #bundle_response{status = 200, location = Uri, etag = Etag, lastModified = <<"2001-01-01">>, outcome = Outcome},
    #bundle_entry{resource = resource:to_resource(Resource), response = Response}.

to_bundle({Props}) -> to_bundle(Props);
to_bundle(Props) ->
  DT = decode:xsd_info(<<"Bundle">>),
  #bundle{
      id          = decode:value(<<"id">>, Props, DT)
    , meta        = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language    = decode:value(<<"language">>, Props, DT)
    , identifier_ = decode:value(<<"identifier">>, Props, DT)
    , type        = decode:value(<<"type">>, Props, DT) 
    , timestamp   = decode:value(<<"timestamp">>, Props, DT)
    , total       = decode:value(<<"total">>, Props, DT)
    , link        = decode:value(<<"link">>, Props, DT)
    , entry       = decode:value(<<"entry">>, Props, DT)
    , signature   = decode:value(<<"signature">>, Props, DT)
    }.

to_bundle_link({Props}) -> to_bundle_link(Props);
to_bundle_link(Props) ->
  DT = decode:xsd_info(<<"Bundle.Link">>),
  #bundle_link{
      relation = decode:value(<<"relation">>, Props, DT)
    , url  = decode:value(<<"url">>, Props, DT)
    }.

to_bundle_entry({Props}) -> to_bundle_entry(Props);
to_bundle_entry(Props) ->
  DT = decode:xsd_info(<<"Bundle.Entry">>),
  #bundle_entry{
      link      = decode:value(<<"link">>,Props, DT)
    , fullUrl   = decode:value(<<"fullUrl">>, Props, DT)
    , resource  = decode:value(<<"resource">>,Props, DT)
    , search    = decode:value(<<"search">>,Props, DT)
    , request   = decode:value(<<"request">>, Props, DT)
    , response  = decode:value(<<"response">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_bundle_search({Props}) -> to_bundle_search(Props);
to_bundle_search(Props) ->
    DT = decode:xsd_info(<<"Bundle.Search">>),
	#bundle_search{
       mode = decode:value(<<"mode">>, Props, DT)
     , score = decode:value(<<"score">>, Props, DT)
	 }.

to_bundle_request({Props}) -> to_bundle_request(Props);
to_bundle_request(Props) ->
    DT = decode:xsd_info(<<"Bundle.Request">>),
    #bundle_request{
           method        = decode:value(<<"method">>, Props, DT)
         , url           = decode:value(<<"url">>, Props, DT)
         , ifNoneMatch   = decode:value(<<"ifNoneMatch">>, Props, DT)
         , ifModifiedSince = decode:value(<<"ifModifiedSince">>, Props, DT)
         , ifMatch       = decode:value(<<"ifMatch">>, Props, DT)
         , ifNoneExist   = decode:value(<<"ifNoneExist">>, Props, DT)
		}.

to_bundle_response({Props}) -> to_bundle_response(Props);
to_bundle_response(Props) ->
    DT = decode:xsd_info(<<"Bundle.Response">>),
    #bundle_response{
           status        = decode:value(<<"status">>, Props, DT)
         , location      = decode:value(<<"location">>, Props, DT)
         , etag          = decode:value(<<"etag">>, Props, DT)
         , lastModified  = decode:value(<<"lastModified">>, Props, DT)
         , outcome       = decode:value(<<"outcome">>, Props, DT)
	}.
%%
%%
%-spec unmarshal({xml, Doc, Description} | {json, Doc, Description} | {postgres, ...}) -> {ok, bundle()} | {error, Reason}.
%-spec marshal(Bundle, xml | json | transit) -> Data.
% repr(Bundle, {msgpack, View}) -> msgpack:encode(repr(Bundle, View));
repr(Bundle, {json, View}) -> jiffy:encode(repr(Bundle, View));
repr(#bundle{ type = T, timestamp = TS, total = Total }, summary) ->
    { TS, T, Total };
repr(Bundle, {summary, R}) ->
    repr_summary(Bundle, R).

repr_summary(#bundle{ entry = Es } = Bundle, R) ->
%    io:format("~p~n",[Bundle]),
    lists:filtermap(fun text/1, Es).

resource(#bundle_entry{resource=Res}) ->
    Res.

id(#bundle_entry{fullUrl= U, resource=Res, request= Req}) ->
    12345.

text(#bundle_entry{resource=R}) ->
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
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,<<"searchset">>, undefined, undefined,
                          [],
                          [{bundle_entry,
                               [],
                               <<"http://eNahar.org/nabu/patient-test">>,
                               {patient,<<"p-21666">>,undefined,undefined,
                                   undefined,undefined,[],[],[],[],undefined,[],[],
                                   undefined,undefined,undefined,undefined,[],
                                   undefined,undefined,undefined,[],[],[],[],
                                   undefined,[]},
                                undefined,undefined,undefined}],
                          undefined}).
bundle_toprop_test() ->
    ?asrtp(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, undefined,
                          [],[],undefined},
         {[{<<"resourceType">>,<<"Bundle">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

bundle_json_test() ->
    ?asrtjson(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, undefined,
                          [],[],undefined},
           <<"{\"resourceType\":\"Bundle\",\"id\":\"p-21666\"}">>).

-endif.
