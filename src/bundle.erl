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
    , meta        :: complex:meta()
    , implicitRules :: uri()
    , language    :: code()
    , identifier_ :: complex:identifier()
    , type        :: code() 
    , timestamp   :: instant()
    , total       :: unsignedInt()
    , link        :: [bundlelink()]
    , entry       :: [bundleentry()]
    , signature   :: complex:signature()
    }).
-type bundle() :: #bundle{}.

-record(bundlelink, {
           relation :: binary()
         , url :: uri()
         }).
-type bundlelink() :: #bundlelink{}.

-record(bundleentry, {
           link :: [bundlelink()]
         , fullUrl :: uri()
         , resource :: resource:resourceContainer()
         , search   :: bundlesearch()
         , request  :: bundlerequest()
         , reponse  :: bundleresponse()
         }).
-type bundleentry() :: #bundleentry{}.

-record(bundlesearch, {
           mode :: code()
         , score :: decimal()
         }).
-type bundlesearch() :: #bundlesearch{}.

-record(bundlerequest, {
           method :: binary()                      % code()
         , uri    :: uri()
         , ifNoneMatch :: binary()
         , ifModifiedSince :: instant()
         , ifMatch :: binary()
         , ifNoneExist :: binary()
         }).
-type bundlerequest() :: #bundlerequest{}.

-record(bundleresponse, {
           status :: binary()
         , location :: uri()
         , etag :: binary()
         , lastModified :: instant()
         , outcome :: resource:resourceContainer()
         }).
-type bundleresponse() :: #bundleresponse{}.



%%
%% API exports
%%-export([]).
%%
%%====================================================================
%% API functions
%%====================================================================


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
  #bundlelink{
      relation = decode:value(<<"relation">>, Props, DT)
    , url  = decode:value(<<"url">>, Props, DT)
    }.

to_bundle_entry({Props}) -> to_bundle_entry(Props);
to_bundle_entry(Props) ->
  DT = decode:xsd_info(<<"Bundle.Entry">>),
  #bundleentry{
      link      = decode:value(<<"link">>,Props, DT)
    , fullUrl   = decode:value(<<"fullUrl">>, Props, DT)
    , resource  = decode:value(<<"resource">>,Props, DT)
    , search    = decode:value(<<"search">>,Props, DT)
    , request   = decode:value(<<"request">>, Props, DT)
    , reponse   = decode:value(<<"response">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_search({Props}) -> to_search(Props);
to_search(Props) ->
    DT = decode:xsd_info(<<"Bundle.Search">>),
	#bundlesearch{
       mode = decode:value(<<"mode">>, Props, DT)
     , score = decode:value(<<"score">>, Props, DT)
	 }.

to_request({Props}) -> to_request(Props);
to_request(Props) ->
    DT = decode:xsd_info(<<"Bundle.Request">>),
    #bundlerequest{
           method        = decode:value(<<"mode">>, Props, DT)
         , uri           = decode:value(<<"uri">>, Props, DT)
         , ifNoneMatch = decode:value(<<"ifNoneMatch">>, Props, DT)
         , ifModifiedSince = decode:value(<<"ifModifiedSince">>, Props, DT)
         , ifMatch       = decode:value(<<"ifMatch">>, Props, DT)
         , ifNoneExist   = decode:value(<<"ifNoneExist">>, Props, DT)
		}.

to_response({Props}) -> to_response(Props);
to_response(Props) ->
    DT = decode:xsd_info(<<"Bundle.Response">>),
    #bundleresponse{
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
    io:format("~p~n",[Bundle]),
    lists:filtermap(fun text/1, Es).

text(#bundleentry{resource=R}) ->
    io:format("~p~n",[R]),
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
-define(asrtp(A, B), ?assertEqual(B, encode:rec_to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode({encode:rec_to_proplist(A)}))).

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
                          [{bundleentry,[],
                                        <<"http://eNahar.org/nabu/patient-test">>,
                                        {[{<<"resourceType">>,<<"Patient">>},
                                          {<<"id">>,<<"p-21666">>}]},
                                        undefined,undefined,undefined}],
                          undefined}).
bundle_toprop_test() ->
    ?asrtp(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, undefined,
                          [],[],undefined},
            [{<<"resourceType">>,<<"Bundle">>},
              {<<"id">>,<<"p-21666">>}
            ]).

bundle_json_test() ->
    ?asrtjson(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, undefined,
                          [],[],undefined},
           <<"{\"resourceType\":\"Bundle\",\"id\":\"p-21666\"}">>).

-endif.
