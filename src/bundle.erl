-module(bundle).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

%% Contains a collection of resources
%% Rule: total only when a search or history
%% Rule: entry.search only when a search
%% Rule: entry.request mandatory for batch/transaction/history, otherwise prohibited
%% Rule: entry.response mandatory for batch-response/transaction-response/history, otherwise prohibited
%% Rule: FullUrl must be unique in a bundle, or else entries with the same fullUrl must have different meta.versionId (except in history bundles)
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
    , type        :: boolean()                           % TODO code
    , timestamp   :: instant()
    , total       :: unsignedInt()
    , link        :: [bundlelink()]
    , entry       :: [bundleentry()]
    , signature   :: complex:signature()
    }).
-type bundle() :: #bundle{}.

-record(bundlelink, {
           relation :: binary()
         , uri :: uri()
         }).
-type bundlelink() :: #bundlelink{}.

-record(bundleentry, {
           link :: [bundlelink()]
         , full_uri :: uri()
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
         , if_none_match :: binary()
         , if_modified_since :: instant()
         , if_match :: binary()
         , if_none_exist :: binary()
         }).
-type bundlerequest() :: #bundlerequest{}.

-record(bundleresponse, {
           status :: binary()
         , location :: uri()
         , etag :: binary()
         , last_modified :: instant()
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


to_bundle(Props) ->
  DT = decode:xsd_info(<<"Bundle">>),
  #bundle{
      id          = decode:value(<<"id">>, Props, DT)
    , meta        = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"inplicitRules">>, Props, DT)
    , language    = decode:value(<<"language">>, Props, DT)
    , identifier_ = decode:value(<<"identifier">>, Props, DT)
    , type        = decode:value(<<"type">>, Props, DT)        % TODO code
    , timestamp   = decode:value(<<"timestamp">>, Props, DT)
    , total       = decode:value(<<"total">>, Props, DT)
    , link        = decode:value(<<"Bundle.Link">>, Props, DT)
    , entry       = decode:value(<<"Bundle.Entry">>, Props, DT)
    , signature   = decode:value(<<"signature">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_link({Props}) -> to_link(Props);
to_link(Props) ->
  DT = decode:xsd_info(<<"Bundle.Link">>),
  #bundlelink{
      relation = decode:value(<<"relationship">>, Props, DT)
    , uri  = decode:value(<<"uri">>, Props, DT)
    }.

to_entry({Props}) -> to_entry(Props);
to_entry(Props) ->
  DT = decode:xsd_info(<<"Bundle.Entry">>),
  #bundleentry{
      link      = decode:value(<<"link">>,Props, DT)
    , full_uri  = decode:value(<<"fullUri">>, Props, DT)
    , resource  = decode:value(<<"resource">>,Props, DT)
    , search    = decode:value(<<"search">>,Props, DT)
    , request   = decode:value(<<"request">>, Props, DT)
    , reponse   = decode:value(<<"response">>, Props, DT)
    }.

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
         , if_none_match = decode:value(<<"ifNoneMatch">>, Props, DT)
         , if_modified_since = decode:value(<<"ifModifiedSince">>, Props, DT)
         , if_match      = decode:value(<<"ifMatch">>, Props, DT)
         , if_none_exist = decode:value(<<"ifNoneExist">>, Props, DT)
		}.

to_response({Props}) -> to_response(Props);
to_response(Props) ->
    DT = decode:xsd_info(<<"Bundle.Response">>),
    #bundleresponse{
           status        = decode:value(<<"status">>, Props, DT)
         , location      = decode:value(<<"location">>, Props, DT)
         , etag          = decode:value(<<"etag">>, Props, DT)
         , last_modified = decode:value(<<"lastModified">>, Props, DT)
         , outcome       = decode:value(<<"outcome">>, Props, DT)
	}.
%%
%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, patient:to_patient(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:rec_to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode({encode:rec_to_proplist(A)}))).

bundle_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, unsigned,
                          [],[],undefined}).
bundle_toprop_test() ->
    ?asrtp(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, unsigned,
                          [],[],undefined},
            [{<<"resourceType">>,<<"Bundle">>},
              {<<"id">>,<<"p-21666">>}
            ]).

bundle_json_test() ->
    ?asrtjson(
         {bundle,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,undefined, undefined, unsigned,
                          [],[],undefined},
           <<"{\"resourceType\":\"Bundle\",\"id\":\"p-21666\"}">>).

-endif.
