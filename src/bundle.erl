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
    , meta        :: datatypes:meta()
    , text        :: datatypes:narrative()
    , extension   :: extensions:extension()
    , identifier_ :: datatypes:identifier()
    , type        :: boolean()                           % TODO code
    , timestamp   :: datatypes:instant()
    , total       :: datatypes:unsigned_int()
    , link        :: [bundlelink()]
    , entry       :: [bundleentry()]
    , signature   :: datatypes:signature()
    }).
-type bundle() :: #bundle{}.

-record(bundlelink, {
           relation :: binary()
         , uri :: datatypes:uri()
         }).
-type bundlelink() :: #bundlelink{}.

-record(bundleentry, {
           link :: [bundlelink()]
         , full_uri :: datatypes:uri()
         , resource :: resource()
         , search   :: bundlesearch()
         , request  :: bundlerequest()
         , reponse  :: bundleresponse()
         }).
-type bundleentry() :: #bundleentry{}.

-record(bundlesearch, {
           mode :: datatypes:code()
         , score :: datatypes:decimal()
         }).
-type bundlesearch() :: #bundlesearch{}.

-record(bundlerequest, {
           method :: binary()                      % code()
         , uri    :: datatypes:uri()
         , if_none_match :: binary()
         , if_modified_since :: datatypes:instant()
         , if_match :: binary()
         , if_none_exist :: binary()
         }).
-type bundlerequest() :: #bundlerequest{}.

-record(bundleresponse, {
           status :: binary()
         , location :: datatypes:uri()
         , etag :: binary()
         , last_modified :: datatypes:instant()
         , outcome :: resource()
         }).
-type bundleresponse() :: #bundleresponse{}.

-type resource() ::
      patient:patient().
%%      careplan()
%%    | careteam()
%%    | composition()
%%    | condition()
%%    | consent()
%%    | encounter()
%%    | episodeocare()
%%    | goal()
%%    | patient()
%%    | requestgroup()
%%    | task().


%%
%% API exports
%%-export([]).
%%
%%====================================================================
%% API functions
%%====================================================================


to_bundle(Props) ->
  DT = complex:get_info(<<"Bundle">>),
  #bundle{
      id          = complex:get_value(<<"id">>, Props, DT)
    , meta        = complex:get_value(<<"meta">>, Props, DT)
    , text        = complex:get_value(<<"text">>, Props, DT)
    , extension   = complex:get_value(<<"Extension">>, Props, DT)
    , identifier_ = complex:get_value(<<"identifier">>, Props, DT)
    , type        = complex:get_value(<<"type">>, Props, DT)        % TODO code
    , timestamp   = complex:get_value(<<"timestamp">>, Props, DT)
    , total       = complex:get_value(<<"total">>, Props, DT)
    , link        = complex:get_value(<<"Bundle.Link">>, Props, DT)
    , entry       = complex:get_value(<<"Bundle.Entry">>, Props, DT)
    , signature   = complex:get_value(<<"signature">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_link({Props}) -> to_link(Props);
to_link(Props) ->
  DT = complex:get_info(<<"Bundle.Link">>),
  #bundlelink{
      relation = complex:get_value(<<"relationship">>, Props, DT)
    , uri  = complex:get_value(<<"uri">>, Props, DT)
    }.

to_entry({Props}) -> to_entry(Props);
to_entry(Props) ->
  DT = complex:get_info(<<"Bundle.Entry">>),
  #bundleentry{
      link      = complex:get_value(<<"link">>,Props, DT)
    , full_uri  = complex:get_value(<<"fullUri">>, Props, DT)
    , resource  = complex:get_value(<<"resource">>,Props, DT)
    , search    = complex:get_value(<<"search">>,Props, DT)
    , request   = complex:get_value(<<"request">>, Props, DT)
    , reponse   = complex:get_value(<<"response">>, Props, DT)
    }.

to_search({Props}) -> to_search(Props);
to_search(Props) ->
    DT = complex:get_info(<<"Bundle.Search">>),
	#bundlesearch{
       mode = complex:get_value(<<"mode">>, Props, DT)
     , score = complex:get_value(<<"score">>, Props, DT)
	 }.

to_request({Props}) -> to_request(Props);
to_request(Props) ->
    DT = complex:get_info(<<"Bundle.Request">>),
    #bundlerequest{
           method        = complex:get_value(<<"mode">>, Props, DT)
         , uri           = complex:get_value(<<"uri">>, Props, DT)
         , if_none_match = complex:get_value(<<"ifNoneMatch">>, Props, DT)
         , if_modified_since = complex:get_value(<<"ifModifiedSince">>, Props, DT)
         , if_match      = complex:get_value(<<"ifMatch">>, Props, DT)
         , if_none_exist = complex:get_value(<<"ifNoneExist">>, Props, DT)
		}.

to_response({Props}) -> to_response(Props);
to_response(Props) ->
    DT = complex:get_info(<<"Bundle.Response">>),
    #bundleresponse{
           status        = complex:get_value(<<"status">>, Props, DT)
         , location      = complex:get_value(<<"location">>, Props, DT)
         , etag          = complex:get_value(<<"etag">>, Props, DT)
         , last_modified = complex:get_value(<<"lastModified">>, Props, DT)
         , outcome       = complex:get_value(<<"outcome">>, Props, DT)
	}.
