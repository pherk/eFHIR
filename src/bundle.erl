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
  DT = types:get_info(<<"Bundle">>),
  #bundle{
      id          = types:get_value(<<"id">>, Props, DT)
    , meta        = types:get_value(<<"meta">>, Props, DT)
    , text        = types:get_value(<<"text">>, Props, DT)
    , extension   = types:get_value(<<"Extension">>, Props, DT)
    , identifier_ = types:get_value(<<"identifier">>, Props, DT)
    , type        = types:get_value(<<"type">>, Props, DT)        % TODO code
    , timestamp   = types:get_value(<<"timestamp">>, Props, DT)
    , total       = types:get_value(<<"total">>, Props, DT)
    , link        = types:get_value(<<"Bundle.Link">>, Props, DT)
    , entry       = types:get_value(<<"Bundle.Entry">>, Props, DT)
    , signature   = types:get_value(<<"signature">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_link({Props}) -> to_link(Props);
to_link(Props) ->
  DT = types:get_info(<<"Bundle.Link">>),
  #bundlelink{
      relation = types:get_value(<<"relationship">>, Props, DT)
    , uri  = types:get_value(<<"uri">>, Props, DT)
    }.

to_entry({Props}) -> to_entry(Props);
to_entry(Props) ->
  DT = types:get_info(<<"Bundle.Entry">>),
  #bundleentry{
      link      = types:get_value(<<"link">>,Props, DT)
    , full_uri  = types:get_value(<<"fullUri">>, Props, DT)
    , resource  = types:get_value(<<"resource">>,Props, DT)
    , search    = types:get_value(<<"search">>,Props, DT)
    , request   = types:get_value(<<"request">>, Props, DT)
    , reponse   = types:get_value(<<"response">>, Props, DT)
    }.

to_search({Props}) -> to_search(Props);
to_search(Props) ->
    DT = types:get_info(<<"Bundle.Search">>),
	#bundlesearch{
       mode = types:get_value(<<"mode">>, Props, DT)
     , score = types:get_value(<<"score">>, Props, DT)
	 }.

to_request({Props}) -> to_request(Props);
to_request(Props) ->
    DT = types:get_info(<<"Bundle.Request">>),
    #bundlerequest{
           method        = types:get_value(<<"mode">>, Props, DT)
         , uri           = types:get_value(<<"uri">>, Props, DT)
         , if_none_match = types:get_value(<<"ifNoneMatch">>, Props, DT)
         , if_modified_since = types:get_value(<<"ifModifiedSince">>, Props, DT)
         , if_match      = types:get_value(<<"ifMatch">>, Props, DT)
         , if_none_exist = types:get_value(<<"ifNoneExist">>, Props, DT)
		}.

to_response({Props}) -> to_response(Props);
to_response(Props) ->
    DT = types:get_info(<<"Bundle.Response">>),
    #bundleresponse{
           status        = types:get_value(<<"status">>, Props, DT)
         , location      = types:get_value(<<"location">>, Props, DT)
         , etag          = types:get_value(<<"etag">>, Props, DT)
         , last_modified = types:get_value(<<"lastModified">>, Props, DT)
         , outcome       = types:get_value(<<"outcome">>, Props, DT)
	}.
