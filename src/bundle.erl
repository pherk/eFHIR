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
    , link        :: [link_()]
    , entry       :: [entry()]
    , signature   :: datatypes:signature()
    }).
-type bundle() :: #bundle{}.

-record(link, {
           relation :: binary()
         , uri :: datatypes:uri()
         }).
-type link_() :: #link{}.

-record(entry, {
           link :: [link_()]
         , full_uri :: datatypes:uri()
         , resource :: datatypes:resource()
         , search   :: search()
         , request  :: request()
         , reponse  :: response()
         }).
-type entry() :: #entry{}.

-record(search, {
           mode :: datatypes:code()
         , score :: datatypes:decimal()
         }).
-type search() :: #search{}.

-record(request, {
           method :: binary()                      % code()
         , uri    :: datatypes:uri()
         , if_none_match :: binary()
         , if_modified_since :: datatypes:instant()
         , if_match :: binary()
         , if_none_exist :: binary()
         }).
-type request() :: #request{}.

-record(response, {
           status :: binary()
         , location :: datatypes:uri()
         , etag :: binary()
         , last_modified :: datatypes:instant()
         , outcome :: dfatatype:resource()
         }).
-type response() :: #response{}.

- type resource() ::
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
  #bundle{
      id          = proplists:get_value(<<"id">>, Props)
    , meta        = datatypes:to_meta(<<"meta">>, Props)
    , text        = datatypes:to_narrative(<<"text">>, Props)
    , extension   = extension:to_extension_list(Props)
    , identifier_ = datatypes:to_identifier_list(<<"identifier">>, Props)
    , type        = proplists:get_value(<<"id">>, Props)        % TODO code
    , timestamp   = datatypes:to_instant(<<"timestamp">>, Props)
    , total       = datatypes:to_unsigned_int(<<"total">>, Props)
    , link        = to_link_list(<<"link">>, Props)
    , entry       = to_entry_list(<<"entry">>, Props)
    , signature   = datatypes:signature(<<"signature">>, Props)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_link_list(Key, Props) ->
    List = proplists:get_value(Key, Props),
    case List of
        undefined -> [];
            _  -> lists:map(fun to_link/1, List)
    end.

to_link({Props}) -> to_link(Props);
to_link(Props) ->
  #link{
      relation = proplists:get_value(<<"relationship">>, Props)
    , uri  = datatypes:to_uri(<<"uri">>, Props)
    }.

to_entry_list(Key, Props) ->
    List = proplists:get_value(Key, Props),
    case List of
        undefined -> [];
            _  -> lists:map(fun to_entry/1, List)
    end.

to_entry({Props}) -> to_entry(Props);
to_entry(Props) ->
  #entry{
      link      = to_link_list(<<"link">>,Props)
    , full_uri  = datatypes:to_uri(<<"fullUri">>, Props)
    , resource  = to_resource_list(<<"resource">>,Props)
    , search    = to_search(<<"search">>,Props)
    , request   = to_request(<<"request">>, Props)
    , reponse   = to_response(<<"response">>, Props)
    }.

to_search(Key, Props) ->
    List = proplists:get_value(Key, Props),	
    case List of
        undefined -> undefined;
        _  -> to_search(List)
    end.
to_search({Props}) -> to_search(Props);
to_search(Props) ->
	#search{
       mode = proplists:get_value(<<"mode">>, Props)
     , score = datatypes:to_decimal(<<"score">>, Props)
	 }.

to_request(Key, Props) ->
    List = proplists:get_value(Key, Props),	
    case List of
        undefined -> undefined;
        _  -> to_request(List)
    end.
to_request({Props}) -> to_request(Props);
to_request(Props) ->
    #request{
           method = proplists:get_value(<<"mode">>, Props)
         , uri    = datatypes:to_uri(<<"uri">>, Props)
         , if_none_match = proplists:get_value(<<"ifNoneMatch">>, Props)
         , if_modified_since = datatypes:instant(<<"ifModifiedSince">>, Props)
         , if_match  = proplists:get_value(<<"ifMatch">>, Props)
         , if_none_exist = proplists:get_value(<<"ifNoneExist">>, Props)
		}.

to_response(Key, Props) ->
    List = proplists:get_value(Key, Props),	
    case List of
        undefined -> undefined;
        _  -> to_response(List)
    end.
to_response({Props}) -> to_response(Props);
to_response(Props) ->
    #response{
           status = proplists:get_value(<<"status">>, Props)
         , location = datatypes:to_uri(<<"location">>, Props)
         , etag = proplists:get_value(<<"etag">>, Props)
         , last_modified = datatypes:instant(<<"lastModified">>, Props)
         , outcome = resource:resource(<<"outcode">>, Props)
	}.

to_resource_list(Key, Props) ->
    List = proplists:get_value(Key, Props),
    case List of
        undefined -> [];
            _  -> lists:map(fun to_resource/1, List)
    end.

to_resource({Props}) -> to_resource(Props);
to_resource(Props) ->
   Rtype = proplists:get_value(<<"resource_type">>, Props),
   to_resource(Rtype, Props). 

to_resource(<<"Patient">>, Props) -> patient:to_patient(Props).
