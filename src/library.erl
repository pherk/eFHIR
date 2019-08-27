-module(library).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

%%
%% Types
%%
-record('Library', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: complex:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: complex:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extension:'Extension'()] | undefined,
	url :: uri() | undefined,
	'identifier' :: [complex:'Identifier'()] | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	subtitle :: string() | undefined,
	status :: code(),
	experimental :: boolean() | undefined,
	type :: complex:'CodeableConcept'(),
	subject ::special:'Reference'() | complex:'CodeableConcept'() | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: [metadata:'ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: [metadata:'UsageContext'()] | undefined,
	jurisdiction :: [complex:'CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	usage :: string() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: complex:'Period'() | undefined,
	topic :: [complex:'CodeableConcept'()] | undefined,
	author :: [metadata:'ContactDetail'()] | undefined,
	editor :: [metadata:'ContactDetail'()] | undefined,
	reviewer :: [metadata:'ContactDetail'()] | undefined,
	endorser :: [metadata:'ContactDetail'()] | undefined,
	relatedArtifact :: [metadata:'RelatedArtifact'()] | undefined,
	parameter :: [metadata:'ParameterDefinition'()] | undefined,
	dataRequirement :: [metadata:'DataRequirement'()] | undefined,
	content :: [complex:'Attachment'()] | undefined}).

-type 'Library'() :: #'Library'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_library({Props}) -> to_library(Props);
to_library(Props) ->
  DT = decode:xsd_info(<<"Library">>),
  #'Library'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , 'identifier' = decode:value(<<"identifier">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , title = decode:value(<<"title">>, Props, DT)
    , subtitle = decode:value(<<"subtitle">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , experimental = decode:value(<<"experimental">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , publisher = decode:value(<<"publisher">>, Props, DT)
    , contact = decode:value(<<"contact">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , useContext = decode:value(<<"useContext">>, Props, DT)
    , jurisdiction = decode:value(<<"jurisdiction">>, Props, DT)
    , purpose = decode:value(<<"purpose">>, Props, DT)
    , usage = decode:value(<<"usage">>, Props, DT)
    , copyright = decode:value(<<"copyright">>, Props, DT)
    , approvalDate = decode:value(<<"approvalDate">>, Props, DT)
    , lastReviewDate = decode:value(<<"lastReviewDate">>, Props, DT)
    , effectivePeriod = decode:value(<<"effectivePeriod">>, Props, DT)
    , topic = decode:value(<<"topic">>, Props, DT)
    , author = decode:value(<<"author">>, Props, DT)
    , editor = decode:value(<<"editor">>, Props, DT)
    , reviewer = decode:value(<<"reviewer">>, Props, DT)
    , endorser = decode:value(<<"endorser">>, Props, DT)
    , relatedArtifact = decode:value(<<"relatedArtifact">>, Props, DT)
    , parameter = decode:value(<<"parameter">>, Props, DT)
    , dataRequirement = decode:value(<<"dataRequirement">>, Props, DT)
    , content = decode:value(<<"content">>, Props, DT)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

text(#'Library'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, library:to_library(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

library_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>},
             {<<"status">>, <<"active">>},
             {<<"type">>, {[{<<"coding">>, [{[{<<"code">>, <<"logic-library">>}]}]}]}}
            ],
            {'Library',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             undefined,[],undefined,undefined, undefined,undefined,<<"active">>,undefined,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"logic-library">>,undefined,undefined}],
                 undefined},
             undefined,undefined,undefined,[],undefined,[],[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],[],[],[],[],[],[],[],[]}
           ).

library_toprop_test() ->
    ?asrtp(
            {'Library',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             undefined,[],undefined,undefined, undefined,undefined,<<"active">>,undefined,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"logic-library">>,undefined,undefined}],
                 undefined},
             undefined,undefined,undefined,[],undefined,[],[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],[],[],[],[],[],[],[],[]},
           {[{<<"resourceType">>,<<"Library">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"active">>},
              {<<"type">>, {[{<<"coding">>, [{[{<<"code">>,<<"logic-library">>}]}]}]}}]}
            ).

library_json_test() ->
    ?asrtjson(
            {'Library',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             undefined,[],undefined,undefined, undefined,undefined,<<"active">>,undefined,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"logic-library">>,undefined,undefined}],
                 undefined},
             undefined,undefined,undefined,[],undefined,[],[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],[],[],[],[],[],[],[],[]},
            <<"{\"resourceType\":\"Library\",\"id\":\"p-21666\",\"status\":\"active\",\"type\":{\"coding\":[{\"code\":\"logic-library\"}]}}">>
      ).

-endif.


