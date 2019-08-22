-module(activitydefinition).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('ActivityDefinition.DynamicValue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	path :: string(),
	expression :: complex:'Expression'()}).

-type 'ActivityDefinition.DynamicValue'() :: #'ActivityDefinition.DynamicValue'{}.


-record('ActivityDefinition.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'ActionParticipantType'(),
	role :: complex:'CodeableConcept'() | undefined}).

-type 'ActivityDefinition.Participant'() :: #'ActivityDefinition.Participant'{}.


-record('ActivityDefinition', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	url :: uri() | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	subtitle :: string() | undefined,
	status :: complex:'PublicationStatus'(),
	experimental :: boolean() | undefined,
	choice :: special:'Reference'() | complex:'CodeableConcept'() | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: [complex:'ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: [complex:'UsageContext'()] | undefined,
	jurisdiction :: [complex:'CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	usage :: string() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: complex:'Period'() | undefined,
	topic :: [complex:'CodeableConcept'()] | undefined,
	author :: [complex:'ContactDetail'()] | undefined,
	editor :: [complex:'ContactDetail'()] | undefined,
	reviewer :: [complex:'ContactDetail'()] | undefined,
	endorser :: [complex:'ContactDetail'()] | undefined,
	relatedArtifact :: [complex:'RelatedArtifact'()] | undefined,
	library :: [canonical()] | undefined,
	kind :: complex:'RequestResourceType'() | undefined,
	profile :: canonical() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	intent :: complex:'RequestIntent'() | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice1 :: complex:'Timing'() | complex:'Range'() | complex:'Period'() | complex:'Duration'() | dateTime() | complex:'Age'() | undefined,
	location :: special:'Reference'() | undefined,
	participant :: [complex:'ActivityDefinition.Participant'()] | undefined,
	choice2 :: special:'Reference'() | complex:'CodeableConcept'() | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	dosage :: [complex:'Dosage'()] | undefined,
	bodySite :: [complex:'CodeableConcept'()] | undefined,
	specimenRequirement :: [special:'Reference'()] | undefined,
	observationRequirement :: [special:'Reference'()] | undefined,
	observationResultRequirement :: [special:'Reference'()] | undefined,
	transform :: canonical() | undefined,
	dynamicValue :: [complex:'ActivityDefinition.DynamicValue'()] | undefined}).

-type 'ActivityDefinition'() :: #'ActivityDefinition'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_activityDefinition({Props}) -> to_activityDefinition(Props);
to_activityDefinition(Props) ->
  DT = decode:xsd_info(<<"ActivityDefinition">>),
  #'ActivityDefinition'{ 
      anyAttribs :: anyAttribs(),
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , identifier = decode:value(<<"identifier">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , title = decode:value(<<"title">>, Props, DT)
    , subtitle = decode:value(<<"subtitle">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , experimental = decode:value(<<"experimental">>, Props, DT)
    , choice = decode:value(<<"choice">>, Props, DT)
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
    , library = decode:value(<<"library">>, Props, DT)
    , kind = decode:value(<<"kind">>, Props, DT)
    , profile = decode:value(<<"profile">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , intent = decode:value(<<"intent">>, Props, DT)
    , priority = decode:value(<<"priority">>, Props, DT)
    , doNotPerform = decode:value(<<"doNotPerform">>, Props, DT)
    , choice1 = decode:value(<<"choice1">>, Props, DT)
    , location = decode:value(<<"location">>, Props, DT)
    , participant = decode:value(<<"participant">>, Props, DT)
    , choice2 = decode:value(<<"choice2">>, Props, DT)
    , quantity = decode:value(<<"quantity">>, Props, DT)
    , dosage = decode:value(<<"dosage">>, Props, DT)
    , bodySite = decode:value(<<"bodySite">>, Props, DT)
    , specimenRequirement = decode:value(<<"specimenRequirement">>, Props, DT)
    , observationRequirement = decode:value(<<"observationRequirement">>, Props, DT)
    , observationResultRequirement = decode:value(<<"observationResultRequirement">>, Props, DT)
    , transform = decode:value(<<"transform">>, Props, DT)
    , dynamicValue = decode:value(<<"dynamicValue">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_activityDefinition_dynamicValue({Props}) -> to_activityDefinition_dynamicValue(Props);
to_activityDefinition_dynamicValue(Props) ->
  DT = decode:xsd_info(<<"ActivityDefinition.DynamicValue">>),
  #'ActivityDefinition.DynamicValue'{ 
      anyAttribs :: anyAttribs(),
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , path = decode:value(<<"path">>, Props, DT)
    , expression = decode:value(<<"expression">>, Props, DT)
    }.


to_activityDefinition_participant({Props}) -> to_activityDefinition_participant(Props);
to_activityDefinition_participant(Props) ->
  DT = decode:xsd_info(<<"ActivityDefinition.Participant">>),
  #'ActivityDefinition.Participant'{ 
      anyAttribs :: anyAttribs(),
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , role = decode:value(<<"role">>, Props, DT)
    }.



text(#'ActivityDefinition'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, activitydefinition:to_activitydefinition(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

activitydefinition_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'ActivityDefinition',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
activitydefinition_toprop_test() ->
    ?asrtp({'ActivityDefinition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"ActivityDefinition">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

activitydefinition_json_test() ->
    ?asrtjson({'ActivityDefinition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"ActivityDefinition\",\"id\":\"p-21666\"}">>).

-endif.


