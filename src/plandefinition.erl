-module(plandefinition).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('PlanDefinition.DynamicValue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	path :: string() | undefined,
	expression :: complex:'Expression'() | undefined}).

-type 'PlanDefinition.DynamicValue'() :: #'PlanDefinition.DynamicValue'{}.


-record('PlanDefinition.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'ActionParticipantType'(),
	role :: complex:'CodeableConcept'() | undefined}).

-type 'PlanDefinition.Participant'() :: #'PlanDefinition.Participant'{}.


-record('PlanDefinition.RelatedAction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	actionId :: id(),
	relationship :: complex:'ActionRelationshipType'(),
	choice :: complex:'Range'() | complex:'Duration'() | undefined}).

-type 'PlanDefinition.RelatedAction'() :: #'PlanDefinition.RelatedAction'{}.


-record('PlanDefinition.Condition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	kind :: complex:'ActionConditionKind'(),
	expression :: complex:'Expression'() | undefined}).

-type 'PlanDefinition.Condition'() :: #'PlanDefinition.Condition'{}.


-record('PlanDefinition.Action', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	prefix :: string() | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	textEquivalent :: string() | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	reason :: [complex:'CodeableConcept'()] | undefined,
	documentation :: [complex:'RelatedArtifact'()] | undefined,
	goalId :: [id()] | undefined,
	choice :: special:'Reference'() | complex:'CodeableConcept'() | undefined,
	trigger :: [complex:'TriggerDefinition'()] | undefined,
	condition :: [complex:'PlanDefinition.Condition'()] | undefined,
	input :: [complex:'DataRequirement'()] | undefined,
	output :: [complex:'DataRequirement'()] | undefined,
	relatedAction :: [complex:'PlanDefinition.RelatedAction'()] | undefined,
	choice1 :: complex:'Timing'() | complex:'Range'() | complex:'Period'() | complex:'Duration'() | dateTime() | complex:'Age'() | undefined,
	participant :: [complex:'PlanDefinition.Participant'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	groupingBehavior :: complex:'ActionGroupingBehavior'() | undefined,
	selectionBehavior :: complex:'ActionSelectionBehavior'() | undefined,
	requiredBehavior :: complex:'ActionRequiredBehavior'() | undefined,
	precheckBehavior :: complex:'ActionPrecheckBehavior'() | undefined,
	cardinalityBehavior :: complex:'ActionCardinalityBehavior'() | undefined,
	choice2 :: uri() | canonical() | undefined,
	transform :: canonical() | undefined,
	dynamicValue :: [complex:'PlanDefinition.DynamicValue'()] | undefined,
	action :: [complex:'PlanDefinition.Action'()] | undefined}).

-type 'PlanDefinition.Action'() :: #'PlanDefinition.Action'{}.


-record('PlanDefinition.Target', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	measure :: complex:'CodeableConcept'() | undefined,
	choice :: complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'() | undefined,
	due :: complex:'Duration'() | undefined}).

-type 'PlanDefinition.Target'() :: #'PlanDefinition.Target'{}.


-record('PlanDefinition.Goal', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	description :: complex:'CodeableConcept'(),
	priority :: complex:'CodeableConcept'() | undefined,
	start :: complex:'CodeableConcept'() | undefined,
	addresses :: [complex:'CodeableConcept'()] | undefined,
	documentation :: [complex:'RelatedArtifact'()] | undefined,
	target :: [complex:'PlanDefinition.Target'()] | undefined}).

-type 'PlanDefinition.Goal'() :: #'PlanDefinition.Goal'{}.


-record('PlanDefinition', {anyAttribs :: anyAttribs(),
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
	type :: complex:'CodeableConcept'() | undefined,
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
	relatedArtifact :: [metadata:'RelatedArtifact'()] | undefined,
	library :: [canonical()] | undefined,
	goal :: ['PlanDefinition.Goal'()] | undefined,
	action :: ['PlanDefinition.Action'()] | undefined}).

-type 'PlanDefinition'() :: #'PlanDefinition'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_planDefinition({Props}) -> to_planDefinition(Props);
to_planDefinition(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition">>),
  #'PlanDefinition'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url  = decode:value(<<"url">>, Props, DT)
	, 'identifier' = decode:value(<<"identifier">>, Props, DT)
    , version  = decode:value(<<"version">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , title  = decode:value(<<"title">>, Props, DT)
    , subtitle  = decode:value(<<"subtitle">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , experimental  = decode:value(<<"experimental">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , publisher  = decode:value(<<"publisher">>, Props, DT)
    , contact  = decode:value(<<"contact">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , useContext  = decode:value(<<"useContext">>, Props, DT)
    , jurisdiction  = decode:value(<<"jurisdiction">>, Props, DT)
    , purpose  = decode:value(<<"purpose">>, Props, DT)
    , usage  = decode:value(<<"usage">>, Props, DT)
    , copyright  = decode:value(<<"copyright">>, Props, DT)
    , approvalDate  = decode:value(<<"approvalDate">>, Props, DT)
    , lastReviewDate  = decode:value(<<"lastReviewDate">>, Props, DT)
    , effectivePeriod  = decode:value(<<"effectivePeriod">>, Props, DT)
    , topic  = decode:value(<<"topic">>, Props, DT)
    , author  = decode:value(<<"author">>, Props, DT)
    , editor  = decode:value(<<"editor">>, Props, DT)
    , reviewer  = decode:value(<<"reviewer">>, Props, DT)
    , endorser  = decode:value(<<"endorser">>, Props, DT)
    , relatedArtifact  = decode:value(<<"relatedArtifact">>, Props, DT)
    , library  = decode:value(<<"library">>, Props, DT)
    , goal  = decode:value(<<"goal">>, Props, DT)
    , action  = decode:value(<<"action">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_planDefinition_dynamicValue({Props}) -> to_planDefinition_dynamicValue(Props);
to_planDefinition_dynamicValue(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.DynamicValue">>),
  #'PlanDefinition.DynamicValue'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , path  = decode:value(<<"path">>, Props, DT)
    , expression  = decode:value(<<"expression">>, Props, DT)
    }.


to_planDefinition_participant({Props}) -> to_planDefinition_participant(Props);
to_planDefinition_participant(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Participant">>),
  #'PlanDefinition.Participant'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , role  = decode:value(<<"role">>, Props, DT)
    }.


to_planDefinition_relatedAction({Props}) -> to_planDefinition_relatedAction(Props);
to_planDefinition_relatedAction(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.RelatedAction">>),
  #'PlanDefinition.RelatedAction'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , actionId  = decode:value(<<"actionId">>, Props, DT)
    , relationship  = decode:value(<<"relationship">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.


to_planDefinition_condition({Props}) -> to_planDefinition_condition(Props);
to_planDefinition_condition(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Condition">>),
  #'PlanDefinition.Condition'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , kind  = decode:value(<<"kind">>, Props, DT)
    , expression  = decode:value(<<"expression">>, Props, DT)
    }.


to_planDefinition_action({Props}) -> to_planDefinition_action(Props);
to_planDefinition_action(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Action">>),
  #'PlanDefinition.Action'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , prefix  = decode:value(<<"prefix">>, Props, DT)
    , title  = decode:value(<<"title">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , textEquivalent  = decode:value(<<"textEquivalent">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , reason  = decode:value(<<"reason">>, Props, DT)
    , documentation  = decode:value(<<"documentation">>, Props, DT)
    , goalId  = decode:value(<<"goalId">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , trigger  = decode:value(<<"trigger">>, Props, DT)
    , condition  = decode:value(<<"condition">>, Props, DT)
    , input  = decode:value(<<"input">>, Props, DT)
    , output  = decode:value(<<"output">>, Props, DT)
    , relatedAction  = decode:value(<<"relatedAction">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , participant  = decode:value(<<"participant">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , groupingBehavior  = decode:value(<<"groupingBehavior">>, Props, DT)
    , selectionBehavior  = decode:value(<<"selectionBehavior">>, Props, DT)
    , requiredBehavior  = decode:value(<<"requiredBehavior">>, Props, DT)
    , precheckBehavior  = decode:value(<<"precheckBehavior">>, Props, DT)
    , cardinalityBehavior  = decode:value(<<"cardinalityBehavior">>, Props, DT)
    , choice2  = decode:value(<<"choice2">>, Props, DT)
    , transform  = decode:value(<<"transform">>, Props, DT)
    , dynamicValue  = decode:value(<<"dynamicValue">>, Props, DT)
    , action  = decode:value(<<"action">>, Props, DT)
    }.


to_planDefinition_target({Props}) -> to_planDefinition_target(Props);
to_planDefinition_target(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Target">>),
  #'PlanDefinition.Target'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , measure  = decode:value(<<"measure">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , due  = decode:value(<<"due">>, Props, DT)
    }.


to_planDefinition_goal({Props}) -> to_planDefinition_goal(Props);
to_planDefinition_goal(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Goal">>),
  #'PlanDefinition.Goal'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , start  = decode:value(<<"start">>, Props, DT)
    , addresses  = decode:value(<<"addresses">>, Props, DT)
    , documentation  = decode:value(<<"documentation">>, Props, DT)
    , target  = decode:value(<<"target">>, Props, DT)
    }.



text(#'PlanDefinition'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, planDefinition:to_planDefinition(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

planDefinition_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'PlanDefinition',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
planDefinition_toprop_test() ->
    ?asrtp({'PlanDefinition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"PlanDefinition">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

planDefinition_json_test() ->
    ?asrtjson({'PlanDefinition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"PlanDefinition\",\"id\":\"p-21666\"}">>).

-endif.



