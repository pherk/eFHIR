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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
	url :: uri() | undefined,
	'identifier' :: [complex:'Identifier'()] | undefined,
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
	relatedArtifact :: [complex:'RelatedArtifact'()] | undefined,
	library :: [canonical()] | undefined,
	goal :: ['PlanDefinition.Goal'()] | undefined,
	action :: ['PlanDefinition.Action'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_planDefinition.DynamicValue({Props}) -> to_planDefinition.DynamicValue(Props);
to_planDefinition.DynamicValue(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.DynamicValue">>),
  #'PlanDefinition.DynamicValue'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	path :: string() | undefined,
	expression :: complex:'Expression'() | undefined}).
    }.


to_planDefinition.Participant({Props}) -> to_planDefinition.Participant(Props);
to_planDefinition.Participant(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Participant">>),
  #'PlanDefinition.Participant'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'ActionParticipantType'(),
	role :: complex:'CodeableConcept'() | undefined}).
    }.


to_planDefinition.RelatedAction({Props}) -> to_planDefinition.RelatedAction(Props);
to_planDefinition.RelatedAction(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.RelatedAction">>),
  #'PlanDefinition.RelatedAction'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	actionId :: id(),
	relationship :: complex:'ActionRelationshipType'(),
	choice :: complex:'Range'() | complex:'Duration'() | undefined}).
    }.


to_planDefinition.Condition({Props}) -> to_planDefinition.Condition(Props);
to_planDefinition.Condition(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Condition">>),
  #'PlanDefinition.Condition'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	kind :: complex:'ActionConditionKind'(),
	expression :: complex:'Expression'() | undefined}).
    }.


to_planDefinition.Action({Props}) -> to_planDefinition.Action(Props);
to_planDefinition.Action(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Action">>),
  #'PlanDefinition.Action'{ 
    anyAttribs :: anyAttribs(),
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
	condition :: ['PlanDefinition.Condition'()] | undefined,
	input :: [complex:'DataRequirement'()] | undefined,
	output :: [complex:'DataRequirement'()] | undefined,
	relatedAction :: ['PlanDefinition.RelatedAction'()] | undefined,
	choice1 :: complex:'Timing'() | complex:'Range'() | complex:'Period'() | complex:'Duration'() | dateTime() | complex:'Age'() | undefined,
	participant :: ['PlanDefinition.Participant'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	groupingBehavior :: complex:'ActionGroupingBehavior'() | undefined,
	selectionBehavior :: complex:'ActionSelectionBehavior'() | undefined,
	requiredBehavior :: complex:'ActionRequiredBehavior'() | undefined,
	precheckBehavior :: complex:'ActionPrecheckBehavior'() | undefined,
	cardinalityBehavior :: complex:'ActionCardinalityBehavior'() | undefined,
	choice2 :: uri() | canonical() | undefined,
	transform :: canonical() | undefined,
	dynamicValue :: ['PlanDefinition.DynamicValue'()] | undefined,
	action :: ['PlanDefinition.Action'()] | undefined}).
    }.


to_planDefinition.Target({Props}) -> to_planDefinition.Target(Props);
to_planDefinition.Target(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Target">>),
  #'PlanDefinition.Target'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	measure :: complex:'CodeableConcept'() | undefined,
	choice :: complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'() | undefined,
	due :: complex:'Duration'() | undefined}).
    }.


to_planDefinition.Goal({Props}) -> to_planDefinition.Goal(Props);
to_planDefinition.Goal(Props) ->
  DT = decode:xsd_info(<<"PlanDefinition.Goal">>),
  #'PlanDefinition.Goal'{ 
    anyAttribs :: anyAttribs(),
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



