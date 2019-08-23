-module(requestgroup).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('RequestGroup.RelatedAction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	actionId :: id(),
	relationship :: complex:'ActionRelationshipType'(),
	choice :: complex:'Range'() | complex:'Duration'() | undefined}).

-type 'RequestGroup.RelatedAction'() :: #'RequestGroup.RelatedAction'{}.


-record('RequestGroup.Condition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	kind :: complex:'ActionConditionKind'(),
	expression :: complex:'Expression'() | undefined}).

-type 'RequestGroup.Condition'() :: #'RequestGroup.Condition'{}.


-record('RequestGroup.Action', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	prefix :: string() | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	textEquivalent :: string() | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	documentation :: [complex:'RelatedArtifact'()] | undefined,
	condition :: [complex:'RequestGroup.Condition'()] | undefined,
	relatedAction :: [complex:'RequestGroup.RelatedAction'()] | undefined,
	choice :: complex:'Timing'() | complex:'Range'() | complex:'Period'() | complex:'Duration'() | dateTime() | complex:'Age'() | undefined,
	participant :: [special:'Reference'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	groupingBehavior :: complex:'ActionGroupingBehavior'() | undefined,
	selectionBehavior :: complex:'ActionSelectionBehavior'() | undefined,
	requiredBehavior :: complex:'ActionRequiredBehavior'() | undefined,
	precheckBehavior :: complex:'ActionPrecheckBehavior'() | undefined,
	cardinalityBehavior :: complex:'ActionCardinalityBehavior'() | undefined,
	resource :: special:'Reference'() | undefined,
	action :: [complex:'RequestGroup.Action'()] | undefined}).

-type 'RequestGroup.Action'() :: #'RequestGroup.Action'{}.


-record('RequestGroup', {
    anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	replaces :: [special:'Reference'()] | undefined,
	groupIdentifier :: complex:'Identifier'() | undefined,
	status :: complex:'RequestStatus'(),
	intent :: complex:'RequestIntent'(),
	priority :: complex:'RequestPriority'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	authoredOn :: dateTime() | undefined,
	author :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	action :: [complex:'RequestGroup.Action'()] | undefined}).

-type 'RequestGroup'() :: #'RequestGroup'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_requestGroup({Props}) -> to_requestGroup(Props);
to_requestGroup(Props) ->
  DT = decode:xsd_info(<<"RequestGroup">>),
  #'RequestGroup'{ 
      anyAttribs       = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , replaces  = decode:value(<<"replaces">>, Props, DT)
    , groupIdentifier  = decode:value(<<"groupIdentifier">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , intent  = decode:value(<<"intent">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , authoredOn  = decode:value(<<"authoredOn">>, Props, DT)
    , author  = decode:value(<<"author">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , action  = decode:value(<<"action">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_requestGroup_relatedAction({Props}) -> to_requestGroup_relatedAction(Props);
to_requestGroup_relatedAction(Props) ->
  DT = decode:xsd_info(<<"RequestGroup.RelatedAction">>),
  #'RequestGroup.RelatedAction'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , actionId  = decode:value(<<"actionId">>, Props, DT)
    , relationship  = decode:value(<<"relationship">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.


to_requestGroup_condition({Props}) -> to_requestGroup_condition(Props);
to_requestGroup_condition(Props) ->
  DT = decode:xsd_info(<<"RequestGroup.Condition">>),
  #'RequestGroup.Condition'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , kind  = decode:value(<<"kind">>, Props, DT)
    , expression  = decode:value(<<"expression">>, Props, DT)
    }.


to_requestGroup_action({Props}) -> to_requestGroup_action(Props);
to_requestGroup_action(Props) ->
  DT = decode:xsd_info(<<"RequestGroup.Action">>),
  #'RequestGroup.Action'{ 
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
    , documentation  = decode:value(<<"documentation">>, Props, DT)
    , condition  = decode:value(<<"condition">>, Props, DT)
    , relatedAction  = decode:value(<<"relatedAction">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , participant  = decode:value(<<"participant">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , groupingBehavior  = decode:value(<<"groupingBehavior">>, Props, DT)
    , selectionBehavior  = decode:value(<<"selectionBehavior">>, Props, DT)
    , requiredBehavior  = decode:value(<<"requiredBehavior">>, Props, DT)
    , precheckBehavior  = decode:value(<<"precheckBehavior">>, Props, DT)
    , cardinalityBehavior  = decode:value(<<"cardinalityBehavior">>, Props, DT)
    , resource  = decode:value(<<"resource">>, Props, DT)
    , action  = decode:value(<<"action">>, Props, DT)
    }.



text(#'RequestGroup'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, requestgroup:to_requestGroup(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

requestGroup_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>},{<<"status">>,<<"requested">>},{<<"intent">>,<<"order">>}],
         {'RequestGroup',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],[],
          [],[],[],[], undefined,<<"requested">>,<<"order">>,undefined,undefined,
                                 undefined,undefined,undefined,undefined,[],
                                 [],[],[]}).
requestGroup_toprop_test() ->
    ?asrtp(
         {'RequestGroup',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],[],
          [],[],[],[], undefined,<<"requested">>,<<"order">>,undefined,undefined,
                                 undefined,undefined,undefined,undefined,[],
                                 [],[],[]},
           {[{<<"resourceType">>,<<"RequestGroup">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"requested">>},
              {<<"intent">>,<<"order">>}
            ]}).

requestGroup_json_test() ->
    ?asrtjson({'RequestGroup',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"RequestGroup\",\"id\":\"p-21666\"}">>).

-endif.




