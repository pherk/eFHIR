-module(careplan).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('CarePlan.Detail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	kind :: complex:'CarePlanActivityKind'() | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	goal :: [special:'Reference'()] | undefined,
	status :: complex:'CarePlanActivityStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice :: complex:'Timing'() | string() | complex:'Period'() | undefined,
	location :: special:'Reference'() | undefined,
	performer :: [special:'Reference'()] | undefined,
	choice1 :: special:'Reference'() | complex:'CodeableConcept'() | undefined,
	dailyAmount :: complex:'Quantity'() | undefined,
	quantity :: complex:'Quantity'() | undefined,
	description :: string() | undefined}).

-type 'CarePlan.Detail'() :: #'CarePlan.Detail'{}.


-record('CarePlan.Activity', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	outcomeCodeableConcept :: [complex:'CodeableConcept'()] | undefined,
	outcomeReference :: [special:'Reference'()] | undefined,
	progress :: [complex:'Annotation'()] | undefined,
	reference :: special:'Reference'() | undefined,
	detail :: 'CarePlan.Detail'() | undefined}).

-type 'CarePlan.Activity'() :: #'CarePlan.Activity'{}.


-record('CarePlan', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	replaces :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: code(),
	intent :: code(),
	category :: [complex:'CodeableConcept'()] | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	period :: complex:'Period'() | undefined,
	created :: dateTime() | undefined,
	author :: special:'Reference'() | undefined,
	contributor :: [special:'Reference'()] | undefined,
	careTeam :: [special:'Reference'()] | undefined,
	addresses :: [special:'Reference'()] | undefined,
	supportingInfo :: [special:'Reference'()] | undefined,
	goal :: [special:'Reference'()] | undefined,
	activity :: ['CarePlan.Activity'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).

-type 'CarePlan'() :: #'CarePlan'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_carePlan({Props}) -> to_carePlan(Props);
to_carePlan(Props) ->
  DT = decode:xsd_info(<<"CarePlan">>),
  #'CarePlan'{ 
      id               = decode:value(<<"id">>, Props, DT)
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
    , partOf  = decode:value(<<"partOf">>, Props, DT) 
    , status  = decode:value(<<"status">>, Props, DT) 
    , intent  = decode:value(<<"intent">>, Props, DT) 
    , category  = decode:value(<<"category">>, Props, DT) 
    , title  = decode:value(<<"title">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    , subject  = decode:value(<<"subject">>, Props, DT) 
    , encounter  = decode:value(<<"encounter">>, Props, DT) 
    , period  = decode:value(<<"period">>, Props, DT) 
    , created  = decode:value(<<"created">>, Props, DT) 
    , author  = decode:value(<<"author">>, Props, DT) 
    , contributor  = decode:value(<<"contributor">>, Props, DT) 
    , careTeam  = decode:value(<<"careTeam">>, Props, DT) 
    , addresses  = decode:value(<<"addresses">>, Props, DT) 
    , supportingInfo  = decode:value(<<"supportingInfo">>, Props, DT) 
    , goal  = decode:value(<<"goal">>, Props, DT) 
    , activity  = decode:value(<<"activity">>, Props, DT) 
    , note  = decode:value(<<"note">>, Props, DT) 
    }.


%%====================================================================
%% Internal functions
%%====================================================================
carePlan_detail({Props}) -> carePlan_detail(Props);
carePlan_detail(Props) ->
  DT = decode:xsd_info(<<"CarePlan.Detail">>),
  #'CarePlan.Detail'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , kind  = decode:value(<<"kind">>, Props, DT) 
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT) 
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT) 
    , code  = decode:value(<<"code">>, Props, DT) 
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT) 
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT) 
    , goal  = decode:value(<<"goal">>, Props, DT) 
    , status  = decode:value(<<"status">>, Props, DT) 
    , statusReason  = decode:value(<<"statusReason">>, Props, DT) 
    , doNotPerform  = decode:value(<<"doNotPerform">>, Props, DT) 
    , choice  = decode:value(<<"choice">>, Props, DT) 
    , location  = decode:value(<<"location">>, Props, DT) 
    , performer  = decode:value(<<"performer">>, Props, DT) 
    , choice1  = decode:value(<<"choice1">>, Props, DT) 
    , dailyAmount  = decode:value(<<"dailyAmount">>, Props, DT) 
    , quantity  = decode:value(<<"quantity">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    }.


carePlan_activity({Props}) -> carePlan_activity(Props);
carePlan_activity(Props) ->
  DT = decode:xsd_info(<<"CarePlan.Activity">>),
  #'CarePlan.Activity'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , outcomeCodeableConcept  = decode:value(<<"outcomeCodeableConcept">>, Props, DT) 
    , outcomeReference  = decode:value(<<"outcomeReference">>, Props, DT) 
    , progress  = decode:value(<<"progress">>, Props, DT) 
    , reference  = decode:value(<<"reference">>, Props, DT) 
    , detail  = decode:value(<<"detail">>, Props, DT) 
    }.


text(#'CarePlan'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, careplan:to_carePlan(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

carePlan_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"status">>, <<"active">>},
             {<<"intent">>, <<"plan">>},
             {<<"subject">>, {[{<<"reference">>, <<"nabu/Patient/p-21666">>}]}}
            ],
            {'CarePlan',undefined,<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[], <<"active">>,<<"plan">>,[],undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,undefined,[],[],[],[],[], [],[]}
           ).

carePlan_toprop_test() ->
    ?asrtp(
            {'CarePlan',undefined,<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[], <<"active">>,<<"plan">>,[],undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,undefined,[],[],[],[],[], [],[]},
           {[{<<"resourceType">>,<<"CarePlan">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"active">>},
              {<<"intent">>,<<"plan">>},
              {<<"subject">>, {[{<<"reference">>,<<"nabu/Patient/p-21666">>}]}}]}
            ).

carePlan_json_test() ->
    ?asrtjson(
            {'CarePlan',undefined,<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[], <<"active">>,<<"plan">>,[],undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,undefined,[],[],[],[],[], [],[]},
            <<"{\"resourceType\":\"CarePlan\",\"id\":\"p-21666\",\"status\":\"active\",\"intent\":\"plan\",\"subject\":{\"reference\":\"nabu/Patient/p-21666\"}}">>
      ).

-endif.




