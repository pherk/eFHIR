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
	dailyAmount :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
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
	detail :: complex:'CarePlan.Detail'() | undefined}).

-type 'CarePlan.Activity'() :: #'CarePlan.Activity'{}.


-record('CarePlan', {anyAttribs :: anyAttribs(),
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
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'RequestStatus'(),
	intent :: complex:'CarePlanIntent'(),
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
	activity :: [complex:'CarePlan.Activity'()] | undefined,
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
    }.


%%====================================================================
%% Internal functions
%%====================================================================
carePlan_detail({Props}) -> carePlan_detail(Props);
carePlan_detail(Props) ->
  DT = decode:xsd_info(<<"CarePlan.Detail">>),
  #'CarePlan.Detail'{ 
    anyAttribs :: anyAttribs(),
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
	dailyAmount :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	description :: string() | undefined}).
    }.


carePlan_activity({Props}) -> carePlan_activity(Props);
carePlan_activity(Props) ->
  DT = decode:xsd_info(<<"CarePlan.Activity">>),
  #'CarePlan.Activity'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	outcomeCodeableConcept :: [complex:'CodeableConcept'()] | undefined,
	outcomeReference :: [special:'Reference'()] | undefined,
	progress :: [complex:'Annotation'()] | undefined,
	reference :: special:'Reference'() | undefined,
	detail :: complex:'CarePlan.Detail'() | undefined}).
    }.


text(#'CarePlan'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, carePlan:to_carePlan(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

carePlan_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'CarePlan',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
carePlan_toprop_test() ->
    ?asrtp({'CarePlan',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"CarePlan">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

carePlan_json_test() ->
    ?asrtjson({'CarePlan',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"CarePlan\",\"id\":\"p-21666\"}">>).

-endif.




