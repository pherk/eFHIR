-module(observation).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Observation.Component', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: complex:'CodeableConcept'(),
	choice :: time() | string() | complex:'SampledData'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'Period'() | integer() | dateTime() | complex:'CodeableConcept'() | boolean() | undefined,
	dataAbsentReason :: complex:'CodeableConcept'() | undefined,
	interpretation :: [complex:'CodeableConcept'()] | undefined,
	referenceRange :: [complex:'Observation.ReferenceRange'()] | undefined}).

-type 'Observation.Component'() :: #'Observation.Component'{}.


-record('Observation.ReferenceRange', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	low :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	high :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	appliesTo :: [complex:'CodeableConcept'()] | undefined,
	age :: complex:'Range'() | undefined,
	text :: string() | undefined}).

-type 'Observation.ReferenceRange'() :: #'Observation.ReferenceRange'{}.


-record('Observation', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'ObservationStatus'(),
	category :: [complex:'CodeableConcept'()] | undefined,
	code :: complex:'CodeableConcept'(),
	subject :: special:'Reference'() | undefined,
	focus :: [special:'Reference'()] | undefined,
	encounter :: special:'Reference'() | undefined,
	choice :: complex:'Timing'() | complex:'Period'() | instant() | dateTime() | undefined,
	issued :: instant() | undefined,
	performer :: [special:'Reference'()] | undefined,
	choice1 :: time() | string() | complex:'SampledData'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'Period'() | integer() | dateTime() | complex:'CodeableConcept'() | boolean() | undefined,
	dataAbsentReason :: complex:'CodeableConcept'() | undefined,
	interpretation :: [complex:'CodeableConcept'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	bodySite :: complex:'CodeableConcept'() | undefined,
	method :: complex:'CodeableConcept'() | undefined,
	specimen :: special:'Reference'() | undefined,
	device :: special:'Reference'() | undefined,
	referenceRange :: [complex:'Observation.ReferenceRange'()] | undefined,
	hasMember :: [special:'Reference'()] | undefined,
	derivedFrom :: [special:'Reference'()] | undefined,
	component :: [complex:'Observation.Component'()] | undefined}).

-type 'Observation'() :: #'Observation'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_observation({Props}) -> to_observation(Props);
to_observation(Props) ->
  DT = decode:xsd_info(<<"Observation">>),
  #'Observation'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , focus  = decode:value(<<"focus">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , issued  = decode:value(<<"issued">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , dataAbsentReason  = decode:value(<<"dataAbsentReason">>, Props, DT)
    , interpretation  = decode:value(<<"interpretation">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , method  = decode:value(<<"method">>, Props, DT)
    , specimen  = decode:value(<<"specimen">>, Props, DT)
    , device  = decode:value(<<"device">>, Props, DT)
    , referenceRange  = decode:value(<<"referenceRange">>, Props, DT)
    , hasMember  = decode:value(<<"hasMember">>, Props, DT)
    , derivedFrom  = decode:value(<<"derivedFrom">>, Props, DT)
    , component  = decode:value(<<"component">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_observation.Component({Props}) -> to_observation.Component(Props);
to_observation.Component(Props) ->
  DT = decode:xsd_info(<<"Observation.Component">>),
  #'Observation.Component'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , dataAbsentReason  = decode:value(<<"dataAbsentReason">>, Props, DT)
    , interpretation  = decode:value(<<"interpretation">>, Props, DT)
    , referenceRange  = decode:value(<<"referenceRange">>, Props, DT)
    }.


to_observation.ReferenceRange({Props}) -> to_observation.ReferenceRange(Props);
to_observation.ReferenceRange(Props) ->
  DT = decode:xsd_info(<<"Observation.ReferenceRange">>),
  #'Observation.ReferencRange'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , low  = decode:value(<<"low">>, Props, DT)
    , high  = decode:value(<<"high">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , appliesTo  = decode:value(<<"appliesTo">>, Props, DT)
    , age  = decode:value(<<"age">>, Props, DT)
    , text  = decode:value(<<"text">>, Props, DT)
    }.


text(#'Observation'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, observation:to_observation(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

observation_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Observation',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
observation_toprop_test() ->
    ?asrtp({'Observation',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Observation">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

observation_json_test() ->
    ?asrtjson({'Observation',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Observation\",\"id\":\"p-21666\"}">>).

-endif.



