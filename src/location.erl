-module(location).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Location.HoursOfOperation', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	daysOfWeek :: [code()] | undefined,
	allDay :: boolean() | undefined,
	openingTime :: time() | undefined,
	closingTime :: time() | undefined}).

-type 'Location.HoursOfOperation'() :: #'Location.HoursOfOperation'{}.


-record('Location.Position', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	longitude :: decimal(),
	latitude :: decimal(),
	altitude :: decimal() | undefined}).

-type 'Location.Position'() :: #'Location.Position'{}.


-record('Location', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: code() | undefined,
	operationalStatus :: complex:'Coding'() | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	description :: string() | undefined,
	mode :: code() | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: complex:'Address'() | undefined,
	physicalType :: complex:'CodeableConcept'() | undefined,
	position :: 'Location.Position'() | undefined,
	managingOrganization :: special:'Reference'() | undefined,
	partOf :: special:'Reference'() | undefined,
	hoursOfOperation :: ['Location.HoursOfOperation'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: [special:'Reference'()] | undefined}).

-type 'Location'() :: #'Location'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_location({Props}) -> to_location(Props);
to_location(Props) ->
  DT = decode:xsd_info(<<"Location">>),
  #'Location'{ 
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
    , status  = decode:value(<<"status">>, Props, DT)
    , operationalStatus  = decode:value(<<"operationalStatus">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , alias  = decode:value(<<"alias">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , mode  = decode:value(<<"mode">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , telecom  = decode:value(<<"telecom">>, Props, DT)
    , address  = decode:value(<<"address">>, Props, DT)
    , physicalType  = decode:value(<<"physicalType">>, Props, DT)
    , position  = decode:value(<<"position">>, Props, DT)
    , managingOrganization  = decode:value(<<"managingOrganization">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , hoursOfOperation  = decode:value(<<"hoursOfOperation">>, Props, DT)
    , availabilityExceptions  = decode:value(<<"availabilityExceptions">>, Props, DT)
    , endpoint  = decode:value(<<"endpoint">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_location_hoursOfOperation({Props}) -> to_location_hoursOfOperation(Props);
to_location_hoursOfOperation(Props) ->
  DT = decode:xsd_info(<<"Location.HoursOfOperation">>),
  #'Location.HoursOfOperation'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , daysOfWeek  = decode:value(<<"daysOfWeek">>, Props, DT)
    , allDay  = decode:value(<<"allDay">>, Props, DT)
    , openingTime  = decode:value(<<"openingTime">>, Props, DT)
    , closingTime  = decode:value(<<"closingTime">>, Props, DT)
    }.


to_location_position({Props}) -> to_location_position(Props);
to_location_position(Props) ->
  DT = decode:xsd_info(<<"Location.Position">>),
  #'Location.Position'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , longitude  = decode:value(<<"longitude">>, Props, DT)
    , latitude  = decode:value(<<"latitude">>, Props, DT)
    , altitude  = decode:value(<<"altitude">>, Props, DT)
    }.



text(#'Location'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, location:to_location(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

location_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
            {'Location',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,undefined, undefined,[],undefined,undefined,[],[],undefined,
             undefined,undefined,undefined,undefined,[], undefined,[]}
           ).

location_toprop_test() ->
    ?asrtp(
            {'Location',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,undefined, undefined,[],undefined,undefined,[],[],undefined,
             undefined,undefined,undefined,undefined,[], undefined,[]},
           {[{<<"resourceType">>,<<"Location">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

location_json_test() ->
    ?asrtjson(
            {'Location',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,undefined, undefined,[],undefined,undefined,[],[],undefined,
             undefined,undefined,undefined,undefined,[], undefined,[]},
           <<"{\"resourceType\":\"Location\",\"id\":\"p-21666\"}">>).

-endif.


