-module(location).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Location.HoursOfOperation', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	daysOfWeek :: [complex:'DaysOfWeek'()] | undefined,
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
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'LocationStatus'() | undefined,
	operationalStatus :: complex:'Coding'() | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	description :: string() | undefined,
	mode :: complex:'LocationMode'() | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: complex:'Address'() | undefined,
	physicalType :: complex:'CodeableConcept'() | undefined,
	position :: complex:'Location.Position'() | undefined,
	managingOrganization :: special:'Reference'() | undefined,
	partOf :: special:'Reference'() | undefined,
	hoursOfOperation :: [complex:'Location.HoursOfOperation'()] | undefined,
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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	status :: complex:'LocationStatus'() | undefined,
	operationalStatus :: complex:'Coding'() | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	description :: string() | undefined,
	mode :: complex:'LocationMode'() | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: complex:'Address'() | undefined,
	physicalType :: complex:'CodeableConcept'() | undefined,
	position :: complex:'Location.Position'() | undefined,
	managingOrganization :: special:'Reference'() | undefined,
	partOf :: special:'Reference'() | undefined,
	hoursOfOperation :: [complex:'Location.HoursOfOperation'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: [special:'Reference'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_location.HoursOfOperation({Props}) -> to_location.HoursOfOperation(Props);
to_location.HoursOfOperation(Props) ->
  DT = decode:xsd_info(<<"Location.HoursOfOperation">>),
  #'Location.HoursOfOperation'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	daysOfWeek :: [complex:'DaysOfWeek'()] | undefined,
	allDay :: boolean() | undefined,
	openingTime :: time() | undefined,
	closingTime :: time() | undefined}).
    }.


to_location.Position({Props}) -> to_location.Position(Props);
to_location.Position(Props) ->
  DT = decode:xsd_info(<<"Location.Position">>),
  #'Location.Position'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	longitude :: decimal(),
	latitude :: decimal(),
	altitude :: decimal() | undefined}).
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
         {'Location',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
location_toprop_test() ->
    ?asrtp({'Location',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Location">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

location_json_test() ->
    ?asrtjson({'Location',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Location\",\"id\":\"p-21666\"}">>).

-endif.


