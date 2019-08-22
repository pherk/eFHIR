-module(practitionerrole).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('PractitionerRole.NotAvailable', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	description :: string(),
	during :: complex:'Period'() | undefined}).

-type 'PractitionerRole.NotAvailable'() :: #'PractitionerRole.NotAvailable'{}.


-record('PractitionerRole.AvailableTime', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	daysOfWeek :: [complex:'DaysOfWeek'()] | undefined,
	allDay :: boolean() | undefined,
	availableStartTime :: time() | undefined,
	availableEndTime :: time() | undefined}).

-type 'PractitionerRole.AvailableTime'() :: #'PractitionerRole.AvailableTime'{}.


-record('PractitionerRole', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	active :: boolean() | undefined,
	period :: complex:'Period'() | undefined,
	practitioner :: special:'Reference'() | undefined,
	organization :: special:'Reference'() | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	specialty :: [complex:'CodeableConcept'()] | undefined,
	location :: [special:'Reference'()] | undefined,
	healthcareService :: [special:'Reference'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	availableTime :: [complex:'PractitionerRole.AvailableTime'()] | undefined,
	notAvailable :: [complex:'PractitionerRole.NotAvailable'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: [special:'Reference'()] | undefined}).

-type 'PractitionerRole'() :: #'PractitionerRole'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_practitionerRole({Props}) -> to_practitionerRole(Props);
to_practitionerRole(Props) ->
  DT = decode:xsd_info(<<"PractitionerRole">>),
  #'PractitionerRole'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	active :: boolean() | undefined,
	period :: complex:'Period'() | undefined,
	practitioner :: special:'Reference'() | undefined,
	organization :: special:'Reference'() | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	specialty :: [complex:'CodeableConcept'()] | undefined,
	location :: [special:'Reference'()] | undefined,
	healthcareService :: [special:'Reference'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	availableTime :: [complex:'PractitionerRole.AvailableTime'()] | undefined,
	notAvailable :: [complex:'PractitionerRole.NotAvailable'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: [special:'Reference'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_practitionerRole.NotAvailable({Props}) -> to_practitionerRole.NotAvailable(Props);
to_practitionerRole.NotAvailable(Props) ->
  DT = decode:xsd_info(<<"PractitionerRole.NotAvailableTime">>),
  #'PractitionerRole.NotAvailableTime'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	description :: string(),
	during :: complex:'Period'() | undefined}).
    }.



to_practitionerRole.AvailableTime({Props}) -> to_practitionerRole.AvailableTime(Props);
to_practitionerRole.AvailableTime(Props) ->
  DT = decode:xsd_info(<<"PractitionerRole.AvailableTime">>),
  #'PractitionerRole.AvailableTime'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	daysOfWeek :: [complex:'DaysOfWeek'()] | undefined,
	allDay :: boolean() | undefined,
	availableStartTime :: time() | undefined,
	availableEndTime :: time() | undefined}).
    }.




text(#'PractitionerRole'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, practitionerRole:to_practitionerRole(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

practitionerRole_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'PractitionerRole',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
practitionerRole_toprop_test() ->
    ?asrtp({'PractitionerRole',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"PractitionerRole">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

practitionerRole_json_test() ->
    ?asrtjson({'PractitionerRole',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"PractitionerRole\",\"id\":\"p-21666\"}">>).

-endif.



