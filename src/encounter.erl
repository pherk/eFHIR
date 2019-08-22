-module(encounter).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('Encounter.Location', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	location :: special:'Reference'(),
	status :: complex:'EncounterLocationStatus'() | undefined,
	physicalType :: complex:'CodeableConcept'() | undefined,
	period :: complex:'Period'() | undefined}).

-type 'Encounter.Location'() :: #'Encounter.Location'{}.


-record('Encounter.Hospitalization', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	preAdmissionIdentifier :: complex:'Identifier'() | undefined,
	origin :: special:'Reference'() | undefined,
	admitSource :: complex:'CodeableConcept'() | undefined,
	reAdmission :: complex:'CodeableConcept'() | undefined,
	dietPreference :: [complex:'CodeableConcept'()] | undefined,
	specialCourtesy :: [complex:'CodeableConcept'()] | undefined,
	specialArrangement :: [complex:'CodeableConcept'()] | undefined,
	destination :: special:'Reference'() | undefined,
	dischargeDisposition :: complex:'CodeableConcept'() | undefined}).

-type 'Encounter.Hospitalization'() :: #'Encounter.Hospitalization'{}.


-record('Encounter.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	condition :: special:'Reference'(),
	use :: complex:'CodeableConcept'() | undefined,
	rank :: positiveInt() | undefined}).

-type 'Encounter.Diagnosis'() :: #'Encounter.Diagnosis'{}.


-record('Encounter.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	period :: complex:'Period'() | undefined,
	individual :: special:'Reference'() | undefined}).

-type 'Encounter.Participant'() :: #'Encounter.Participant'{}.


-record('Encounter.ClassHistory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	class :: complex:'Coding'(),
	period :: complex:'Period'()}).

-type 'Encounter.ClassHistory'() :: #'Encounter.ClassHistory'{}.


-record('Encounter.StatusHistory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	status :: complex:'EncounterStatus'(),
	period :: complex:'Period'()}).

-type 'Encounter.StatusHistory'() :: #'Encounter.StatusHistory'{}.


-record('Encounter', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'EncounterStatus'(),
	statusHistory :: [complex:'Encounter.StatusHistory'()] | undefined,
	class :: complex:'Coding'(),
	classHistory :: [complex:'Encounter.ClassHistory'()] | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	serviceType :: complex:'CodeableConcept'() | undefined,
	priority :: complex:'CodeableConcept'() | undefined,
	subject :: special:'Reference'() | undefined,
	episodeOfCare :: [special:'Reference'()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	participant :: [complex:'Encounter.Participant'()] | undefined,
	appointment :: [special:'Reference'()] | undefined,
	period :: complex:'Period'() | undefined,
	length :: complex:'Duration'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	diagnosis :: [complex:'Encounter.Diagnosis'()] | undefined,
	account :: [special:'Reference'()] | undefined,
	hospitalization :: complex:'Encounter.Hospitalization'() | undefined,
	location :: ['Encounter.Location'()] | undefined,
	serviceProvider :: special:'Reference'() | undefined,
	partOf :: special:'Reference'() | undefined}).
    }.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_encounter({Props}) -> to_encounter(Props);
to_encounter(Props) ->
  DT = decode:xsd_info(<<"Encounter">>),
  #'Encounter'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	status :: complex:'EncounterStatus'(),
	statusHistory :: [complex:'Encounter.StatusHistory'()] | undefined,
	class :: complex:'Coding'(),
	classHistory :: [complex:'Encounter.ClassHistory'()] | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	serviceType :: complex:'CodeableConcept'() | undefined,
	priority :: complex:'CodeableConcept'() | undefined,
	subject :: special:'Reference'() | undefined,
	episodeOfCare :: [special:'Reference'()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	participant :: [complex:'Encounter.Participant'()] | undefined,
	appointment :: [special:'Reference'()] | undefined,
	period :: complex:'Period'() | undefined,
	length :: complex:'Duration'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	diagnosis :: [complex:'Encounter.Diagnosis'()] | undefined,
	account :: [special:'Reference'()] | undefined,
	hospitalization :: complex:'Encounter.Hospitalization'() | undefined,

    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_encounter.Location({Props}) -> to_encounter.Location(Props);
to_encounter.Location(Props) ->
  DT = decode:xsd_info(<<"Encounter.Location">>),
  #'Encounter.Location'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	location :: special:'Reference'(),
	status :: complex:'EncounterLocationStatus'() | undefined,
	physicalType :: complex:'CodeableConcept'() | undefined,
	period :: complex:'Period'() | undefined}).
    }.


to_encounter.Hospitalization({Props}) -> to_encounter.Hospitalization(Props);
to_encounter.Hospitalization(Props) ->
  DT = decode:xsd_info(<<"Encounter.Hospitalization">>),
  #'Encounter.Hospitalization'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	preAdmissionIdentifier :: complex:'Identifier'() | undefined,
	origin :: special:'Reference'() | undefined,
	admitSource :: complex:'CodeableConcept'() | undefined,
	reAdmission :: complex:'CodeableConcept'() | undefined,
	dietPreference :: [complex:'CodeableConcept'()] | undefined,
	specialCourtesy :: [complex:'CodeableConcept'()] | undefined,
	specialArrangement :: [complex:'CodeableConcept'()] | undefined,
	destination :: special:'Reference'() | undefined,
	dischargeDisposition :: complex:'CodeableConcept'() | undefined}).
    }.


to_encounter.Diagnosis({Props}) -> to_encounter.Diagnosis(Props);
to_encounter.Diagnosis(Props) ->
  DT = decode:xsd_info(<<"Encounter.Diagnosis">>),
  #'Encounter.Diagnosis'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	condition :: special:'Reference'(),
	use :: complex:'CodeableConcept'() | undefined,
	rank :: positiveInt() | undefined}).

to_encounter.Participant({Props}) -> to_encounter.Participant(Props);
to_encounter.Participant(Props) ->
  DT = decode:xsd_info(<<"Encounter.Participant">>),
  #'Encounter.Participant'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: [complex:'CodeableConcept'()] | undefined,
	period :: complex:'Period'() | undefined,
	individual :: special:'Reference'() | undefined}).
    }.

to_encounter.ClassHistory({Props}) -> to_encounter.ClassHistory(Props);
to_encounter.ClassHistory(Props) ->
  DT = decode:xsd_info(<<"Encounter.ClassHistory">>),
  #'Encounter.ClassHistory'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	class :: complex:'Coding'(),
	period :: complex:'Period'()}).
    }.

to_encounter.StatusHistory({Props}) -> to_encounter.StatusHistory(Props);
to_encounter.StatusHistory(Props) ->
  DT = decode:xsd_info(<<"Encounter.StatusHistory">>),
  #'Encounter.StatusHistory'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	status :: complex:'EncounterStatus'(),
	period :: complex:'Period'()}).
    }.

text(#'Encounter'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, encounter:to_encounter(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

encounter_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Encounter',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
encounter_toprop_test() ->
    ?asrtp({'Encounter',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Encounter">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

encounter_json_test() ->
    ?asrtjson({'Encounter',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Encounter\",\"id\":\"p-21666\"}">>).

-endif.


