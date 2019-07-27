-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record(patient, {
      id          :: binary()
    , meta        :: special:meta()
    , implicitRules :: uri()
    , language    :: code()
    , text        :: special:narrative()
    , contained   :: [resource:resource()]
    , extension   :: [extensions:extension()]
    , modifierExtension   :: [extensions:extension()]
    , identifier_ :: complex:identifier()
    , active      :: boolean()
    , name        :: [complex:humanName()]
    , telecom     :: [complex:contactPoint()]
    , gender      :: code()
    , birthDate   :: date()
    , deceasedBoolean  :: boolean()
    , deceasedDateTime :: dateTime()
    , address          :: [complex:address()]
    , maritalStatus    :: complex:codeableConcept()
    , multipleBirthBoolean :: boolean()
    , multipleBirthInteger :: integer()
    , photo                :: [complex:attachment()]
    , contact              :: [patient_contact()]	
    , communication        :: [patient_communication()]
    , generalPractitioner  :: [special:reference_()]
    , managingOrganization :: special:reference_()
    , link                 :: [patient_link()]
    }).
-opaque patient() :: #patient{}.

-record(patient_contact, {
	  relationship :: [complex:codeableConcept()]
    , name         :: complex:humanName()
    , telecom      :: [complex:contactPoint()]
    , address      :: complex:address()
    , gender       :: complex:code()
    , organization :: special:reference_()
    , period       :: complex:period()
    }).
-opaque patient_contact() :: #patient_contact{}.

-record(patient_communication, {
          language :: complex:codeableCOncept()
        , preferred :: boolean()
    }).
-opaque patient_communication() :: #patient_communication{}.

-record(patient_link, {
          other :: special:reference_()
        , type  :: complex:code()
    }). 
-opaque patient_link() :: #patient_link{}.

%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_patient({Props}) -> to_patient(Props);
to_patient(Props) ->
  DT = decode:xsd_info(<<"Patient">>),
  #patient{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier_      = decode:value(<<"identifier">>, Props, DT)
    , active           = decode:value(<<"active">>, Props, DT)
    , name             = decode:value(<<"name">>, Props, DT)
    , telecom          = decode:value(<<"telecom">>, Props, DT)
    , gender           = decode:value(<<"gender">>, Props, DT)
    , birthDate        = decode:value(<<"birthDate">>, Props, DT)
    , deceasedBoolean  = decode:value(<<"deceasedBoolean">>, Props, DT)
    , deceasedDateTime = decode:value(<<"deceasedDateTime">>, Props, DT)
    , address          = decode:value(<<"address">>, Props, DT)
    , maritalStatus    = decode:value(<<"maritalStatus">>, Props, DT)
    , multipleBirthBoolean = decode:value(<<"multipleBirthBoolean">>, Props, DT)
    , multipleBirthInteger = decode:value(<<"multipleBirthInteger">>, Props, DT)
    , photo                = decode:value(<<"photo">>, Props, DT)
    , contact              = decode:value(<<"contact">>, Props, DT)	
    , communication        = decode:value(<<"communication">>, Props, DT)
    , generalPractitioner  = decode:value(<<"generalPractitioner">>, Props, DT)
    , managingOrganization = decode:value(<<"managingOrganization">>, Props, DT)
    , link                 = decode:value(<<"link">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_patient_contact({Props}) -> to_patient_contact(Props);
to_patient_contact(Props) ->
  DT = decode:xsd_info(<<"Patient.Contact">>),
  #patient_contact{ 
      relationship = decode:value(<<"relationship">>, Props, DT)
    , name         = decode:value(<<"name">>, Props, DT)
    , telecom      = decode:value(<<"telecom">>, Props, DT)
    , address      = decode:value(<<"address">>, Props, DT)
    , gender       = decode:value(<<"gender">>, Props, DT)
    , organization = decode:value(<<"organization">>, Props, DT)
    , period       = decode:value(<<"period">>, Props, DT)
    }.

to_patient_communication({Props}) -> to_patient_communication(Props);
to_patient_communication(Props) -> 
  DT = decode:xsd_info(<<"Patient.Communication">>),
  #patient_communication{
      language  = decode:value(<<"language">>, Props, DT)
    , preferred = decode:value(<<"preferred">>, Props, DT)
    }.

to_patient_link({Props}) -> to_patient_link(Props);
to_patient_link(Props) -> 
  DT = decode:xsd_info(<<"Patient.Link">>),
  #patient_link{
      other = decode:value(<<"other">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    }.

text(#patient{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, patient:to_patient(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:rec_to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode({encode:rec_to_proplist(A)}))).

patient_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {patient,<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
patient_toprop_test() ->
    ?asrtp({patient,<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
            [{<<"resourceType">>,<<"Patient">>},
              {<<"id">>,<<"p-21666">>}
            ]).

patient_json_test() ->
    ?asrtjson({patient,<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Patient\",\"id\":\"p-21666\"}">>).

-endif.
