-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record(patient, {
      id          :: binary()
    , meta        :: complex:meta()
    , implicitRules :: uri()
    , language    :: code()
    , text        :: complex:narrative()
    , contained   :: [complex:resource()]
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
    , generalPractitioner  :: [complex:reference_()]
    , managingOrganization :: complex:reference_()
    , link                 :: [patient_link()]
    }).
-opaque patient() :: #patient{}.

-record(patient_contact, {
	  relationship :: [complex:codeableConcept()]
    , name         :: complex:humanName()
    , telecom      :: [complex:contactPoint()]
    , address      :: complex:address()
    , gender       :: complex:code()
    , organization :: complex:reference_()
    , period       :: complex:period()
    }).
-opaque patient_contact() :: #patient_contact{}.

-record(patient_communication, {
          language :: complex:codeableCOncept()
        , preferred :: boolean()
    }).
-opaque patient_communication() :: #patient_communication{}.

-record(patient_link, {
          other :: complex:reference_()
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
  DT = complex:xsd_info(<<"Patient">>),
  #patient{ 
      id               = complex:get_value(<<"id">>, Props, DT)
    , meta             = complex:get_value(<<"meta">>, Props, DT)
    , implicitRules    = complex:get_value(<<"implicitRules">>, Props, DT)
    , language         = complex:get_value(<<"language">>, Props, DT)
    , text             = complex:get_value(<<"text">>, Props, DT)
    , contained        = complex:get_value(<<"contained">>, Props, DT)
    , extension        = complex:get_value(<<"extension">>, Props, DT)
    , modifierExtension = complex:get_value(<<"modifierExtension">>, Props, DT)
    , identifier_      = complex:get_value(<<"identifier">>, Props, DT)
    , active           = complex:get_value(<<"active">>, Props, DT)
    , name             = complex:get_value(<<"name">>, Props, DT)
    , telecom          = complex:get_value(<<"telecom">>, Props, DT)
    , gender           = complex:get_value(<<"gender">>, Props, DT)
    , birthDate        = complex:get_value(<<"birthDate">>, Props, DT)
    , deceasedBoolean  = complex:get_value(<<"deceasedBoolean">>, Props, DT)
    , deceasedDateTime = complex:get_value(<<"deceasedDateTime">>, Props, DT)
    , address          = complex:get_value(<<"address">>, Props, DT)
    , maritalStatus    = complex:get_value(<<"maritalStatus">>, Props, DT)
    , multipleBirthBoolean = complex:get_value(<<"multipleBirthBoolean">>, Props, DT)
    , multipleBirthInteger = complex:get_value(<<"multipleBirthInteger">>, Props, DT)
    , photo                = complex:get_value(<<"photo">>, Props, DT)
    , contact              = complex:get_value(<<"contact">>, Props, DT)	
    , communication        = complex:get_value(<<"communication">>, Props, DT)
    , generalPractitioner  = complex:get_value(<<"generalPractitioner">>, Props, DT)
    , managingOrganization = complex:get_value(<<"managingOrganization">>, Props, DT)
    , link                 = complex:get_value(<<"link">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_patient_contact({Props}) -> to_patient_contact(Props);
to_patient_contact(Props) ->
  DT = complex:xsd_info(<<"Patient.Contact">>),
  #patient_contact{ 
      relationship = complex:get_value(<<"relationship">>, Props, DT)
    , name         = complex:get_value(<<"name">>, Props, DT)
    , telecom      = complex:get_value(<<"telecom">>, Props, DT)
    , address      = complex:get_value(<<"address">>, Props, DT)
    , gender       = complex:get_value(<<"gender">>, Props, DT)
    , organization = complex:get_value(<<"organization">>, Props, DT)
    , period       = complex:get_value(<<"period">>, Props, DT)
    }.

to_patient_communication({Props}) -> to_patient_communication(Props);
to_patient_communication(Props) -> 
  DT = complex:xsd_info(<<"Patient.Communication">>),
  #patient_communication{
      language  = complex:get_value(<<"language">>, Props, DT)
    , preferred = complex:get_value(<<"preferred">>, Props, DT)
    }.

to_patient_link({Props}) -> to_patient_link(Props);
to_patient_link(Props) -> 
  DT = complex:xsd_info(<<"Patient.Link">>),
  #patient_link{
      other = complex:get_value(<<"other">>, Props, DT)
    , type  = complex:get_value(<<"type">>, Props, DT)
    }.

%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, patient:to_patient(A))).
-define(asrtp(A, B), ?assertEqual(B, utils:rec_to_proplist(A))).

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
           <<"{\"resourceType\":\"patient\",\"id\":\"p-21666\",\"meta\":\"undefined\",\"text\":\"undefined\",\"extension\":[],\"identifier_\":[],\"active\":\"undefined\",\"name\":[],\"telecom\":[],\"gender\":\"undefined\",\"birthDate\":\"undefined\",\"deceasedBoolean\":\"undefined\",\"deceasedDateTime\":\"undefined\",\"address\":[],\"maritalStatus\":\"undefined\",\"multipleBirthBoolean\":\"undefined\",\"multipleBirthInteger\":\"undefined\",\"photo\":[],\"contact\":[],\"communication\":[],\"generalPractitioner\":[],\"managingOrganization\":\"undefined\",\"link\":[]}">>).

-endif.
