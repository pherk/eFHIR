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
  DT = types:xsd_info(<<"Patient">>),
  #patient{ 
      id               = types:get_value(<<"id">>, Props, DT)
    , meta             = types:get_value(<<"meta">>, Props, DT)
    , implicitRules    = types:get_value(<<"implicitRules">>, Props, DT)
    , language         = types:get_value(<<"language">>, Props, DT)
    , text             = types:get_value(<<"text">>, Props, DT)
    , contained        = types:get_value(<<"contained">>, Props, DT)
    , extension        = types:get_value(<<"extension">>, Props, DT)
    , modifierExtension = types:get_value(<<"modifierExtension">>, Props, DT)
    , identifier_      = types:get_value(<<"identifier">>, Props, DT)
    , active           = types:get_value(<<"active">>, Props, DT)
    , name             = types:get_value(<<"name">>, Props, DT)
    , telecom          = types:get_value(<<"telecom">>, Props, DT)
    , gender           = types:get_value(<<"gender">>, Props, DT)
    , birthDate        = types:get_value(<<"birthDate">>, Props, DT)
    , deceasedBoolean  = types:get_value(<<"deceasedBoolean">>, Props, DT)
    , deceasedDateTime = types:get_value(<<"deceasedDateTime">>, Props, DT)
    , address          = types:get_value(<<"address">>, Props, DT)
    , maritalStatus    = types:get_value(<<"maritalStatus">>, Props, DT)
    , multipleBirthBoolean = types:get_value(<<"multipleBirthBoolean">>, Props, DT)
    , multipleBirthInteger = types:get_value(<<"multipleBirthInteger">>, Props, DT)
    , photo                = types:get_value(<<"photo">>, Props, DT)
    , contact              = types:get_value(<<"contact">>, Props, DT)	
    , communication        = types:get_value(<<"communication">>, Props, DT)
    , generalPractitioner  = types:get_value(<<"generalPractitioner">>, Props, DT)
    , managingOrganization = types:get_value(<<"managingOrganization">>, Props, DT)
    , link                 = types:get_value(<<"link">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_patient_contact({Props}) -> to_patient_contact(Props);
to_patient_contact(Props) ->
  DT = types:xsd_info(<<"Patient.Contact">>),
  #patient_contact{ 
      relationship = types:get_value(<<"relationship">>, Props, DT)
    , name         = types:get_value(<<"name">>, Props, DT)
    , telecom      = types:get_value(<<"telecom">>, Props, DT)
    , address      = types:get_value(<<"address">>, Props, DT)
    , gender       = types:get_value(<<"gender">>, Props, DT)
    , organization = types:get_value(<<"organization">>, Props, DT)
    , period       = types:get_value(<<"period">>, Props, DT)
    }.

to_patient_communication({Props}) -> to_patient_communication(Props);
to_patient_communication(Props) -> 
  DT = types:xsd_info(<<"Patient.Communication">>),
  #patient_communication{
      language  = types:get_value(<<"language">>, Props, DT)
    , preferred = types:get_value(<<"preferred">>, Props, DT)
    }.

to_patient_link({Props}) -> to_patient_link(Props);
to_patient_link(Props) -> 
  DT = types:xsd_info(<<"Patient.Link">>),
  #patient_link{
      other = types:get_value(<<"other">>, Props, DT)
    , type  = types:get_value(<<"type">>, Props, DT)
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
