-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-define(patient_info,
    #{
     <<"patient">> => [
         {id          , {id, optional}}
       , {meta        , {meta, optional}}
       , {text        , {narrative, optional}}
       , {extension   , {extensions:extension, optional}}
       , {identifier_ , {identifier, optional}}
       , {active      , {boolean, optional}}
       , {name        , {humanName, list}}
       , {telecom     , {contactPoint, list}}
       , {gender      , {code, optional}}
       , {birthDate   , {date, optional}}
       , {deceasedBoolean  , {boolean, optional}}
       , {deceasedDateTime , {dateTime, optional}}
       , {address          , {address, list}}
       , {maritalStatus    , {codeableConcept, optional}}
       , {multipleBirthBoolean , {boolean, optional}}
       , {multipleBirthInteger , {integer, optional}}
       , {photo                , {attachment, list}}
       , {contact              , {contact, list}}	
       , {communication        , {communication, list}}
       , {generalPractitioner  , {reference_, list}}
       , {managingOrganization , {reference_, optional}}
       , {link                 , {link_, list}}]
     , <<"contact">> => [
         {relationship , {codeableConcept, list}}
       , {name         , {humanName, optional}}
       , {telecom      , {contactPoint, list}}
       , {address      , {address, optional}}
       , {gender       , {code, optional}}
       , {organization , {reference_, optional}}
       , {period       , {period, optional}} ]
    , <<"communication">> => []
         {language     , {codeableCOncept, optional}}
       , {preferred    , {boolean, optional}} ]
    , <<"link">> => [
         {other        , {reference_, optional}}
       , {type         , {code, optional}} ]
    }).

-record(patient, {
      id          :: id()
    , meta        :: complex:meta()
    , text        :: complex:narrative()
    , extension   :: extensions:extension()
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
    , contact              :: [contact()]	
    , communication        :: [communication()]
    , generalPractitioner  :: [complex:reference_()]
    , managingOrganization :: complex:reference_()
    , link                 :: [link_()]
    }).
-opaque patient() :: #patient{}.

-record(contact, {
	  relationship :: [complex:codeableConcept()]
    , name         :: complex:humanName()
    , telecom      :: [complex:contactPoint()]
    , address      :: complex:address()
    , gender       :: complex:code()
    , organization :: complex:reference_()
    , period       :: complex:period()
    }).
-opaque contact() :: #contact{}.

-record(communication, {
          language :: complex:codeableCOncept()
        , preferred :: boolean()
    }).
-opaque communication() :: #communication{}.

-record(link, {
          other :: complex:reference_()
        , type  :: complex:code()
    }). 
-opaque link_() :: #link{}.

%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_patient({Props}) -> to_patient(Props);
to_patient(Props) ->
  #patient{ 
      id               = complex:get_value(<<"id">>, Props, DT)
    , meta             = complex:get_value(<<"meta">>, Props, DT)
    , text             = complex:get_value(<<"text">>, Props, DT)
    , extension        = complex:get_value(<<"extension">>, Props, DT)
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
to_contact({Props}) -> to_contact(Props);
to_contact(Props) ->
  #contact{ 
      relationship = complex:get_value(<<"relationship">>, Props, DT)
    , name         = complex:get_value(<<"name">>, Props, DT)
    , telecom      = complex:get_value(<<"telecom">>, Props, DT)
    , address      = complex:get_value(<<"address">>, Props, DT)
    , gender       = complex:get_value(<<"gender">>, Props, DT)
    , organization = complex:get_value(<<"organization">>, Props, DT)
    , period       = complex:get_value(<<"period">>, Props, DT)
    }.

to_communication({Props}) -> to_communication(Props);
to_communication(Props) -> 
  #communication{
      language  = complex:get_value(<<"language">>, Props, DT)
    , preferred = complex:get_value(<<"preferred">>, Props, DT)
    }.

to_link(Props) -> to_link(Props);
to_link(Props) -> 
  #link{
      other = complex:get_value(<<"other">>, Props, DT)
    , type  = complex:get_value(<<"type">>, Props, DT)
    }.

%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, patient:to_patient(A))).
-define(asrtp(A, B), ?assertEqual(B, patient:record_to_proplist(A))).

patient_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {patient,<<"p-21666">>,undefined,undefined,[],[],undefined, [],[],undefined,undefined,undefined,undefined,[], undefined,undefined,undefined,[],[],[],[],undefined, []}).

patient_toprop_test() ->
    ?asrtp({patient,<<"p-21666">>,undefined,undefined,[],[],undefined, [],[],undefined,undefined,undefined,undefined,[], undefined,undefined,undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"patient\",\"id\":\"p-21666\",\"meta\":\"undefined\",\"text\":\"undefined\",\"extension\":[],\"identifier_\":[],\"active\":\"undefined\",\"name\":[],\"telecom\":[],\"gender\":\"undefined\",\"birthDate\":\"undefined\",\"deceasedBoolean\":\"undefined\",\"deceasedDateTime\":\"undefined\",\"address\":[],\"maritalStatus\":\"undefined\",\"multipleBirthBoolean\":\"undefined\",\"multipleBirthInteger\":\"undefined\",\"photo\":[],\"contact\":[],\"communication\":[],\"generalPractitioner\":[],\"managingOrganization\":\"undefined\",\"link\":[]}">>).

-endif.
