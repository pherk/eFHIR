-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Patient', {
      anyAttribs :: anyAttribs()
    , id          :: binary()
    , meta        :: special:'Meta'()
    , implicitRules :: uri()
    , language    :: code()                                  % 5
    , text        :: special:'Narrative'()
    , contained   :: [resource:'Resource'()]
    , extension   :: [extensions:'Extension'()]
    , modifierExtension   :: [extensions:'Extension'()]
    , 'identifier' :: complex:'Identifier'()                 % 10
    , active      :: boolean()
    , name        :: [complex:'HumanName'()]
    , telecom     :: [complex:'ContactPoint'()]
    , gender      :: code()
    , birthDate   :: date()                                  % 15
    , deceased    :: boolean() | dateTime() | undefined
    , address          :: [complex:'Address'()]
    , maritalStatus    :: complex:'CodeableConcept'()
    , multipleBirth :: boolean() | integer() | undefined
    , photo                :: [complex:'Attachment'()]       % 20
    , contact              :: ['Patient.Contact'()]	
    , communication        :: ['Patient.Communication'()]
    , generalPractitioner  :: [special:'Reference'()]
    , managingOrganization :: special:'Reference'()
    , link                 :: ['Patient.Link'()]
    }).
-opaque 'Patient'() :: #'Patient'{}.

-record('Patient.Contact', {
      anyAttribs :: anyAttribs()
	, id :: string() | undefined
	, extension :: [extensios:'Extension'()] | undefined
	, modifierExtension :: [extensions:'Extension'()] | undefined
	, relationship :: [complex:'CodeableConcept'()]
    , name         :: complex:'HumanName'()
    , telecom      :: [complex:'ContactPoint'()]
    , address      :: complex:'Address'()
    , gender       :: code()
    , organization :: special:'Reference'()
    , period       :: complex:'Period'()
    }).
-opaque 'Patient.Contact'() :: #'Patient.Contact'{}.

-record('Patient.Communication', {
      anyAttribs :: anyAttribs()
	, id :: string() | undefined
	, extension :: [extensios:'Extension'()] | undefined
	, modifierExtension :: [extensions:'Extension'()] | undefined
    , language :: complex:'CodeableCOncept'()
    , preferred :: boolean()
    }).
-opaque 'Patient.Communication'() :: #'Patient.Communication'{}.

-record('Patient.Link', {
      anyAttribs :: anyAttribs()
	, id :: string() | undefined
	, extension :: [extensios:'Extension'()] | undefined
	, modifierExtension :: [extensions:'Extension'()] | undefined
    , other :: special:'Reference'()
    , type  :: code()
    }). 
-opaque 'Patient.Link'() :: #'Patient.Link'{}.

%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
fields('Patient') -> record_info(fields, 'Patient');
fields('Patient.Contact') -> record_info(fields, 'Patient.Contact');
fields('Patient.Communication') -> record_info(fields, 'Patient.Communication');
fields('Patient.Link') -> record_info(fields, 'Patient.Link').

to_patient({Props}) -> to_patient(Props);
to_patient(Props) ->
  DT = decode:xsd_info(<<"Patient">>),
  #'Patient'{ 
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
    , active           = decode:value(<<"active">>, Props, DT)
    , name             = decode:value(<<"name">>, Props, DT)
    , telecom          = decode:value(<<"telecom">>, Props, DT)
    , gender           = decode:value(<<"gender">>, Props, DT)
    , birthDate        = decode:value(<<"birthDate">>, Props, DT)
    , deceased         = decode:value(<<"deceased">>, Props, DT)
    , address          = decode:value(<<"address">>, Props, DT)
    , maritalStatus    = decode:value(<<"maritalStatus">>, Props, DT)
    , multipleBirth    = decode:value(<<"multipleBirth">>, Props, DT)
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
  #'Patient.Contact'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , relationship = decode:value(<<"relationship">>, Props, DT)
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
  #'Patient.Communication'{
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , language  = decode:value(<<"language">>, Props, DT)
    , preferred = decode:value(<<"preferred">>, Props, DT)
    }.

to_patient_link({Props}) -> to_patient_link(Props);
to_patient_link(Props) -> 
  DT = decode:xsd_info(<<"Patient.Link">>),
  #'Patient.Link'{
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , other     = decode:value(<<"other">>, Props, DT)
    , type      = decode:value(<<"type">>, Props, DT)
    }.

text(#'Patient'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, patient:to_patient(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

patient_to_test() ->
   ?asrtto([{<<"id">>, <<"p-21666">>}],
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]}
          ),
   ?asrtto([{<<"id">>, <<"p-21666">>},{<<"birthDate">>, <<"2019-01-01">>}],
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],[],
            undefined,[],[],undefined,<<"2019-01-01">>,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]}
          ),
   ?asrtto([{<<"id">>, <<"p-21666">>},
            {<<"identifier">>, [{[{<<"system">>,<<"orbispid">>}, {<<"value">>, <<"1234567890">>}]}]},
            {<<"name">>, [{[{<<"family">>, <<"Polausi">>, {<<"given">>, [<<"Vausi">>]}, {<<"use">>, <<"offizial">>}}]}]},
            {<<"multipleBirthInteger">>, <<"1">>}
           ],
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
                     [{'Identifier',[],undefined,[],undefined,undefined,
                          <<"orbispid">>,<<"1234567890">>,undefined,
                          undefined}],
                     undefined,
                     [{'HumanName',[],undefined,[],undefined,undefined,
                          undefined,[],[],[],undefined}],
                     [],undefined,undefined,undefined,[],undefined,
                     {<<"Integer">>,<<"1">>},[],[],[],[],undefined,[]}
          ).
patient_toprop_test() ->
    ?asrtp(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {[{<<"resourceType">>,<<"Patient">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

patient_json_test() ->
    ?asrtjson(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           <<"{\"resourceType\":\"Patient\",\"id\":\"p-21666\"}">>).

-endif.

