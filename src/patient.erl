-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

%-import(datatypes, [meta/0]).

-record(patient, {
      id          :: id()
    , meta        :: datatypes:meta()
    , text        :: datatypes:narrative()
    , extension   :: extensions:extension()
    , identifier_ :: datatypes:identifier()
    , active      :: boolean()
    , name        :: [datatypes:humanName()]
    , telecom     :: [datatypes:contactPoint()]
    , gender      :: code()
    , birthDate   :: date()
    , deceasedBoolean  :: boolean()
    , deceasedDateTime :: dateTime()
    , address          :: [datatypes:address()]
    , maritalStatus    :: datatypes:codeableConcept()
    , multipleBirthBoolean :: boolean()
    , multipleBirthInteger :: integer()
    , photo                :: [datatypes:attachment()]
    , contact              :: [contact()]	
    , communication        :: [communication()]
    , generalPractitioner  :: [datatypes:reference_()]
    , managingOrganization :: datatypes:reference_()
    , link                 :: [link_()]
    }).
-opaque patient() :: #patient{}.

-record(contact, {
	  relationship :: [datatypes:codeableConcept()]
        , name         :: datatypes:humanName()
        , telecom      :: [datatypes:contactPoint()]
        , address      :: datatypes:address()
        , gender       :: datatypes:code()
        , organization :: datatypes:reference_()
        , period       :: datatypes:period()
    }).
-opaque contact() :: #contact{}.

-record(communication, {
          language :: datatypes:codeableCOncept()
        , preferred :: boolean()
    }).
-opaque communication() :: #communication{}.

-record(link, {
          other :: datatypes:reference_()
        , type  :: datatypes:code()
    }). 
-opaque link_() :: #link{}.

%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_patient(Props) ->
  #patient{ 
      id          = proplists:get_value(<<"id">>, Props)
    , meta        = datatypes:to_meta(<<"meta">>, Props)
    , text        = datatypes:to_narrative(<<"text">>, Props)
    , extension   = extensions:to_extension_list(Props)
    , identifier_ = datatypes:to_identifier_list(<<"identifier">>, Props)
    , active      = proplists:get_value(<<"active">>, Props)
    , name        = datatypes:to_humanName_list(<<"name">>, Props)
    , telecom     = datatypes:to_contactPoint_list(<<"telecom">>, Props)
    , gender      = proplists:get_value(<<"gender">>, Props)
    , birthDate   = proplists:get_value(<<"birthDate">>, Props)
    , deceasedBoolean  = proplists:get_value(<<"deceasedBoolean">>, Props)
    , deceasedDateTime = proplists:get_value(<<"deceasedDateTime">>, Props)
    , address          = datatypes:to_address_list(<<"address">>, Props)
    , maritalStatus    = datatypes:to_codeableConcept(<<"maritalStatus">>, Props)
    , multipleBirthBoolean = proplists:get_value(<<"multipleBirthBoolean">>, Props)
    , multipleBirthInteger = proplists:get_value(<<"multipleBirthInteger">>, Props)
    , photo                = datatypes:to_attachment_list(<<"photo">>, Props)
    , contact              = to_contact_list(Props)	
    , communication        = to_communication_list(Props)
    , generalPractitioner  = datatypes:to_reference_list(<<"generalPractitioner">>, Props)
    , managingOrganization = datatypes:to_reference(<<"managingOrganization">>, Props)
    , link                 = to_link_list(Props)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_contact_list(Props) ->
    List = proplists:get_value(<<"contact">>, Props),
    case List of
        undefined -> [];
	    _  -> lists:map(fun to_contact/1, List)
    end.

to_contact({Props}) -> to_contact(Props);
to_contact(Props) ->
  #contact{ 
      relationship = datatypes:to_codeableConcept(<<"relationship">>, Props)
    , name         = datatypes:to_humanName(<<"name">>, Props)
    , telecom      = datatypes:to_contactPoint_list(<<"telecom">>, Props)
    , address      = datatypes:to_address(<<"address">>, Props)
    , gender       = proplists:get_value(<<"gender">>, Props)
    , organization = datatypes:to_reference(<<"organization">>, Props)
    , period       = datatypes:to_period(<<"period">>, Props)
    }.

to_communication_list(Props) ->
    List = proplists:get_value(<<"communication">>, Props),
    case List of
        undefined -> [];
	    _  -> lists:map(fun to_communication/1, List)
    end.

to_communication({Props}) -> to_communication(Props);
to_communication(Props) -> 
  #communication{
      language  = datatypes:to_codeableConcept(<<"language">>, Props)
    , preferred = proplists:get_value(<<"preferred">>, Props)
    }.

to_link_list(Props) ->
    List = proplists:get_value(<<"link">>, Props),
    case List of
        undefined -> [];
	    _  -> lists:map(fun to_link/1, List)
    end.


to_link({Props}) -> to_link(Props);
to_link(Props) -> 
  #link{
      other = datatypes:to_reference(<<"other">>, Props)
    , type  = proplists:get_value(<<"type">>, Props)
    }.

record_to_proplist(Rec) ->
  L =  [{resourceType, patient}] ++
      lists:zip(record_info(fields, patient), tl(tuple_to_list(Rec))),
  J =jiffy:encode({L}),
  io:format("~p~n",[J]),
  J.

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
