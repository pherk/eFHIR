-module(patient).
-compile(export_all).
-include("fhir.hrl").
-include("fhir_primitives.hrl").

%-import(fhir_datatypes, [meta/0]).

-record(patient, {
      id          :: id()
    , meta        :: fhir_datatypes:meta()
    , text        :: fhir_datatypes:narrative()
    , extension   :: fhir_extension:extension()
    , identifier_ :: fhir_datatypes:identifier()
    , active      :: boolean()
    , name        :: [fhir_datatypes:humanName()]
    , telecom     :: [fhir_datatypes:contactPoint()]
    , gender      :: code()
    , birthDate   :: date()
    , deceasedBoolean  :: boolean()
    , deceasedDateTime :: dateTime()
    , address          :: [fhir_datatypes:address()]
    , maritalStatus    :: fhir_datatypes:codeableConcept()
    , multipleBirthBoolean :: boolean()
    , multipleBirthInteger :: integer()
    , photo                :: [fhir_datatypes:attachment()]
    , contact              :: [contact()]	
    , communication        :: [communication()]
    , generalPractitioner  :: [fhir_datatypes:reference_()]
    , managingOrganization :: fhir_datatypes:reference_()
    , link                 :: [link_()]
    }).
-opaque patient() :: #patient{}.

-record(contact, {
	  relationship :: [fhir_datatypes:codeableConcept()]
        , name         :: fhir_datatypes:humanName()
        , telecom      :: [fhir_datatypes:contactPoint()]
        , address      :: fhir_datatypes:address()
        , gender       :: fhir_datatypes:code()
        , organization :: fhir_datatypes:reference_()
        , period       :: fhir_datatypes:period()
    }).
-opaque contact() :: #contact{}.

-record(communication, {
          language :: fhir_datatypes:codeableCOncept()
        , preferred :: boolean()
    }).
-opaque communication() :: #communication{}.

-record(link, {
          other :: fhir_datatypes:reference_()
        , type  :: fhir_datatypes:code()
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
    , meta        = fhir_datatypes:to_meta(<<"meta">>, Props)
    , text        = fhir_datatypes:to_narrative(<<"text">>, Props)
    , extension   = fhir_extension:to_extension_list(Props)
    , identifier_ = fhir_datatypes:to_identifier_list(<<"identifier">>, Props)
    , active      = proplists:get_value(<<"active">>, Props)
    , name        = fhir_datatypes:to_humanName_list(<<"name">>, Props)
    , telecom     = fhir_datatypes:to_contactPoint_list(<<"telecom">>, Props)
    , gender      = proplists:get_value(<<"gender">>, Props)
    , birthDate   = proplists:get_value(<<"birthDate">>, Props)
    , deceasedBoolean  = proplists:get_value(<<"deceasedBoolean">>, Props)
    , deceasedDateTime = proplists:get_value(<<"deceasedDateTime">>, Props)
    , address          = fhir_datatypes:to_address_list(<<"address">>, Props)
    , maritalStatus    = fhir_datatypes:to_codeableConcept(<<"maritalStatus">>, Props)
    , multipleBirthBoolean = proplists:get_value(<<"multipleBirthBoolean">>, Props)
    , multipleBirthInteger = proplists:get_value(<<"multipleBirthInteger">>, Props)
    , photo                = fhir_datatypes:to_attachment_list(<<"photo">>, Props)
    , contact              = to_contact_list(Props)	
    , communication        = to_communication_list(Props)
    , generalPractitioner  = fhir_datatypes:to_reference_list(<<"generalPractitioner">>, Props)
    , managingOrganization = fhir_datatypes:to_reference(<<"managingOrganization">>, Props)
    , link                 = to_link_list(Props)
    }.

%%====================================================================
%% Internal functions
%%====================================================================
to_contact_list(Props) ->
    List = proplists:get_value(<<"contact">>, Props),
    case List of
        undefined -> [];
	    _  -> map(fun to_contact/1, List)
    end.

to_contact({Props}) -> to_contact(Props);
to_contact(Props) ->
  #contact{ 
      relationship = fhir_datatypes:to_codeableConcept(<<"relationship">>, Props)
    , name         = fhir_datatypes:to_humanName(<<"name">>, Props)
    , telecom      = fhir_datatypes:to_contactPoint_list(<<"telecom">>, Props)
    , address      = fhir_datatypes:to_address(<<"address">>, Props)
    , gender       = proplists:get_value(<<"gender">>, Props)
    , organization = fhir_datatypes:to_reference(<<"organization">>, Props)
    , period       = fhir_datatypes:to_period(<<"period">>, Props)
    }.

to_communication_list(Props) ->
    List = proplists:get_value(<<"communication">>, Props),
    case List of
        undefined -> [];
	    _  -> map(fun to_communication/1, List)
    end.

to_communication({Props}) -> to_communication(Props);
to_communication(Props) -> 
  #communication{
      language  = fhir_datatypes:to_codeableConcept(<<"language">>, Props)
    , preferred = proplists:get_value(<<"preferred">>, Props)
    }.

to_link_list(Props) ->
    List = proplists:get_value(<<"link">>, Props),
    case List of
        undefined -> [];
	    _  -> map(fun to_link/1, List)
    end.


to_link({Props}) -> to_link(Props);
to_link(Props) -> 
  #link{
      other = fhir_datatypes:to_reference(<<"other">>, Props)
    , type  = proplists:get_value(<<"type">>, Props)
    }.

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
