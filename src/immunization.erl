-module(immunization).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Immunization.ProtocolApplied', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	series :: string() | undefined,
	authority :: special:'Reference'() | undefined,
	targetDisease :: [complex:'CodeableConcept'()] | undefined,
	choice :: string() | positiveInt(),
	choice1 :: string() | positiveInt() | undefined}).

-type 'Immunization.ProtocolApplied'() :: #'Immunization.ProtocolApplied'{}.


-record('Immunization.Reaction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	date :: dateTime() | undefined,
	detail :: special:'Reference'() | undefined,
	reported :: boolean() | undefined}).

-type 'Immunization.Reaction'() :: #'Immunization.Reaction'{}.


-record('Immunization.Education', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	documentType :: string() | undefined,
	reference :: uri() | undefined,
	publicationDate :: dateTime() | undefined,
	presentationDate :: dateTime() | undefined}).

-type 'Immunization.Education'() :: #'Immunization.Education'{}.


-record('Immunization.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'()}).

-type 'Immunization.Performer'() :: #'Immunization.Performer'{}.


-record('Immunization', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'ImmunizationStatusCodes'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	vaccineCode :: complex:'CodeableConcept'(),
	patient :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice :: string() | dateTime(),
	recorded :: dateTime() | undefined,
	primarySource :: boolean() | undefined,
	reportOrigin :: complex:'CodeableConcept'() | undefined,
	location :: special:'Reference'() | undefined,
	manufacturer :: special:'Reference'() | undefined,
	lotNumber :: string() | undefined,
	expirationDate :: date() | undefined,
	site :: complex:'CodeableConcept'() | undefined,
	route :: complex:'CodeableConcept'() | undefined,
	doseQuantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	performer :: [complex:'Immunization.Performer'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	isSubpotent :: boolean() | undefined,
	subpotentReason :: [complex:'CodeableConcept'()] | undefined,
	education :: [complex:'Immunization.Education'()] | undefined,
	programEligibility :: [complex:'CodeableConcept'()] | undefined,
	fundingSource :: complex:'CodeableConcept'() | undefined,
	reaction :: [complex:'Immunization.Reaction'()] | undefined,
	protocolApplied :: [complex:'Immunization.ProtocolApplied'()] | undefined}).

-type 'Immunization'() :: #'Immunization'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_immunization({Props}) -> to_immunization(Props);
to_immunization(Props) ->
  DT = decode:xsd_info(<<"Immunization">>),
  #'Immunization'{ 
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
    , status  = decode:value(<<"status">>, Props, DT)
    , statusReason  = decode:value(<<"statusReason">>, Props, DT)
    , vaccineCode  = decode:value(<<"vaccineCode">>, Props, DT)
    , patient  = decode:value(<<"patient">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , recorded  = decode:value(<<"recorded">>, Props, DT)
    , primarySource  = decode:value(<<"primarySource">>, Props, DT)
    , reportOrigin  = decode:value(<<"reportOrigin">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , manufacturer  = decode:value(<<"manufacturer">>, Props, DT)
    , lotNumber  = decode:value(<<"lotNumber">>, Props, DT)
    , expirationDate  = decode:value(<<"expirationDate">>, Props, DT)
    , site  = decode:value(<<"site">>, Props, DT)
    , route  = decode:value(<<"route">>, Props, DT)
    , doseQuantity  = decode:value(<<"doseQuantity">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , isSubpotent  = decode:value(<<"isSubpotent">>, Props, DT)
    , subpotentReason  = decode:value(<<"subpotentReason">>, Props, DT)
    , education  = decode:value(<<"education">>, Props, DT)
    , programEligibility  = decode:value(<<"programEligibility">>, Props, DT)
    , fundingSource  = decode:value(<<"fundingSource">>, Props, DT)
    , reaction  = decode:value(<<"reaction">>, Props, DT)
    , protocolApplied  = decode:value(<<"protocolApplied">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_immunization_protocolApplied({Props}) -> to_immunization_protocolApplied(Props);
to_immunization_protocolApplied(Props) ->
  DT = decode:xsd_info(<<"Immunization.ProtcolApplied">>),
  #'Immunization.ProtocolApplied'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , series  = decode:value(<<"series">>, Props, DT)
    , authority  = decode:value(<<"authority">>, Props, DT)
    , targetDisease  = decode:value(<<"targetDisease">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    }.


to_immunization_reaction({Props}) -> to_immunization_reaction(Props);
to_immunization_reaction(Props) ->
  DT = decode:xsd_info(<<"Immunization.Reaction">>),
  #'Immunization.Reaction'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , detail  = decode:value(<<"detail">>, Props, DT)
    , reported  = decode:value(<<"reported">>, Props, DT)
    }.

to_immunization_education({Props}) -> to_immunization_education(Props);
to_immunization_education(Props) ->
  DT = decode:xsd_info(<<"Immunization.Education">>),
  #'Immunization.Education'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , documentType  = decode:value(<<"documentType">>, Props, DT)
    , reference  = decode:value(<<"reference">>, Props, DT)
    , publicationDate  = decode:value(<<"publicationDate">>, Props, DT)
    , presentationDate  = decode:value(<<"presentationDate">>, Props, DT)
    }.


to_immunization_performer({Props}) -> to_immunization_performer(Props);
to_immunization_performer(Props) ->
  DT = decode:xsd_info(<<"Immunization.Performer">>),
  #'Immunization.Performer'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , function  = decode:value(<<"function">>, Props, DT)
    , actor  = decode:value(<<"actor">>, Props, DT)
    }.

text(#'Immunization'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, immunization:to_immunization(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

immunization_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Immunization',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
immunization_toprop_test() ->
    ?asrtp({'Immunization',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Immunization">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

immunization_json_test() ->
    ?asrtjson({'Immunization',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Immunization\",\"id\":\"p-21666\"}">>).

-endif.



