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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
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
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_immunization.ProtocolApplied({Props}) -> to_immunization.ProtocolApplied(Props);
to_immunization.ProtocolApplied(Props) ->
  DT = decode:xsd_info(<<"Immunization.ProtcolApplied">>),
  #'Immunization.ProtocolApplied'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	series :: string() | undefined,
	authority :: special:'Reference'() | undefined,
	targetDisease :: [complex:'CodeableConcept'()] | undefined,
	choice :: string() | positiveInt(),
	choice1 :: string() | positiveInt() | undefined}).
    }.


to_immunization.Reaction({Props}) -> to_immunization.Reaction(Props);
to_immunization.Reaction(Props) ->
  DT = decode:xsd_info(<<"Immunization.Reaction">>),
  #'Immunization.Reaction'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	date :: dateTime() | undefined,
	detail :: special:'Reference'() | undefined,
	reported :: boolean() | undefined}).

to_immunization.Education({Props}) -> to_immunization.Education(Props);
to_immunization.Education(Props) ->
  DT = decode:xsd_info(<<"Immunization.Education">>),
  #'Immunization.Education'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	documentType :: string() | undefined,
	reference :: uri() | undefined,
	publicationDate :: dateTime() | undefined,
	presentationDate :: dateTime() | undefined}).
    }.


to_immunization.Performer({Props}) -> to_immunization.Performer(Props);
to_immunization.Performer(Props) ->
  DT = decode:xsd_info(<<"Immunization.Performer">>),
  #'Immunization.Performer'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'()}).
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



