-module(medicationrequest).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('MedicationRequest.Substitution', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: complex:'CodeableConcept'() | boolean(),
	reason :: complex:'CodeableConcept'() | undefined}).

-type 'MedicationRequest.Substitution'() :: #'MedicationRequest.Substitution'{}.


-record('MedicationRequest.InitialFill', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	quantity :: complex:'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	duration :: complex:'Duration'() | undefined}).

-type 'MedicationRequest.InitialFill'() :: #'MedicationRequest.InitialFill'{}.


-record('MedicationRequest.DispenseRequest', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	initialFill :: complex:'MedicationRequest.InitialFill'() | undefined,
	dispenseInterval :: complex:'Duration'() | undefined,
	validityPeriod :: complex:'Period'() | undefined,
	numberOfRepeatsAllowed :: unsignedInt() | undefined,
	quantity :: complex:'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	expectedSupplyDuration :: complex:'Duration'() | undefined,
	performer :: special:'Reference'() | undefined}).

-type 'MedicationRequest.DispenseRequest'() :: #'MedicationRequest.DispenseRequest'{}.


-record('MedicationRequest', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: medicationrequestStatus(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	intent :: medicationRequestIntent(),
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice :: special:'Reference'() | boolean() | undefined,
	choice1 :: special:'Reference'() | 'CodeableConcept'(),
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	supportingInformation :: [special:'Reference'()] | undefined,
	authoredOn :: dateTime() | undefined,
	requester :: special:'Reference'() | undefined,
	performer :: special:'Reference'() | undefined,
	performerType :: complex:'CodeableConcept'() | undefined,
	recorder :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	groupIdentifier :: complex:'Identifier'() | undefined,
	courseOfTherapyType :: complex:'CodeableConcept'() | undefined,
	insurance :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	dosageInstruction :: [complex:'Dosage'()] | undefined,
	dispenseRequest :: complex:'MedicationRequest.DispenseRequest'() | undefined,
	substitution :: complex:'MedicationRequest.Substitution'() | undefined,
	priorPrescription :: special:'Reference'() | undefined,
	detectedIssue :: [special:'Reference'()] | undefined,
	eventHistory :: [special:'Reference'()] | undefined}).

-type 'MedicationRequest'() :: #'MedicationRequest'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_medicationRequest({Props}) -> to_medicationRequest(Props);
to_medicationRequest(Props) ->
  DT = decode:xsd_info(<<"MedicationRequest">>),
  #'MedicationRequest'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	status :: medicationrequestStatus(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	intent :: medicationRequestIntent(),
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice :: special:'Reference'() | boolean() | undefined,
	choice1 :: special:'Reference'() | 'CodeableConcept'(),
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	supportingInformation :: [special:'Reference'()] | undefined,
	authoredOn :: dateTime() | undefined,
	requester :: special:'Reference'() | undefined,
	performer :: special:'Reference'() | undefined,
	performerType :: complex:'CodeableConcept'() | undefined,
	recorder :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	groupIdentifier :: complex:'Identifier'() | undefined,
	courseOfTherapyType :: complex:'CodeableConcept'() | undefined,
	insurance :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	dosageInstruction :: [complex:'Dosage'()] | undefined,
	dispenseRequest :: complex:'MedicationRequest.DispenseRequest'() | undefined,
	substitution :: complex:'MedicationRequest.Substitution'() | undefined,
	priorPrescription :: special:'Reference'() | undefined,
	detectedIssue :: [special:'Reference'()] | undefined,
	eventHistory :: [special:'Reference'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_medicationRequest.Substitution', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: complex:'CodeableConcept'() | boolean(),
	reason :: complex:'CodeableConcept'() | undefined}).
    }.


to_medicationRequest.InitialFill', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	quantity :: complex:'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	duration :: complex:'Duration'() | undefined}).
    }.



to_medicationRequest.DispenseRequest', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	initialFill :: complex:'MedicationRequest.InitialFill'() | undefined,
	dispenseInterval :: complex:'Duration'() | undefined,
	validityPeriod :: complex:'Period'() | undefined,
	numberOfRepeatsAllowed :: unsignedInt() | undefined,
	quantity :: complex:'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	expectedSupplyDuration :: complex:'Duration'() | undefined,
	performer :: special:'Reference'() | undefined}).
    }.



text(#'MedicationRequest'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, medicationRequest:to_medicationRequest(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

medicationRequest_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'MedicationRequest',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
medicationRequest_toprop_test() ->
    ?asrtp({'MedicationRequest',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"MedicationRequest">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

medicationRequest_json_test() ->
    ?asrtjson({'MedicationRequest',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"MedicationRequest\",\"id\":\"p-21666\"}">>).

-endif.




