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
    , intent  = decode:value(<<"intent">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , doNotPerform  = decode:value(<<"doNotPerform">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , supportingInformation  = decode:value(<<"supportingInformation">>, Props, DT)
    , authoredOn  = decode:value(<<"authoredOn">>, Props, DT)
    , requester  = decode:value(<<"requester">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , performerType  = decode:value(<<"performerType">>, Props, DT)
    , recorder  = decode:value(<<"recorder">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , groupIdentifier  = decode:value(<<"groupIdentifier">>, Props, DT)
    , courseOfTherapyType  = decode:value(<<"courseOfTherapyType">>, Props, DT)
    , insurance  = decode:value(<<"insurance">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , dosageInstruction  = decode:value(<<"dosageInstruction">>, Props, DT)
    , dispenseRequest  = decode:value(<<"dispenseRequest">>, Props, DT)
    , substitution  = decode:value(<<"substitution">>, Props, DT)
    , priorPrescription  = decode:value(<<"priorPrescription">>, Props, DT)
    , detectedIssue  = decode:value(<<"detectedIssue">>, Props, DT)
    , eventHistory  = decode:value(<<"eventHistory">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_medicationRequest.Substitution({Props}) -> to_medicationRequest.Substitution(Props);
to_medicationRequest.Substitution(Props) ->
  DT = decode:xsd_info(<<"MedicationRequest.Substitution">>),
  #'MedicationRequest.Substitution'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , reason  = decode:value(<<"reason">>, Props, DT)
    }.


to_medicationRequest.InitialFill({Props}) -> to_medicationRequest.InitialFill(Props);
to_medicationRequest.InitialFill(Props) ->
  DT = decode:xsd_info(<<"MedicationRequest.InitialFIll">>),
  #'MedicationRequest.InitialFill'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , duration  = decode:value(<<"duration">>, Props, DT)
    }.



to_medicationRequest.DispenseRequest({Props}) -> to_medicationRequest.DispenseRequest(Props);
to_medicationRequest.DispenseRequest(Props) ->
  DT = decode:xsd_info(<<"MedicationRequest.DispenseRequest">>),
  #'MedicationRequest.DispenseRequest'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , initialFill  = decode:value(<<"initialFill">>, Props, DT)
    , dispenseInterval  = decode:value(<<"dispenseInterval">>, Props, DT)
    , validityPeriod  = decode:value(<<"validityPeriod">>, Props, DT)
    , numberOfRepeatsAllowed  = decode:value(<<"numberOfRepeatsAllowed">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , expectedSupplyDuration  = decode:value(<<"expectedSupplyDuration">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
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




