-module(servicerequest).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").


-record('ServiceRequest', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	replaces :: [special:'Reference'()] | undefined,
	requisition :: complex:'Identifier'() | undefined,
	status :: complex:'RequestStatus'(),
	intent :: complex:'RequestIntent'(),
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	orderDetail :: [complex:'CodeableConcept'()] | undefined,
	choice :: complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice1 :: complex:'Timing'() | complex:'Period'() | dateTime() | undefined,
	choice2 :: complex:'CodeableConcept'() | boolean() | undefined,
	authoredOn :: dateTime() | undefined,
	requester :: special:'Reference'() | undefined,
	performerType :: complex:'CodeableConcept'() | undefined,
	performer :: [special:'Reference'()] | undefined,
	locationCode :: [complex:'CodeableConcept'()] | undefined,
	locationReference :: [special:'Reference'()] | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	insurance :: [special:'Reference'()] | undefined,
	supportingInfo :: [special:'Reference'()] | undefined,
	specimen :: [special:'Reference'()] | undefined,
	bodySite :: [complex:'CodeableConcept'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	patientInstruction :: string() | undefined,
	relevantHistory :: [special:'Reference'()] | undefined}).

-type 'ServiceRequest'() :: #'ServiceRequest'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_serviceRequest({Props}) -> to_serviceRequest(Props);
to_serviceRequest(Props) ->
  DT = decode:xsd_info(<<"ServiceRequest">>),
  #'ServiceRequest'{ 
      anyAttribs       = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , replaces  = decode:value(<<"replaces">>, Props, DT)
    , requisition  = decode:value(<<"requisition">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , intent  = decode:value(<<"intent">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , doNotPerform  = decode:value(<<"doNotPerform">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , orderDetail  = decode:value(<<"orderDetail">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , choice2  = decode:value(<<"choice2">>, Props, DT)
    , authoredOn  = decode:value(<<"authoredOn">>, Props, DT)
    , requester  = decode:value(<<"requester">>, Props, DT)
    , performerType  = decode:value(<<"performerType">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , locationCode  = decode:value(<<"locationCode">>, Props, DT)
    , locationReference  = decode:value(<<"locationReference">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , insurance  = decode:value(<<"insurance">>, Props, DT)
    , supportingInfo  = decode:value(<<"supportingInfo">>, Props, DT)
    , specimen  = decode:value(<<"specimen">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , patientInstruction  = decode:value(<<"patientInstruction">>, Props, DT)
    , relevantHistory  = decode:value(<<"relevantHistory">>, Props, DT)
   }.

%%====================================================================
%% Internal functions
%%====================================================================

text(#'ServiceRequest'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, servicerequest:to_serviceRequest(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

servicerequest_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'ServiceRequest',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
servicerequest_toprop_test() ->

    ?asrtp({'ServiceRequest',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"ServiceRequest">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

servicerequest_json_test() ->
    ?asrtjson({'ServiceRequest',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"ServiceRequest\",\"id\":\"p-21666\"}">>).

-endif.


