-module(device).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('Device.Property', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	valueQuantity :: [complex:'Quantity'()] | undefined,
	valueCode :: [complex:'CodeableConcept'()] | undefined}).

-type 'Device.Property'() :: #'Device.Property'{}.


-record('Device.Version', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	component :: complex:'Identifier'() | undefined,
	value :: string()}).

-type 'Device.Version'() :: #'Device.Version'{}.


-record('Device.Specialization', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	systemType :: complex:'CodeableConcept'(),
	version :: string() | undefined}).

-type 'Device.Specialization'() :: #'Device.Specialization'{}.


-record('Device.DeviceName', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	name :: string(),
	type :: code()}).

-type 'Device.DeviceName'() :: #'Device.DeviceName'{}.


-record('Device.UdiCarrier', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	deviceIdentifier :: string() | undefined,
	issuer :: uri() | undefined,
	jurisdiction :: uri() | undefined,
	carrierAIDC :: base64Binary() | undefined,
	carrierHRF :: string() | undefined,
	entryType :: code() | undefined}).

-type 'Device.UdiCarrier'() :: #'Device.UdiCarrier'{}.


-record('Device', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	definition :: special:'Reference'() | undefined,
	udiCarrier :: ['Device.UdiCarrier'()] | undefined,
	status :: code() | undefined,
	statusReason :: [complex:'CodeableConcept'()] | undefined,
	distinctIdentifier :: string() | undefined,
	manufacturer :: string() | undefined,
	manufactureDate :: dateTime() | undefined,
	expirationDate :: dateTime() | undefined,
	lotNumber :: string() | undefined,
	serialNumber :: string() | undefined,
	deviceName :: ['Device.DeviceName'()] | undefined,
	modelNumber :: string() | undefined,
	partNumber :: string() | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	specialization :: ['Device.Specialization'()] | undefined,
	version :: ['Device.Version'()] | undefined,
	property :: ['Device.Property'()] | undefined,
	patient :: special:'Reference'() | undefined,
	owner :: special:'Reference'() | undefined,
	contact :: [complex:'ContactPoint'()] | undefined,
	location :: special:'Reference'() | undefined,
	url :: uri() | undefined,
	note :: [complex:'Annotation'()] | undefined,
	safety :: [complex:'CodeableConcept'()] | undefined,
	parent :: special:'Reference'() | undefined}).

-type 'Device'() :: #'Device'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_device({Props}) -> to_device(Props);
to_device(Props) ->
  DT = decode:xsd_info(<<"Device">>),
  #'Device'{ 
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
    , definition  = decode:value(<<"definition">>, Props, DT)
    , udiCarrier  = decode:value(<<"udiCarrier">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , statusReason  = decode:value(<<"statusReason">>, Props, DT)
    , distinctIdentifier  = decode:value(<<"distinctIdentifier">>, Props, DT)
    , manufacturer  = decode:value(<<"manufacturer">>, Props, DT)
    , manufactureDate  = decode:value(<<"manufactureDate">>, Props, DT)
    , expirationDate  = decode:value(<<"expirationDate">>, Props, DT)
    , lotNumber  = decode:value(<<"lotNumber">>, Props, DT)
    , serialNumber  = decode:value(<<"serialNumber">>, Props, DT)
    , deviceName  = decode:value(<<"deviceName">>, Props, DT)
    , modelNumber  = decode:value(<<"modelNumber">>, Props, DT)
    , partNumber  = decode:value(<<"partNumber">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , specialization  = decode:value(<<"specialization">>, Props, DT)
    , version  = decode:value(<<"version">>, Props, DT)
    , property  = decode:value(<<"property">>, Props, DT)
    , patient  = decode:value(<<"patient">>, Props, DT)
    , owner  = decode:value(<<"owner">>, Props, DT)
    , contact  = decode:value(<<"contact">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , url  = decode:value(<<"url">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , safety  = decode:value(<<"safety">>, Props, DT)
    , parent  = decode:value(<<"parent">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_device_property({Props}) -> to_device_property(Props);
to_device_property(Props) ->
  DT = decode:xsd_info(<<"Device.Property">>),
  #'Device.Property'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , valueQuantity  = decode:value(<<"valueQuantity">>, Props, DT)
    , valueCode  = decode:value(<<"valueCode">>, Props, DT)
    }.


to_device_version({Props}) -> to_device_version(Props);
to_device_version(Props) ->
  DT = decode:xsd_info(<<"Device.Version">>),
  #'Device.Version'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , component  = decode:value(<<"component">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    }.


to_device_specialization({Props}) -> to_device_specialization(Props);
to_device_specialization(Props) ->
  DT = decode:xsd_info(<<"Device.Specialization">>),
  #'Device.Specialization'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , systemType  = decode:value(<<"systemType">>, Props, DT)
    , version  = decode:value(<<"version">>, Props, DT)
    }.


to_device_deviceName({Props}) -> to_device_deviceName(Props);
to_device_deviceName(Props) ->
  DT = decode:xsd_info(<<"Device.DeviceName">>),
  #'Device.DeviceName'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    }.



to_device_udiCarrier({Props}) -> to_device_udiCarrier(Props);
to_device_udiCarrier(Props) ->
  DT = decode:xsd_info(<<"Device.UdiCarrier">>),
  #'Device.UdiCarrier'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , deviceIdentifier  = decode:value(<<"deviceIdentifier">>, Props, DT)
    , issuer  = decode:value(<<"issuer">>, Props, DT)
    , jurisdiction  = decode:value(<<"jurisdiction">>, Props, DT)
    , carrierAIDC  = decode:value(<<"carrierAIDC">>, Props, DT)
    , carrierHRF  = decode:value(<<"carrierHRF">>, Props, DT)
    , entryType  = decode:value(<<"entryType">>, Props, DT)
    }.


text(#'Device'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, device:to_device(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

device_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
            {'Device',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,[],undefined,[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],undefined,undefined,undefined,[],[],
             [],undefined,undefined,[],undefined,undefined,[], [],undefined}
           ).

device_toprop_test() ->
    ?asrtp(
            {'Device',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,[],undefined,[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],undefined,undefined,undefined,[],[],
             [],undefined,undefined,[],undefined,undefined,[], [],undefined},
           {[{<<"resourceType">>,<<"Device">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

device_json_test() ->
    ?asrtjson(
            {'Device',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,[],undefined,[], undefined,undefined,undefined,undefined,undefined,
             undefined,[],undefined,undefined,undefined,[],[],
             [],undefined,undefined,[],undefined,undefined,[], [],undefined},
           <<"{\"resourceType\":\"Device\",\"id\":\"p-21666\"}">>).

-endif.


