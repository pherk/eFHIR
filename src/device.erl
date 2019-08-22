-module(device).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('Device.Property', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	valueQuantity :: [complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'()] | undefined,
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
	type :: complex:'DeviceNameType'()}).

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
	entryType :: complex:'UDIEntryType'() | undefined}).

-type 'Device.UdiCarrier'() :: #'Device.UdiCarrier'{}.


-record('Device', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	definition :: special:'Reference'() | undefined,
	udiCarrier :: [complex:'Device.UdiCarrier'()] | undefined,
	status :: complex:'FHIRDeviceStatus'() | undefined,
	statusReason :: [complex:'CodeableConcept'()] | undefined,
	distinctIdentifier :: string() | undefined,
	manufacturer :: string() | undefined,
	manufactureDate :: dateTime() | undefined,
	expirationDate :: dateTime() | undefined,
	lotNumber :: string() | undefined,
	serialNumber :: string() | undefined,
	deviceName :: [complex:'Device.DeviceName'()] | undefined,
	modelNumber :: string() | undefined,
	partNumber :: string() | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	specialization :: [complex:'Device.Specialization'()] | undefined,
	version :: [complex:'Device.Version'()] | undefined,
	property :: [complex:'Device.Property'()] | undefined,
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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	definition :: special:'Reference'() | undefined,
	udiCarrier :: [complex:'Device.UdiCarrier'()] | undefined,
	status :: complex:'FHIRDeviceStatus'() | undefined,
	statusReason :: [complex:'CodeableConcept'()] | undefined,
	distinctIdentifier :: string() | undefined,
	manufacturer :: string() | undefined,
	manufactureDate :: dateTime() | undefined,
	expirationDate :: dateTime() | undefined,
	lotNumber :: string() | undefined,
	serialNumber :: string() | undefined,
	deviceName :: [complex:'Device.DeviceName'()] | undefined,
	modelNumber :: string() | undefined,
	partNumber :: string() | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	specialization :: [complex:'Device.Specialization'()] | undefined,
	version :: [complex:'Device.Version'()] | undefined,
	property :: [complex:'Device.Property'()] | undefined,
	patient :: special:'Reference'() | undefined,
	owner :: special:'Reference'() | undefined,
	contact :: [complex:'ContactPoint'()] | undefined,
	location :: special:'Reference'() | undefined,
	url :: uri() | undefined,
	note :: [complex:'Annotation'()] | undefined,
	safety :: [complex:'CodeableConcept'()] | undefined,
	parent :: special:'Reference'() | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_device.Property({Props}) -> to_device.Property(Props);
to_device.Property(Props) ->
  DT = decode:xsd_info(<<"Device.Property">>),
  #'Device.Property'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	valueQuantity :: [complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'()] | undefined,
	valueCode :: [complex:'CodeableConcept'()] | undefined}).
    }.


to_device.Version({Props}) -> to_device.Version(Props);
to_device.Version(Props) ->
  DT = decode:xsd_info(<<"Device.Version">>),
  #'Device.Version'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	component :: complex:'Identifier'() | undefined,
	value :: string()}).
    }.


to_device.Specialization({Props}) -> to_device.Specialization(Props);
to_device.Specialization(Props) ->
  DT = decode:xsd_info(<<"Device.Specialization">>),
  #'Device.Specialization'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	systemType :: complex:'CodeableConcept'(),
	version :: string() | undefined}).
    }.


to_device.DeviceName({Props}) -> to_device.DeviceName(Props);
to_device.DeviceName(Props) ->
  DT = decode:xsd_info(<<"Device.DeviceName">>),
  #'Device.DeviceName'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	name :: string(),
	type :: complex:'DeviceNameType'()}).
    }.



to_device.UdiCarrier({Props}) -> to_device.UdiCarrier(Props);
to_device.UdiCarrier(Props) ->
  DT = decode:xsd_info(<<"Device.UdiCarrier">>),
  #'Device.UdiCarrier'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	deviceIdentifier :: string() | undefined,
	issuer :: uri() | undefined,
	jurisdiction :: uri() | undefined,
	carrierAIDC :: base64Binary() | undefined,
	carrierHRF :: string() | undefined,
	entryType :: complex:'UDIEntryType'() | undefined}).
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
         {'Device',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
device_toprop_test() ->
    ?asrtp({'Device',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Device">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

device_json_test() ->
    ?asrtjson({'Device',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Device\",\"id\":\"p-21666\"}">>).

-endif.


