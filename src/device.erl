-record('Device.Property', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	valueQuantity :: ['Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'()] | undefined,
	valueCode :: ['CodeableConcept'()] | undefined}).

-type 'Device.Property'() :: #'Device.Property'{}.


-record('Device.Version', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	component :: 'Identifier'() | undefined,
	value :: string()}).

-type 'Device.Version'() :: #'Device.Version'{}.


-record('Device.Specialization', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	systemType :: 'CodeableConcept'(),
	version :: string() | undefined}).

-type 'Device.Specialization'() :: #'Device.Specialization'{}.


-record('Device.DeviceName', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	name :: string(),
	type :: 'DeviceNameType'()}).

-type 'Device.DeviceName'() :: #'Device.DeviceName'{}.


-record('Device.UdiCarrier', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	deviceIdentifier :: string() | undefined,
	issuer :: uri() | undefined,
	jurisdiction :: uri() | undefined,
	carrierAIDC :: base64Binary() | undefined,
	carrierHRF :: string() | undefined,
	entryType :: 'UDIEntryType'() | undefined}).

-type 'Device.UdiCarrier'() :: #'Device.UdiCarrier'{}.


-record('Device', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	definition :: 'Reference'() | undefined,
	udiCarrier :: ['Device.UdiCarrier'()] | undefined,
	status :: 'FHIRDeviceStatus'() | undefined,
	statusReason :: ['CodeableConcept'()] | undefined,
	distinctIdentifier :: string() | undefined,
	manufacturer :: string() | undefined,
	manufactureDate :: dateTime() | undefined,
	expirationDate :: dateTime() | undefined,
	lotNumber :: string() | undefined,
	serialNumber :: string() | undefined,
	deviceName :: ['Device.DeviceName'()] | undefined,
	modelNumber :: string() | undefined,
	partNumber :: string() | undefined,
	type :: 'CodeableConcept'() | undefined,
	specialization :: ['Device.Specialization'()] | undefined,
	version :: ['Device.Version'()] | undefined,
	property :: ['Device.Property'()] | undefined,
	patient :: 'Reference'() | undefined,
	owner :: 'Reference'() | undefined,
	contact :: ['ContactPoint'()] | undefined,
	location :: 'Reference'() | undefined,
	url :: uri() | undefined,
	note :: ['Annotation'()] | undefined,
	safety :: ['CodeableConcept'()] | undefined,
	parent :: 'Reference'() | undefined}).

-type 'Device'() :: #'Device'{}.
