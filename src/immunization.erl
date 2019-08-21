-record('Immunization.ProtocolApplied', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	series :: string() | undefined,
	authority :: 'Reference'() | undefined,
	targetDisease :: ['CodeableConcept'()] | undefined,
	choice :: string() | positiveInt(),
	choice1 :: string() | positiveInt() | undefined}).

-type 'Immunization.ProtocolApplied'() :: #'Immunization.ProtocolApplied'{}.


-record('Immunization.Reaction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	date :: dateTime() | undefined,
	detail :: 'Reference'() | undefined,
	reported :: boolean() | undefined}).

-type 'Immunization.Reaction'() :: #'Immunization.Reaction'{}.


-record('Immunization.Education', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	documentType :: string() | undefined,
	reference :: uri() | undefined,
	publicationDate :: dateTime() | undefined,
	presentationDate :: dateTime() | undefined}).

-type 'Immunization.Education'() :: #'Immunization.Education'{}.


-record('Immunization.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	function :: 'CodeableConcept'() | undefined,
	actor :: 'Reference'()}).

-type 'Immunization.Performer'() :: #'Immunization.Performer'{}.


-record('Immunization', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'ImmunizationStatusCodes'(),
	statusReason :: 'CodeableConcept'() | undefined,
	vaccineCode :: 'CodeableConcept'(),
	patient :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	choice :: string() | dateTime(),
	recorded :: dateTime() | undefined,
	primarySource :: boolean() | undefined,
	reportOrigin :: 'CodeableConcept'() | undefined,
	location :: 'Reference'() | undefined,
	manufacturer :: 'Reference'() | undefined,
	lotNumber :: string() | undefined,
	expirationDate :: date() | undefined,
	site :: 'CodeableConcept'() | undefined,
	route :: 'CodeableConcept'() | undefined,
	doseQuantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	performer :: ['Immunization.Performer'()] | undefined,
	note :: ['Annotation'()] | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	isSubpotent :: boolean() | undefined,
	subpotentReason :: ['CodeableConcept'()] | undefined,
	education :: ['Immunization.Education'()] | undefined,
	programEligibility :: ['CodeableConcept'()] | undefined,
	fundingSource :: 'CodeableConcept'() | undefined,
	reaction :: ['Immunization.Reaction'()] | undefined,
	protocolApplied :: ['Immunization.ProtocolApplied'()] | undefined}).

-type 'Immunization'() :: #'Immunization'{}.


