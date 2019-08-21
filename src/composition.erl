-record('Composition.Section', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	title :: string() | undefined,
	code :: 'CodeableConcept'() | undefined,
	author :: ['Reference'()] | undefined,
	focus :: 'Reference'() | undefined,
	text :: 'Narrative'() | undefined,
	mode :: 'ListMode'() | undefined,
	orderedBy :: 'CodeableConcept'() | undefined,
	entry :: ['Reference'()] | undefined,
	emptyReason :: 'CodeableConcept'() | undefined,
	section :: ['Composition.Section'()] | undefined}).

-type 'Composition.Section'() :: #'Composition.Section'{}.


-record('Composition.Event', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	code :: ['CodeableConcept'()] | undefined,
	period :: 'Period'() | undefined,
	detail :: ['Reference'()] | undefined}).

-type 'Composition.Event'() :: #'Composition.Event'{}.


-record('Composition.RelatesTo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	code :: 'DocumentRelationshipType'(),
	choice :: 'Reference'() | 'Identifier'()}).

-type 'Composition.RelatesTo'() :: #'Composition.RelatesTo'{}.


-record('Composition.Attester', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	mode :: 'CompositionAttestationMode'(),
	time :: dateTime() | undefined,
	party :: 'Reference'() | undefined}).

-type 'Composition.Attester'() :: #'Composition.Attester'{}.


-record('Composition', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: 'Identifier'() | undefined,
	status :: 'CompositionStatus'(),
	type :: 'CodeableConcept'(),
	category :: ['CodeableConcept'()] | undefined,
	subject :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	date :: dateTime(),
	author :: ['Reference'()],
	title :: string(),
	confidentiality :: vConfidentialityClassification() | undefined,
	attester :: ['Composition.Attester'()] | undefined,
	custodian :: 'Reference'() | undefined,
	relatesTo :: ['Composition.RelatesTo'()] | undefined,
	event :: ['Composition.Event'()] | undefined,
	section :: ['Composition.Section'()] | undefined}).

-type 'Composition'() :: #'Composition'{}.


