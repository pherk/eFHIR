-record('Observation.Component', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	code :: 'CodeableConcept'(),
	choice :: time() | string() | 'SampledData'() | 'Ratio'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | 'Period'() | integer() | dateTime() | 'CodeableConcept'() | boolean() | undefined,
	dataAbsentReason :: 'CodeableConcept'() | undefined,
	interpretation :: ['CodeableConcept'()] | undefined,
	referenceRange :: ['Observation.ReferenceRange'()] | undefined}).

-type 'Observation.Component'() :: #'Observation.Component'{}.


-record('Observation.ReferenceRange', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	low :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	high :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	type :: 'CodeableConcept'() | undefined,
	appliesTo :: ['CodeableConcept'()] | undefined,
	age :: 'Range'() | undefined,
	text :: string() | undefined}).

-type 'Observation.ReferenceRange'() :: #'Observation.ReferenceRange'{}.


-record('Observation', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	basedOn :: ['Reference'()] | undefined,
	partOf :: ['Reference'()] | undefined,
	status :: 'ObservationStatus'(),
	category :: ['CodeableConcept'()] | undefined,
	code :: 'CodeableConcept'(),
	subject :: 'Reference'() | undefined,
	focus :: ['Reference'()] | undefined,
	encounter :: 'Reference'() | undefined,
	choice :: 'Timing'() | 'Period'() | instant() | dateTime() | undefined,
	issued :: instant() | undefined,
	performer :: ['Reference'()] | undefined,
	choice1 :: time() | string() | 'SampledData'() | 'Ratio'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | 'Period'() | integer() | dateTime() | 'CodeableConcept'() | boolean() | undefined,
	dataAbsentReason :: 'CodeableConcept'() | undefined,
	interpretation :: ['CodeableConcept'()] | undefined,
	note :: ['Annotation'()] | undefined,
	bodySite :: 'CodeableConcept'() | undefined,
	method :: 'CodeableConcept'() | undefined,
	specimen :: 'Reference'() | undefined,
	device :: 'Reference'() | undefined,
	referenceRange :: ['Observation.ReferenceRange'()] | undefined,
	hasMember :: ['Reference'()] | undefined,
	derivedFrom :: ['Reference'()] | undefined,
	component :: ['Observation.Component'()] | undefined}).

-type 'Observation'() :: #'Observation'{}.


