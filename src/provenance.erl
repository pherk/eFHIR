-record('Provenance.Entity', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	role :: 'ProvenanceEntityRole'(),
	what :: 'Reference'(),
	agent :: ['Provenance.Agent'()] | undefined}).

-type 'Provenance.Entity'() :: #'Provenance.Entity'{}.


-record('Provenance.Agent', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	role :: ['CodeableConcept'()] | undefined,
	who :: 'Reference'(),
	onBehalfOf :: 'Reference'() | undefined}).

-type 'Provenance.Agent'() :: #'Provenance.Agent'{}.


-record('Provenance', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	target :: ['Reference'()],
	choice :: 'Period'() | dateTime() | undefined,
	recorded :: instant(),
	policy :: [uri()] | undefined,
	location :: 'Reference'() | undefined,
	reason :: ['CodeableConcept'()] | undefined,
	activity :: 'CodeableConcept'() | undefined,
	agent :: ['Provenance.Agent'()],
	entity :: ['Provenance.Entity'()] | undefined,
	signature :: ['Signature'()] | undefined}).

-type 'Provenance'() :: #'Provenance'{}.


