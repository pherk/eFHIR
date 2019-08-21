-record('Organization.Contact', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	purpose :: 'CodeableConcept'() | undefined,
	name :: 'HumanName'() | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	address :: 'Address'() | undefined}).

-type 'Organization.Contact'() :: #'Organization.Contact'{}.


-record('Organization', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	active :: boolean() | undefined,
	type :: ['CodeableConcept'()] | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	address :: ['Address'()] | undefined,
	partOf :: 'Reference'() | undefined,
	contact :: ['Organization.Contact'()] | undefined,
	endpoint :: ['Reference'()] | undefined}).

-type 'Organization'() :: #'Organization'{}.

