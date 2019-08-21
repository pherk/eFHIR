-record('Practitioner.Qualification', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	code :: 'CodeableConcept'(),
	period :: 'Period'() | undefined,
	issuer :: 'Reference'() | undefined}).

-type 'Practitioner.Qualification'() :: #'Practitioner.Qualification'{}.


-record('Practitioner', {anyAttribs :: anyAttribs(),
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
	name :: ['HumanName'()] | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	address :: ['Address'()] | undefined,
	gender :: 'AdministrativeGender'() | undefined,
	birthDate :: date() | undefined,
	photo :: ['Attachment'()] | undefined,
	qualification :: ['Practitioner.Qualification'()] | undefined,
	communication :: ['CodeableConcept'()] | undefined}).

-type 'Practitioner'() :: #'Practitioner'{}.



