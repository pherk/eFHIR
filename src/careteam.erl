-record('CareTeam.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	role :: ['CodeableConcept'()] | undefined,
	member :: 'Reference'() | undefined,
	onBehalfOf :: 'Reference'() | undefined,
	period :: 'Period'() | undefined}).

-type 'CareTeam.Participant'() :: #'CareTeam.Participant'{}.

-record('CareTeam', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'CareTeamStatus'() | undefined,
	category :: ['CodeableConcept'()] | undefined,
	name :: string() | undefined,
	subject :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	period :: 'Period'() | undefined,
	participant :: ['CareTeam.Participant'()] | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	managingOrganization :: ['Reference'()] | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	note :: ['Annotation'()] | undefined}).

-type 'CareTeam'() :: #'CareTeam'{}.

