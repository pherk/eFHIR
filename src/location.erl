-record('Location.HoursOfOperation', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	daysOfWeek :: ['DaysOfWeek'()] | undefined,
	allDay :: boolean() | undefined,
	openingTime :: time() | undefined,
	closingTime :: time() | undefined}).

-type 'Location.HoursOfOperation'() :: #'Location.HoursOfOperation'{}.


-record('Location.Position', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	longitude :: decimal(),
	latitude :: decimal(),
	altitude :: decimal() | undefined}).

-type 'Location.Position'() :: #'Location.Position'{}.


-record('Location', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'LocationStatus'() | undefined,
	operationalStatus :: 'Coding'() | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	description :: string() | undefined,
	mode :: 'LocationMode'() | undefined,
	type :: ['CodeableConcept'()] | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	address :: 'Address'() | undefined,
	physicalType :: 'CodeableConcept'() | undefined,
	position :: 'Location.Position'() | undefined,
	managingOrganization :: 'Reference'() | undefined,
	partOf :: 'Reference'() | undefined,
	hoursOfOperation :: ['Location.HoursOfOperation'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: ['Reference'()] | undefined}).

-type 'Location'() :: #'Location'{}.

