-record('PractitionerRole.NotAvailable', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	description :: string(),
	during :: 'Period'() | undefined}).

-type 'PractitionerRole.NotAvailable'() :: #'PractitionerRole.NotAvailable'{}.


-record('PractitionerRole.AvailableTime', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	daysOfWeek :: ['DaysOfWeek'()] | undefined,
	allDay :: boolean() | undefined,
	availableStartTime :: time() | undefined,
	availableEndTime :: time() | undefined}).

-type 'PractitionerRole.AvailableTime'() :: #'PractitionerRole.AvailableTime'{}.


-record('PractitionerRole', {anyAttribs :: anyAttribs(),
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
	period :: 'Period'() | undefined,
	practitioner :: 'Reference'() | undefined,
	organization :: 'Reference'() | undefined,
	code :: ['CodeableConcept'()] | undefined,
	specialty :: ['CodeableConcept'()] | undefined,
	location :: ['Reference'()] | undefined,
	healthcareService :: ['Reference'()] | undefined,
	telecom :: ['ContactPoint'()] | undefined,
	availableTime :: ['PractitionerRole.AvailableTime'()] | undefined,
	notAvailable :: ['PractitionerRole.NotAvailable'()] | undefined,
	availabilityExceptions :: string() | undefined,
	endpoint :: ['Reference'()] | undefined}).

-type 'PractitionerRole'() :: #'PractitionerRole'{}.


