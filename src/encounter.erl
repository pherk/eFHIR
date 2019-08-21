-record('Encounter.Location', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	location :: 'Reference'(),
	status :: 'EncounterLocationStatus'() | undefined,
	physicalType :: 'CodeableConcept'() | undefined,
	period :: 'Period'() | undefined}).

-type 'Encounter.Location'() :: #'Encounter.Location'{}.


-record('Encounter.Hospitalization', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	preAdmissionIdentifier :: 'Identifier'() | undefined,
	origin :: 'Reference'() | undefined,
	admitSource :: 'CodeableConcept'() | undefined,
	reAdmission :: 'CodeableConcept'() | undefined,
	dietPreference :: ['CodeableConcept'()] | undefined,
	specialCourtesy :: ['CodeableConcept'()] | undefined,
	specialArrangement :: ['CodeableConcept'()] | undefined,
	destination :: 'Reference'() | undefined,
	dischargeDisposition :: 'CodeableConcept'() | undefined}).

-type 'Encounter.Hospitalization'() :: #'Encounter.Hospitalization'{}.


-record('Encounter.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	condition :: 'Reference'(),
	use :: 'CodeableConcept'() | undefined,
	rank :: positiveInt() | undefined}).

-type 'Encounter.Diagnosis'() :: #'Encounter.Diagnosis'{}.


-record('Encounter.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: ['CodeableConcept'()] | undefined,
	period :: 'Period'() | undefined,
	individual :: 'Reference'() | undefined}).

-type 'Encounter.Participant'() :: #'Encounter.Participant'{}.


-record('Encounter.ClassHistory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	class :: 'Coding'(),
	period :: 'Period'()}).

-type 'Encounter.ClassHistory'() :: #'Encounter.ClassHistory'{}.


-record('Encounter.StatusHistory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	status :: 'EncounterStatus'(),
	period :: 'Period'()}).

-type 'Encounter.StatusHistory'() :: #'Encounter.StatusHistory'{}.


-record('Encounter', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'EncounterStatus'(),
	statusHistory :: ['Encounter.StatusHistory'()] | undefined,
	class :: 'Coding'(),
	classHistory :: ['Encounter.ClassHistory'()] | undefined,
	type :: ['CodeableConcept'()] | undefined,
	serviceType :: 'CodeableConcept'() | undefined,
	priority :: 'CodeableConcept'() | undefined,
	subject :: 'Reference'() | undefined,
	episodeOfCare :: ['Reference'()] | undefined,
	basedOn :: ['Reference'()] | undefined,
	participant :: ['Encounter.Participant'()] | undefined,
	appointment :: ['Reference'()] | undefined,
	period :: 'Period'() | undefined,
	length :: 'Duration'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	diagnosis :: ['Encounter.Diagnosis'()] | undefined,
	account :: ['Reference'()] | undefined,
	hospitalization :: 'Encounter.Hospitalization'() | undefined,

