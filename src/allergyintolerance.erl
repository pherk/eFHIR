-record('AllergyIntoleranceSeverity', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'AllergyIntoleranceSeverity'() :: #'AllergyIntoleranceSeverity'{}.


-record('AllergyIntoleranceCategory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'AllergyIntoleranceCategory'() :: #'AllergyIntoleranceCategory'{}.


-record('AllergyIntoleranceType', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'AllergyIntoleranceType'() :: #'AllergyIntoleranceType'{}.


-record('AllergyIntoleranceCriticality', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'AllergyIntoleranceCriticality'() :: #'AllergyIntoleranceCriticality'{}.


-record('AllergyIntolerance.Reaction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	substance :: 'CodeableConcept'() | undefined,
	manifestation :: ['CodeableConcept'()],
	description :: string() | undefined,
	onset :: dateTime() | undefined,
	severity :: 'AllergyIntoleranceSeverity'() | undefined,
	exposureRoute :: 'CodeableConcept'() | undefined,
	note :: ['Annotation'()] | undefined}).

-type 'AllergyIntolerance.Reaction'() :: #'AllergyIntolerance.Reaction'{}.


-record('AllergyIntolerance', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	clinicalStatus :: 'CodeableConcept'() | undefined,
	verificationStatus :: 'CodeableConcept'() | undefined,
	type :: 'AllergyIntoleranceType'() | undefined,
	category :: ['AllergyIntoleranceCategory'()] | undefined,
	criticality :: 'AllergyIntoleranceCriticality'() | undefined,
	code :: 'CodeableConcept'() | undefined,
	patient :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	choice :: string() | 'Range'() | 'Period'() | dateTime() | 'Age'() | undefined,
	recordedDate :: dateTime() | undefined,
	recorder :: 'Reference'() | undefined,
	asserter :: 'Reference'() | undefined,
	lastOccurrence :: dateTime() | undefined,
	note :: ['Annotation'()] | undefined,
	reaction :: ['AllergyIntolerance.Reaction'()] | undefined}).

-type 'AllergyIntolerance'() :: #'AllergyIntolerance'{}.

