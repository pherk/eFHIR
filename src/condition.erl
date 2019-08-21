-record('Condition.Evidence', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	code :: ['CodeableConcept'()] | undefined,
	detail :: ['Reference'()] | undefined}).

-type 'Condition.Evidence'() :: #'Condition.Evidence'{}.


-record('Condition.Stage', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	summary :: 'CodeableConcept'() | undefined,
	assessment :: ['Reference'()] | undefined,
	type :: 'CodeableConcept'() | undefined}).

-type 'Condition.Stage'() :: #'Condition.Stage'{}.


-record('Condition', {anyAttribs :: anyAttribs(),
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
	category :: ['CodeableConcept'()] | undefined,
	severity :: 'CodeableConcept'() | undefined,
	code :: 'CodeableConcept'() | undefined,
	bodySite :: ['CodeableConcept'()] | undefined,
	subject :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	choice :: string() | 'Range'() | 'Period'() | dateTime() | 'Age'() | undefined,
	choice1 :: string() | 'Range'() | 'Period'() | dateTime() | 'Age'() | undefined,
	recordedDate :: dateTime() | undefined,
	recorder :: 'Reference'() | undefined,
	asserter :: 'Reference'() | undefined,
	stage :: ['Condition.Stage'()] | undefined,
	evidence :: ['Condition.Evidence'()] | undefined,
	note :: ['Annotation'()] | undefined}).

-type 'Condition'() :: #'Condition'{}.

