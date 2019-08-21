-record('ImagingStudy.Instance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	uid :: id(),
	sopClass :: 'Coding'(),
	number :: unsignedInt() | undefined,
	title :: string() | undefined}).

-type 'ImagingStudy.Instance'() :: #'ImagingStudy.Instance'{}.


-record('ImagingStudy.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	function :: 'CodeableConcept'() | undefined,
	actor :: 'Reference'()}).

-type 'ImagingStudy.Performer'() :: #'ImagingStudy.Performer'{}.


-record('ImagingStudy.Series', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	uid :: id(),
	number :: unsignedInt() | undefined,
	modality :: 'Coding'(),
	description :: string() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	endpoint :: ['Reference'()] | undefined,
	bodySite :: 'Coding'() | undefined,
	laterality :: 'Coding'() | undefined,
	specimen :: ['Reference'()] | undefined,
	started :: dateTime() | undefined,
	performer :: ['ImagingStudy.Performer'()] | undefined,
	instance :: ['ImagingStudy.Instance'()] | undefined}).

-type 'ImagingStudy.Series'() :: #'ImagingStudy.Series'{}.


-record('ImagingStudy', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'ImagingStudyStatus'(),
	modality :: ['Coding'()] | undefined,
	subject :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	started :: dateTime() | undefined,
	basedOn :: ['Reference'()] | undefined,
	referrer :: 'Reference'() | undefined,
	interpreter :: ['Reference'()] | undefined,
	endpoint :: ['Reference'()] | undefined,
	numberOfSeries :: unsignedInt() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	procedureReference :: 'Reference'() | undefined,
	procedureCode :: ['CodeableConcept'()] | undefined,
	location :: 'Reference'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	note :: ['Annotation'()] | undefined,
	description :: string() | undefined,
	series :: ['ImagingStudy.Series'()] | undefined}).

-type 'ImagingStudy'() :: #'ImagingStudy'{}.


