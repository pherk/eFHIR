-record('DiagnosticReport.Media', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	comment :: string() | undefined,
	link :: 'Reference'()}).

-type 'DiagnosticReport.Media'() :: #'DiagnosticReport.Media'{}.


-record('DiagnosticReport', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	basedOn :: ['Reference'()] | undefined,
	status :: 'DiagnosticReportStatus'(),
	category :: ['CodeableConcept'()] | undefined,
	code :: 'CodeableConcept'(),
	subject :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	choice :: 'Period'() | dateTime() | undefined,
	issued :: instant() | undefined,
	performer :: ['Reference'()] | undefined,
	resultsInterpreter :: ['Reference'()] | undefined,
	specimen :: ['Reference'()] | undefined,
	result :: ['Reference'()] | undefined,
	imagingStudy :: ['Reference'()] | undefined,
	media :: ['DiagnosticReport.Media'()] | undefined,
	conclusion :: string() | undefined,
	conclusionCode :: ['CodeableConcept'()] | undefined,
	presentedForm :: ['Attachment'()] | undefined}).

-type 'DiagnosticReport'() :: #'DiagnosticReport'{}.


