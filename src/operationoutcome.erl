-record('OperationOutcome.Issue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	severity :: 'IssueSeverity'(),
	code :: 'IssueType'(),
	details :: 'CodeableConcept'() | undefined,
	diagnostics :: string() | undefined,
	location :: [string()] | undefined,
	expression :: [string()] | undefined}).

-type 'OperationOutcome.Issue'() :: #'OperationOutcome.Issue'{}.


-record('OperationOutcome', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	issue :: ['OperationOutcome.Issue'()]}).

-type 'OperationOutcome'() :: #'OperationOutcome'{}.


