-record('ConsentState', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ConsentState'() :: #'ConsentState'{}.


-record('ConsentDataMeaning', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ConsentDataMeaning'() :: #'ConsentDataMeaning'{}.


-record('ConsentProvisionType', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ConsentProvisionType'() :: #'ConsentProvisionType'{}.


-record('Consent.Data', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	meaning :: 'ConsentDataMeaning'(),
	reference :: 'Reference'()}).

-type 'Consent.Data'() :: #'Consent.Data'{}.


-record('Consent.Actor', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	role :: 'CodeableConcept'(),
	reference :: 'Reference'()}).

-type 'Consent.Actor'() :: #'Consent.Actor'{}.


-record('Consent.Provision', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'ConsentProvisionType'() | undefined,
	period :: 'Period'() | undefined,
	actor :: ['Consent.Actor'()] | undefined,
	action :: ['CodeableConcept'()] | undefined,
	securityLabel :: ['Coding'()] | undefined,
	purpose :: ['Coding'()] | undefined,
	class :: ['Coding'()] | undefined,
	code :: ['CodeableConcept'()] | undefined,
	dataPeriod :: 'Period'() | undefined,
	data :: ['Consent.Data'()] | undefined,
	provision :: ['Consent.Provision'()] | undefined}).

-type 'Consent.Provision'() :: #'Consent.Provision'{}.


-record('Consent.Verification', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	verified :: boolean(),
	verifiedWith :: 'Reference'() | undefined,
	verificationDate :: dateTime() | undefined}).

-type 'Consent.Verification'() :: #'Consent.Verification'{}.


-record('Consent.Policy', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	authority :: uri() | undefined,
	uri :: uri() | undefined}).

-type 'Consent.Policy'() :: #'Consent.Policy'{}.


-record('Consent', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'ConsentState'(),
	scope :: 'CodeableConcept'(),
	category :: ['CodeableConcept'()],
	patient :: 'Reference'() | undefined,
	dateTime :: dateTime() | undefined,
	performer :: ['Reference'()] | undefined,
	organization :: ['Reference'()] | undefined,
	choice :: 'Reference'() | 'Attachment'() | undefined,
	policy :: ['Consent.Policy'()] | undefined,
	policyRule :: 'CodeableConcept'() | undefined,
	verification :: ['Consent.Verification'()] | undefined,
	provision :: 'Consent.Provision'() | undefined}).

-type 'Consent'() :: #'Consent'{}.

