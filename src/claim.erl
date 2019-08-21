-record('Claim.SubDetail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	revenue :: 'CodeableConcept'() | undefined,
	category :: 'CodeableConcept'() | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	programCode :: ['CodeableConcept'()] | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	udi :: ['Reference'()] | undefined}).

-type 'Claim.SubDetail'() :: #'Claim.SubDetail'{}.


-record('Claim.Detail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	revenue :: 'CodeableConcept'() | undefined,
	category :: 'CodeableConcept'() | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	programCode :: ['CodeableConcept'()] | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	udi :: ['Reference'()] | undefined,
	subDetail :: ['Claim.SubDetail'()] | undefined}).

-type 'Claim.Detail'() :: #'Claim.Detail'{}.


-record('Claim.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	careTeamSequence :: [positiveInt()] | undefined,
	diagnosisSequence :: [positiveInt()] | undefined,
	procedureSequence :: [positiveInt()] | undefined,
	informationSequence :: [positiveInt()] | undefined,
	revenue :: 'CodeableConcept'() | undefined,
	category :: 'CodeableConcept'() | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	programCode :: ['CodeableConcept'()] | undefined,
	choice :: 'Period'() | date() | undefined,
	choice1 :: 'Reference'() | 'CodeableConcept'() | 'Address'() | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	udi :: ['Reference'()] | undefined,
	bodySite :: 'CodeableConcept'() | undefined,
	subSite :: ['CodeableConcept'()] | undefined,
	encounter :: ['Reference'()] | undefined,
	detail :: ['Claim.Detail'()] | undefined}).

-type 'Claim.Item'() :: #'Claim.Item'{}.


-record('Claim.Accident', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	date :: date(),
	type :: 'CodeableConcept'() | undefined,
	choice :: 'Reference'() | 'Address'() | undefined}).

-type 'Claim.Accident'() :: #'Claim.Accident'{}.


-record('Claim.Insurance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	focal :: boolean(),
	identifier :: 'Identifier'() | undefined,
	coverage :: 'Reference'(),
	businessArrangement :: string() | undefined,
	preAuthRef :: [string()] | undefined,
	claimResponse :: 'Reference'() | undefined}).

-type 'Claim.Insurance'() :: #'Claim.Insurance'{}.


-record('Claim.Procedure', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	type :: ['CodeableConcept'()] | undefined,
	date :: dateTime() | undefined,
	choice :: 'Reference'() | 'CodeableConcept'(),
	udi :: ['Reference'()] | undefined}).

-type 'Claim.Procedure'() :: #'Claim.Procedure'{}.


-record('Claim.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	choice :: 'Reference'() | 'CodeableConcept'(),
	type :: ['CodeableConcept'()] | undefined,
	onAdmission :: 'CodeableConcept'() | undefined,
	packageCode :: 'CodeableConcept'() | undefined}).

-type 'Claim.Diagnosis'() :: #'Claim.Diagnosis'{}.


-record('Claim.SupportingInfo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	category :: 'CodeableConcept'(),
	code :: 'CodeableConcept'() | undefined,
	choice :: 'Period'() | date() | undefined,
	choice1 :: string() | 'Reference'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | boolean() | 'Attachment'() | undefined,
	reason :: 'CodeableConcept'() | undefined}).

-type 'Claim.SupportingInfo'() :: #'Claim.SupportingInfo'{}.


-record('Claim.CareTeam', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	provider :: 'Reference'(),
	responsible :: boolean() | undefined,
	role :: 'CodeableConcept'() | undefined,
	qualification :: 'CodeableConcept'() | undefined}).

-type 'Claim.CareTeam'() :: #'Claim.CareTeam'{}.


-record('Claim.Payee', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	party :: 'Reference'() | undefined}).

-type 'Claim.Payee'() :: #'Claim.Payee'{}.


-record('Claim.Related', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	claim :: 'Reference'() | undefined,
	relationship :: 'CodeableConcept'() | undefined,
	reference :: 'Identifier'() | undefined}).

-type 'Claim.Related'() :: #'Claim.Related'{}.


-record('Claim', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'FinancialResourceStatusCodes'(),
	type :: 'CodeableConcept'(),
	subType :: 'CodeableConcept'() | undefined,
	use :: 'Use'(),
	patient :: 'Reference'(),
	billablePeriod :: 'Period'() | undefined,
	created :: dateTime(),
	enterer :: 'Reference'() | undefined,
	insurer :: 'Reference'() | undefined,
	provider :: 'Reference'(),
	priority :: 'CodeableConcept'(),
	fundsReserve :: 'CodeableConcept'() | undefined,
	related :: ['Claim.Related'()] | undefined,
	prescription :: 'Reference'() | undefined,
	originalPrescription :: 'Reference'() | undefined,
	payee :: 'Claim.Payee'() | undefined,
	referral :: 'Reference'() | undefined,
	facility :: 'Reference'() | undefined,
	careTeam :: ['Claim.CareTeam'()] | undefined,
	supportingInfo :: ['Claim.SupportingInfo'()] | undefined,
	diagnosis :: ['Claim.Diagnosis'()] | undefined,
	procedure :: ['Claim.Procedure'()] | undefined,
	insurance :: ['Claim.Insurance'()],
	accident :: 'Claim.Accident'() | undefined,
	item :: ['Claim.Item'()] | undefined,
	total :: 'Money'() | undefined}).

-type 'Claim'() :: #'Claim'{}.


