-record('ExplanationOfBenefit.Financial', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	choice :: unsignedInt() | string() | 'Money'() | undefined,
	choice1 :: unsignedInt() | 'Money'() | undefined}).

-type 'ExplanationOfBenefit.Financial'() :: #'ExplanationOfBenefit.Financial'{}.


-record('ExplanationOfBenefit.BenefitBalance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	category :: 'CodeableConcept'(),
	excluded :: boolean() | undefined,
	name :: string() | undefined,
	description :: string() | undefined,
	network :: 'CodeableConcept'() | undefined,
	unit :: 'CodeableConcept'() | undefined,
	term :: 'CodeableConcept'() | undefined,
	financial :: ['ExplanationOfBenefit.Financial'()] | undefined}).

-type 'ExplanationOfBenefit.BenefitBalance'() :: #'ExplanationOfBenefit.BenefitBalance'{}.


-record('ExplanationOfBenefit.ProcessNote', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	number :: positiveInt() | undefined,
	type :: 'NoteType'() | undefined,
	text :: string() | undefined,
	language :: 'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.ProcessNote'() :: #'ExplanationOfBenefit.ProcessNote'{}.


-record('ExplanationOfBenefit.Payment', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	adjustment :: 'Money'() | undefined,
	adjustmentReason :: 'CodeableConcept'() | undefined,
	date :: date() | undefined,
	amount :: 'Money'() | undefined,
	identifier :: 'Identifier'() | undefined}).

-type 'ExplanationOfBenefit.Payment'() :: #'ExplanationOfBenefit.Payment'{}.


-record('ExplanationOfBenefit.Total', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	category :: 'CodeableConcept'(),
	amount :: 'Money'()}).

-type 'ExplanationOfBenefit.Total'() :: #'ExplanationOfBenefit.Total'{}.


-record('ExplanationOfBenefit.SubDetail1', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined}).

-type 'ExplanationOfBenefit.SubDetail1'() :: #'ExplanationOfBenefit.SubDetail1'{}.


-record('ExplanationOfBenefit.Detail1', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined,
	subDetail :: ['ExplanationOfBenefit.SubDetail1'()] | undefined}).

-type 'ExplanationOfBenefit.Detail1'() :: #'ExplanationOfBenefit.Detail1'{}.


-record('ExplanationOfBenefit.AddItem', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	itemSequence :: [positiveInt()] | undefined,
	detailSequence :: [positiveInt()] | undefined,
	subDetailSequence :: [positiveInt()] | undefined,
	provider :: ['Reference'()] | undefined,
	productOrService :: 'CodeableConcept'(),
	modifier :: ['CodeableConcept'()] | undefined,
	programCode :: ['CodeableConcept'()] | undefined,
	choice :: 'Period'() | date() | undefined,
	choice1 :: 'Reference'() | 'CodeableConcept'() | 'Address'() | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	unitPrice :: 'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: 'Money'() | undefined,
	bodySite :: 'CodeableConcept'() | undefined,
	subSite :: ['CodeableConcept'()] | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined,
	detail :: ['ExplanationOfBenefit.Detail1'()] | undefined}).

-type 'ExplanationOfBenefit.AddItem'() :: #'ExplanationOfBenefit.AddItem'{}.


-record('ExplanationOfBenefit.SubDetail', {anyAttribs :: anyAttribs(),
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
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined}).

-type 'ExplanationOfBenefit.SubDetail'() :: #'ExplanationOfBenefit.SubDetail'{}.


-record('ExplanationOfBenefit.Detail', {anyAttribs :: anyAttribs(),
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
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined,
	subDetail :: ['ExplanationOfBenefit.SubDetail'()] | undefined}).

-type 'ExplanationOfBenefit.Detail'() :: #'ExplanationOfBenefit.Detail'{}.


-record('ExplanationOfBenefit.Adjudication', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	category :: 'CodeableConcept'(),
	reason :: 'CodeableConcept'() | undefined,
	amount :: 'Money'() | undefined,
	value :: decimal() | undefined}).

-type 'ExplanationOfBenefit.Adjudication'() :: #'ExplanationOfBenefit.Adjudication'{}.

-record('ExplanationOfBenefit.Item', {anyAttribs :: anyAttribs(),
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
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined,
	detail :: ['ExplanationOfBenefit.Detail'()] | undefined}).

-type 'ExplanationOfBenefit.Item'() :: #'ExplanationOfBenefit.Item'{}.


-record('ExplanationOfBenefit.Accident', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	date :: date() | undefined,
	type :: 'CodeableConcept'() | undefined,
	choice :: 'Reference'() | 'Address'() | undefined}).

-type 'ExplanationOfBenefit.Accident'() :: #'ExplanationOfBenefit.Accident'{}.


-record('ExplanationOfBenefit.Insurance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	focal :: boolean(),
	coverage :: 'Reference'(),
	preAuthRef :: [string()] | undefined}).

-type 'ExplanationOfBenefit.Insurance'() :: #'ExplanationOfBenefit.Insurance'{}.


-record('ExplanationOfBenefit.Procedure', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	type :: ['CodeableConcept'()] | undefined,
	date :: dateTime() | undefined,
	choice :: 'Reference'() | 'CodeableConcept'(),
	udi :: ['Reference'()] | undefined}).

-type 'ExplanationOfBenefit.Procedure'() :: #'ExplanationOfBenefit.Procedure'{}.


-record('ExplanationOfBenefit.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	choice :: 'Reference'() | 'CodeableConcept'(),
	type :: ['CodeableConcept'()] | undefined,
	onAdmission :: 'CodeableConcept'() | undefined,
	packageCode :: 'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.Diagnosis'() :: #'ExplanationOfBenefit.Diagnosis'{}.


-record('ExplanationOfBenefit.SupportingInfo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	category :: 'CodeableConcept'(),
	code :: 'CodeableConcept'() | undefined,
	choice :: 'Period'() | date() | undefined,
	choice1 :: string() | 'Reference'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | boolean() | 'Attachment'() | undefined,
	reason :: 'Coding'() | undefined}).

-type 'ExplanationOfBenefit.SupportingInfo'() :: #'ExplanationOfBenefit.SupportingInfo'{}.

-record('ExplanationOfBenefit.CareTeam', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	sequence :: positiveInt(),
	provider :: 'Reference'(),
	responsible :: boolean() | undefined,
	role :: 'CodeableConcept'() | undefined,
	qualification :: 'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.CareTeam'() :: #'ExplanationOfBenefit.CareTeam'{}.


-record('ExplanationOfBenefit.Payee', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	party :: 'Reference'() | undefined}).

-type 'ExplanationOfBenefit.Payee'() :: #'ExplanationOfBenefit.Payee'{}.


-record('ExplanationOfBenefit.Related', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	claim :: 'Reference'() | undefined,
	relationship :: 'CodeableConcept'() | undefined,
	reference :: 'Identifier'() | undefined}).

-type 'ExplanationOfBenefit.Related'() :: #'ExplanationOfBenefit.Related'{}.


-record('ExplanationOfBenefit', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'ExplanationOfBenefitStatus'(),
	type :: 'CodeableConcept'(),
	subType :: 'CodeableConcept'() | undefined,
	use :: 'Use'(),
	patient :: 'Reference'(),
	billablePeriod :: 'Period'() | undefined,
	created :: dateTime(),
	enterer :: 'Reference'() | undefined,
	insurer :: 'Reference'(),
	provider :: 'Reference'(),
	priority :: 'CodeableConcept'() | undefined,
	fundsReserveRequested :: 'CodeableConcept'() | undefined,
	fundsReserve :: 'CodeableConcept'() | undefined,
	related :: ['ExplanationOfBenefit.Related'()] | undefined,
	prescription :: 'Reference'() | undefined,
	originalPrescription :: 'Reference'() | undefined,
	payee :: 'ExplanationOfBenefit.Payee'() | undefined,
	referral :: 'Reference'() | undefined,
	facility :: 'Reference'() | undefined,
	claim :: 'Reference'() | undefined,
	claimResponse :: 'Reference'() | undefined,
	outcome :: 'ClaimProcessingCodes'(),
	disposition :: string() | undefined,
	preAuthRef :: [string()] | undefined,
	preAuthRefPeriod :: ['Period'()] | undefined,
	careTeam :: ['ExplanationOfBenefit.CareTeam'()] | undefined,
	supportingInfo :: ['ExplanationOfBenefit.SupportingInfo'()] | undefined,
	diagnosis :: ['ExplanationOfBenefit.Diagnosis'()] | undefined,
	procedure :: ['ExplanationOfBenefit.Procedure'()] | undefined,
	precedence :: positiveInt() | undefined,
	insurance :: ['ExplanationOfBenefit.Insurance'()],
	accident :: 'ExplanationOfBenefit.Accident'() | undefined,
	item :: ['ExplanationOfBenefit.Item'()] | undefined,
	addItem :: ['ExplanationOfBenefit.AddItem'()] | undefined,
	adjudication :: ['ExplanationOfBenefit.Adjudication'()] | undefined,
	total :: ['ExplanationOfBenefit.Total'()] | undefined,
	payment :: 'ExplanationOfBenefit.Payment'() | undefined,
	formCode :: 'CodeableConcept'() | undefined,
	form :: 'Attachment'() | undefined,
	processNote :: ['ExplanationOfBenefit.ProcessNote'()] | undefined,
	benefitPeriod :: 'Period'() | undefined,
	benefitBalance :: ['ExplanationOfBenefit.BenefitBalance'()] | undefined}).

-type 'ExplanationOfBenefit'() :: #'ExplanationOfBenefit'{}.


