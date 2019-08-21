-record('Coverage.Exception', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	period :: 'Period'() | undefined}).

-type 'Coverage.Exception'() :: #'Coverage.Exception'{}.


-record('Coverage.CostToBeneficiary', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	choice :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | 'Money'(),
	exception :: ['Coverage.Exception'()] | undefined}).

-type 'Coverage.CostToBeneficiary'() :: #'Coverage.CostToBeneficiary'{}.


-record('Coverage.Class', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	value :: string(),
	name :: string() | undefined}).

-type 'Coverage.Class'() :: #'Coverage.Class'{}.


-record('Coverage', {anyAttribs :: anyAttribs(),
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
	type :: 'CodeableConcept'() | undefined,
	policyHolder :: 'Reference'() | undefined,
	subscriber :: 'Reference'() | undefined,
	subscriberId :: string() | undefined,
	beneficiary :: 'Reference'(),
	dependent :: string() | undefined,
	relationship :: 'CodeableConcept'() | undefined,
	period :: 'Period'() | undefined,
	payor :: ['Reference'()],
	class :: ['Coverage.Class'()] | undefined,
	order :: positiveInt() | undefined,
	network :: string() | undefined,
	costToBeneficiary :: ['Coverage.CostToBeneficiary'()] | undefined,
	subrogation :: boolean() | undefined,
	contract :: ['Reference'()] | undefined}).

-type 'Coverage'() :: #'Coverage'{}.


