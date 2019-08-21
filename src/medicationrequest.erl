-record('MedicationRequest.Substitution', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	choice :: 'CodeableConcept'() | boolean(),
	reason :: 'CodeableConcept'() | undefined}).

-type 'MedicationRequest.Substitution'() :: #'MedicationRequest.Substitution'{}.


-record('MedicationRequest.InitialFill', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	duration :: 'Duration'() | undefined}).

-type 'MedicationRequest.InitialFill'() :: #'MedicationRequest.InitialFill'{}.


-record('MedicationRequest.DispenseRequest', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	initialFill :: 'MedicationRequest.InitialFill'() | undefined,
	dispenseInterval :: 'Duration'() | undefined,
	validityPeriod :: 'Period'() | undefined,
	numberOfRepeatsAllowed :: unsignedInt() | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	expectedSupplyDuration :: 'Duration'() | undefined,
	performer :: 'Reference'() | undefined}).

-type 'MedicationRequest.DispenseRequest'() :: #'MedicationRequest.DispenseRequest'{}.


-record('MedicationRequest', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: medicationrequestStatus(),
	statusReason :: 'CodeableConcept'() | undefined,
	intent :: medicationRequestIntent(),
	category :: ['CodeableConcept'()] | undefined,
	priority :: 'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice :: 'Reference'() | boolean() | undefined,
	choice1 :: 'Reference'() | 'CodeableConcept'(),
	subject :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	supportingInformation :: ['Reference'()] | undefined,
	authoredOn :: dateTime() | undefined,
	requester :: 'Reference'() | undefined,
	performer :: 'Reference'() | undefined,
	performerType :: 'CodeableConcept'() | undefined,
	recorder :: 'Reference'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: ['Reference'()] | undefined,
	groupIdentifier :: 'Identifier'() | undefined,
	courseOfTherapyType :: 'CodeableConcept'() | undefined,
	insurance :: ['Reference'()] | undefined,
	note :: ['Annotation'()] | undefined,
	dosageInstruction :: ['Dosage'()] | undefined,
	dispenseRequest :: 'MedicationRequest.DispenseRequest'() | undefined,
	substitution :: 'MedicationRequest.Substitution'() | undefined,
	priorPrescription :: 'Reference'() | undefined,
	detectedIssue :: ['Reference'()] | undefined,
	eventHistory :: ['Reference'()] | undefined}).

-type 'MedicationRequest'() :: #'MedicationRequest'{}.



