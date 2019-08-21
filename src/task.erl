-record('Task.Output', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	choice :: uuid() | 'UsageContext'() | url() | uri() | unsignedInt() | 'TriggerDefinition'() | 'Timing'() | time() | string() | 'Signature'() | 'SampledData'() | 'RelatedArtifact'() | 'Reference'() | 'Ratio'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | positiveInt() | 'Period'() | 'ParameterDefinition'() | oid() | 'Money'() | markdown() | integer() | instant() | 'Identifier'() | id() | 'HumanName'() | 'Expression'() | 'Duration'() | 'Dosage'() | 'Distance'() | decimal() | dateTime() | date() | 'DataRequirement'() | 'Count'() | 'Contributor'() | 'ContactPoint'() | 'ContactDetail'() | 'Coding'() | 'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | 'Attachment'() | 'Annotation'() | 'Age'() | 'Address'()}).

-type 'Task.Output'() :: #'Task.Output'{}.


-record('Task.Input', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'CodeableConcept'(),
	choice :: uuid() | 'UsageContext'() | url() | uri() | unsignedInt() | 'TriggerDefinition'() | 'Timing'() | time() | string() | 'Signature'() | 'SampledData'() | 'RelatedArtifact'() | 'Reference'() | 'Ratio'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | positiveInt() | 'Period'() | 'ParameterDefinition'() | oid() | 'Money'() | markdown() | integer() | instant() | 'Identifier'() | id() | 'HumanName'() | 'Expression'() | 'Duration'() | 'Dosage'() | 'Distance'() | decimal() | dateTime() | date() | 'DataRequirement'() | 'Count'() | 'Contributor'() | 'ContactPoint'() | 'ContactDetail'() | 'Coding'() | 'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | 'Attachment'() | 'Annotation'() | 'Age'() | 'Address'()}).

-type 'Task.Input'() :: #'Task.Input'{}.


-record('Task.Restriction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	repetitions :: positiveInt() | undefined,
	period :: 'Period'() | undefined,
	recipient :: ['Reference'()] | undefined}).

-type 'Task.Restriction'() :: #'Task.Restriction'{}.


-record('Task', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	instantiatesCanonical :: canonical() | undefined,
	instantiatesUri :: uri() | undefined,
	basedOn :: ['Reference'()] | undefined,
	groupIdentifier :: 'Identifier'() | undefined,
	partOf :: ['Reference'()] | undefined,
	status :: 'TaskStatus'(),
	statusReason :: 'CodeableConcept'() | undefined,
	businessStatus :: 'CodeableConcept'() | undefined,
	intent :: 'TaskIntent'(),
	priority :: 'RequestPriority'() | undefined,
	code :: 'CodeableConcept'() | undefined,
	description :: string() | undefined,
	focus :: 'Reference'() | undefined,
	for :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	executionPeriod :: 'Period'() | undefined,
	authoredOn :: dateTime() | undefined,
	lastModified :: dateTime() | undefined,
	requester :: 'Reference'() | undefined,
	performerType :: ['CodeableConcept'()] | undefined,
	owner :: 'Reference'() | undefined,
	location :: 'Reference'() | undefined,
	reasonCode :: 'CodeableConcept'() | undefined,
	reasonReference :: 'Reference'() | undefined,
	insurance :: ['Reference'()] | undefined,
	note :: ['Annotation'()] | undefined,
	relevantHistory :: ['Reference'()] | undefined,
	restriction :: 'Task.Restriction'() | undefined,
	input :: ['Task.Input'()] | undefined,
	output :: ['Task.Output'()] | undefined}).

-type 'Task'() :: #'Task'{}.
