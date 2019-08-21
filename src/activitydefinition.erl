-record('ActivityDefinition.DynamicValue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	path :: string(),
	expression :: 'Expression'()}).

-type 'ActivityDefinition.DynamicValue'() :: #'ActivityDefinition.DynamicValue'{}.


-record('ActivityDefinition.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'ActionParticipantType'(),
	role :: 'CodeableConcept'() | undefined}).

-type 'ActivityDefinition.Participant'() :: #'ActivityDefinition.Participant'{}.


-record('ActivityDefinition', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	url :: uri() | undefined,
	identifier :: ['Identifier'()] | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	subtitle :: string() | undefined,
	status :: 'PublicationStatus'(),
	experimental :: boolean() | undefined,
	choice :: 'Reference'() | 'CodeableConcept'() | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: ['ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: ['UsageContext'()] | undefined,
	jurisdiction :: ['CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	usage :: string() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: 'Period'() | undefined,
	topic :: ['CodeableConcept'()] | undefined,
	author :: ['ContactDetail'()] | undefined,
	editor :: ['ContactDetail'()] | undefined,
	reviewer :: ['ContactDetail'()] | undefined,
	endorser :: ['ContactDetail'()] | undefined,
	relatedArtifact :: ['RelatedArtifact'()] | undefined,
	library :: [canonical()] | undefined,
	kind :: 'RequestResourceType'() | undefined,
	profile :: canonical() | undefined,
	code :: 'CodeableConcept'() | undefined,
	intent :: 'RequestIntent'() | undefined,
	priority :: 'RequestPriority'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice1 :: 'Timing'() | 'Range'() | 'Period'() | 'Duration'() | dateTime() | 'Age'() | undefined,
	location :: 'Reference'() | undefined,
	participant :: ['ActivityDefinition.Participant'()] | undefined,
	choice2 :: 'Reference'() | 'CodeableConcept'() | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	dosage :: ['Dosage'()] | undefined,
	bodySite :: ['CodeableConcept'()] | undefined,
	specimenRequirement :: ['Reference'()] | undefined,
	observationRequirement :: ['Reference'()] | undefined,
	observationResultRequirement :: ['Reference'()] | undefined,
	transform :: canonical() | undefined,
	dynamicValue :: ['ActivityDefinition.DynamicValue'()] | undefined}).

-type 'ActivityDefinition'() :: #'ActivityDefinition'{}.



