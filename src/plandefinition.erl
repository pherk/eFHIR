-record('PlanDefinition.DynamicValue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	path :: string() | undefined,
	expression :: 'Expression'() | undefined}).

-type 'PlanDefinition.DynamicValue'() :: #'PlanDefinition.DynamicValue'{}.


-record('PlanDefinition.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	type :: 'ActionParticipantType'(),
	role :: 'CodeableConcept'() | undefined}).

-type 'PlanDefinition.Participant'() :: #'PlanDefinition.Participant'{}.


-record('PlanDefinition.RelatedAction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	actionId :: id(),
	relationship :: 'ActionRelationshipType'(),
	choice :: 'Range'() | 'Duration'() | undefined}).

-type 'PlanDefinition.RelatedAction'() :: #'PlanDefinition.RelatedAction'{}.


-record('PlanDefinition.Condition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	kind :: 'ActionConditionKind'(),
	expression :: 'Expression'() | undefined}).

-type 'PlanDefinition.Condition'() :: #'PlanDefinition.Condition'{}.


-record('PlanDefinition.Action', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	prefix :: string() | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	textEquivalent :: string() | undefined,
	priority :: 'RequestPriority'() | undefined,
	code :: ['CodeableConcept'()] | undefined,
	reason :: ['CodeableConcept'()] | undefined,
	documentation :: ['RelatedArtifact'()] | undefined,
	goalId :: [id()] | undefined,
	choice :: 'Reference'() | 'CodeableConcept'() | undefined,
	trigger :: ['TriggerDefinition'()] | undefined,
	condition :: ['PlanDefinition.Condition'()] | undefined,
	input :: ['DataRequirement'()] | undefined,
	output :: ['DataRequirement'()] | undefined,
	relatedAction :: ['PlanDefinition.RelatedAction'()] | undefined,
	choice1 :: 'Timing'() | 'Range'() | 'Period'() | 'Duration'() | dateTime() | 'Age'() | undefined,
	participant :: ['PlanDefinition.Participant'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	groupingBehavior :: 'ActionGroupingBehavior'() | undefined,
	selectionBehavior :: 'ActionSelectionBehavior'() | undefined,
	requiredBehavior :: 'ActionRequiredBehavior'() | undefined,
	precheckBehavior :: 'ActionPrecheckBehavior'() | undefined,
	cardinalityBehavior :: 'ActionCardinalityBehavior'() | undefined,
	choice2 :: uri() | canonical() | undefined,
	transform :: canonical() | undefined,
	dynamicValue :: ['PlanDefinition.DynamicValue'()] | undefined,
	action :: ['PlanDefinition.Action'()] | undefined}).

-type 'PlanDefinition.Action'() :: #'PlanDefinition.Action'{}.


-record('PlanDefinition.Target', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	measure :: 'CodeableConcept'() | undefined,
	choice :: 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | 'CodeableConcept'() | undefined,
	due :: 'Duration'() | undefined}).

-type 'PlanDefinition.Target'() :: #'PlanDefinition.Target'{}.


-record('PlanDefinition.Goal', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	category :: 'CodeableConcept'() | undefined,
	description :: 'CodeableConcept'(),
	priority :: 'CodeableConcept'() | undefined,
	start :: 'CodeableConcept'() | undefined,
	addresses :: ['CodeableConcept'()] | undefined,
	documentation :: ['RelatedArtifact'()] | undefined,
	target :: ['PlanDefinition.Target'()] | undefined}).

-type 'PlanDefinition.Goal'() :: #'PlanDefinition.Goal'{}.


-record('PlanDefinition', {anyAttribs :: anyAttribs(),
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
	type :: 'CodeableConcept'() | undefined,
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
	goal :: ['PlanDefinition.Goal'()] | undefined,
	action :: ['PlanDefinition.Action'()] | undefined}).

-type 'PlanDefinition'() :: #'PlanDefinition'{}.



