-record('RequestGroup.RelatedAction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	actionId :: id(),
	relationship :: 'ActionRelationshipType'(),
	choice :: 'Range'() | 'Duration'() | undefined}).

-type 'RequestGroup.RelatedAction'() :: #'RequestGroup.RelatedAction'{}.


-record('RequestGroup.Condition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	kind :: 'ActionConditionKind'(),
	expression :: 'Expression'() | undefined}).

-type 'RequestGroup.Condition'() :: #'RequestGroup.Condition'{}.


-record('RequestGroup.Action', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	prefix :: string() | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	textEquivalent :: string() | undefined,
	priority :: 'RequestPriority'() | undefined,
	code :: ['CodeableConcept'()] | undefined,
	documentation :: ['RelatedArtifact'()] | undefined,
	condition :: ['RequestGroup.Condition'()] | undefined,
	relatedAction :: ['RequestGroup.RelatedAction'()] | undefined,
	choice :: 'Timing'() | 'Range'() | 'Period'() | 'Duration'() | dateTime() | 'Age'() | undefined,
	participant :: ['Reference'()] | undefined,
	type :: 'CodeableConcept'() | undefined,
	groupingBehavior :: 'ActionGroupingBehavior'() | undefined,
	selectionBehavior :: 'ActionSelectionBehavior'() | undefined,
	requiredBehavior :: 'ActionRequiredBehavior'() | undefined,
	precheckBehavior :: 'ActionPrecheckBehavior'() | undefined,
	cardinalityBehavior :: 'ActionCardinalityBehavior'() | undefined,
	resource :: 'Reference'() | undefined,
	action :: ['RequestGroup.Action'()] | undefined}).

-type 'RequestGroup.Action'() :: #'RequestGroup.Action'{}.


-record('RequestGroup', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: ['Reference'()] | undefined,
	replaces :: ['Reference'()] | undefined,
	groupIdentifier :: 'Identifier'() | undefined,
	status :: 'RequestStatus'(),
	intent :: 'RequestIntent'(),
	priority :: 'RequestPriority'() | undefined,
	code :: 'CodeableConcept'() | undefined,
	subject :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	authoredOn :: dateTime() | undefined,
	author :: 'Reference'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	note :: ['Annotation'()] | undefined,
	action :: ['RequestGroup.Action'()] | undefined}).

-type 'RequestGroup'() :: #'RequestGroup'{}.



