-record('Goal.Target', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	measure :: 'CodeableConcept'() | undefined,
	choice :: string() | 'Ratio'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | integer() | 'CodeableConcept'() | boolean() | undefined,
	choice1 :: 'Duration'() | date() | undefined}).

-type 'Goal.Target'() :: #'Goal.Target'{}.


-record('Goal', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	lifecycleStatus :: 'GoalLifecycleStatus'(),
	achievementStatus :: 'CodeableConcept'() | undefined,
	category :: ['CodeableConcept'()] | undefined,
	priority :: 'CodeableConcept'() | undefined,
	description :: 'CodeableConcept'(),
	subject :: 'Reference'(),
	choice :: date() | 'CodeableConcept'() | undefined,
	target :: ['Goal.Target'()] | undefined,
	statusDate :: date() | undefined,
	statusReason :: string() | undefined,
	expressedBy :: 'Reference'() | undefined,
	addresses :: ['Reference'()] | undefined,
	note :: ['Annotation'()] | undefined,
	outcomeCode :: ['CodeableConcept'()] | undefined,
	outcomeReference :: ['Reference'()] | undefined}).

-type 'Goal'() :: #'Goal'{}.

