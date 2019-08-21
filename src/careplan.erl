-record('CarePlanIntent', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'CarePlanIntent'() :: #'CarePlanIntent'{}.


-record('CarePlanActivityStatus', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'CarePlanActivityStatus'() :: #'CarePlanActivityStatus'{}.


-record('CarePlanActivityKind', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'CarePlanActivityKind'() :: #'CarePlanActivityKind'{}.


-record('CarePlan.Detail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	kind :: 'CarePlanActivityKind'() | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	code :: 'CodeableConcept'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	goal :: ['Reference'()] | undefined,
	status :: 'CarePlanActivityStatus'(),
	statusReason :: 'CodeableConcept'() | undefined,
	doNotPerform :: boolean() | undefined,
	choice :: 'Timing'() | string() | 'Period'() | undefined,
	location :: 'Reference'() | undefined,
	performer :: ['Reference'()] | undefined,
	choice1 :: 'Reference'() | 'CodeableConcept'() | undefined,
	dailyAmount :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	quantity :: 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | undefined,
	description :: string() | undefined}).

-type 'CarePlan.Detail'() :: #'CarePlan.Detail'{}.


-record('CarePlan.Activity', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	outcomeCodeableConcept :: ['CodeableConcept'()] | undefined,
	outcomeReference :: ['Reference'()] | undefined,
	progress :: ['Annotation'()] | undefined,
	reference :: 'Reference'() | undefined,
	detail :: 'CarePlan.Detail'() | undefined}).

-type 'CarePlan.Activity'() :: #'CarePlan.Activity'{}.


-record('CarePlan', {anyAttribs :: anyAttribs(),
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
	partOf :: ['Reference'()] | undefined,
	status :: 'RequestStatus'(),
	intent :: 'CarePlanIntent'(),
	category :: ['CodeableConcept'()] | undefined,
	title :: string() | undefined,
	description :: string() | undefined,
	subject :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	period :: 'Period'() | undefined,
	created :: dateTime() | undefined,
	author :: 'Reference'() | undefined,
	contributor :: ['Reference'()] | undefined,
	careTeam :: ['Reference'()] | undefined,
	addresses :: ['Reference'()] | undefined,
	supportingInfo :: ['Reference'()] | undefined,
	goal :: ['Reference'()] | undefined,
	activity :: ['CarePlan.Activity'()] | undefined,
	note :: ['Annotation'()] | undefined}).

-type 'CarePlan'() :: #'CarePlan'{}.


-record('ConditionalDeleteStatus', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ConditionalDeleteStatus'() :: #'ConditionalDeleteStatus'{}.


-record('ReferenceHandlingPolicy', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ReferenceHandlingPolicy'() :: #'ReferenceHandlingPolicy'{}.


-record('ConditionalReadStatus', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'ConditionalReadStatus'() :: #'ConditionalReadStatus'{}.


-record('SystemRestfulInteraction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'SystemRestfulInteraction'() :: #'SystemRestfulInteraction'{}.


-record('TypeRestfulInteraction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: ['Extension'()] | undefined}).

-type 'TypeRestfulInteraction'() :: #'TypeRestfulInteraction'{}.


-record('RestfulCapabilityMode', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,

