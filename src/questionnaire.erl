-record('Questionnaire.Initial', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	choice :: uri() | time() | string() | 'Reference'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | integer() | decimal() | dateTime() | date() | 'Coding'() | boolean() | 'Attachment'()}).

-type 'Questionnaire.Initial'() :: #'Questionnaire.Initial'{}.


-record('Questionnaire.AnswerOption', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	choice :: time() | string() | 'Reference'() | integer() | date() | 'Coding'(),
	initialSelected :: boolean() | undefined}).

-type 'Questionnaire.AnswerOption'() :: #'Questionnaire.AnswerOption'{}.


-record('Questionnaire.EnableWhen', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	question :: string(),
	operator :: 'QuestionnaireItemOperator'(),
	choice :: time() | string() | 'Reference'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | integer() | decimal() | dateTime() | date() | 'Coding'() | boolean()}).

-type 'Questionnaire.EnableWhen'() :: #'Questionnaire.EnableWhen'{}.


-record('Questionnaire.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	linkId :: string(),
	definition :: uri() | undefined,
	code :: ['Coding'()] | undefined,
	prefix :: string() | undefined,
	text :: string() | undefined,
	type :: 'QuestionnaireItemType'(),
	enableWhen :: ['Questionnaire.EnableWhen'()] | undefined,
	enableBehavior :: 'EnableWhenBehavior'() | undefined,
	required :: boolean() | undefined,
	repeats :: boolean() | undefined,
	readOnly :: boolean() | undefined,
	maxLength :: integer() | undefined,
	answerValueSet :: canonical() | undefined,
	answerOption :: ['Questionnaire.AnswerOption'()] | undefined,
	initial :: ['Questionnaire.Initial'()] | undefined,
	item :: ['Questionnaire.Item'()] | undefined}).

-type 'Questionnaire.Item'() :: #'Questionnaire.Item'{}.


-record('Questionnaire', {anyAttribs :: anyAttribs(),
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
	derivedFrom :: [canonical()] | undefined,
	status :: 'PublicationStatus'(),
	experimental :: boolean() | undefined,
	subjectType :: [code()] | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: ['ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: ['UsageContext'()] | undefined,
	jurisdiction :: ['CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: 'Period'() | undefined,
	code :: ['Coding'()] | undefined,
	item :: ['Questionnaire.Item'()] | undefined}).

-type 'Questionnaire'() :: #'Questionnaire'{}.


