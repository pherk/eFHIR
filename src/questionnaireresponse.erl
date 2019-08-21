-record('QuestionnaireResponse.Answer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	choice :: uri() | time() | string() | 'Reference'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | integer() | decimal() | dateTime() | date() | 'Coding'() | boolean() | 'Attachment'() | undefined,
	item :: ['QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse.Answer'() :: #'QuestionnaireResponse.Answer'{}.


-record('QuestionnaireResponse.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	linkId :: string(),
	definition :: uri() | undefined,
	text :: string() | undefined,
	answer :: ['QuestionnaireResponse.Answer'()] | undefined,
	item :: ['QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse.Item'() :: #'QuestionnaireResponse.Item'{}.


-record('QuestionnaireResponse', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: 'Identifier'() | undefined,
	basedOn :: ['Reference'()] | undefined,
	partOf :: ['Reference'()] | undefined,
	questionnaire :: canonical() | undefined,
	status :: 'QuestionnaireResponseStatus'(),
	subject :: 'Reference'() | undefined,
	encounter :: 'Reference'() | undefined,
	authored :: dateTime() | undefined,
	author :: 'Reference'() | undefined,
	source :: 'Reference'() | undefined,
	item :: ['QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse'() :: #'QuestionnaireResponse'{}.


