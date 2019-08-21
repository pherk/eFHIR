-record('Group.Member', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	entity :: 'Reference'(),
	period :: 'Period'() | undefined,
	inactive :: boolean() | undefined}).

-type 'Group.Member'() :: #'Group.Member'{}.


-record('Group.Characteristic', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	code :: 'CodeableConcept'(),
	choice :: 'Reference'() | 'Range'() | 'Quantity'() | 'Duration'() | 'Age'() | 'Distance'() | 'Count'() | 'CodeableConcept'() | boolean(),
	exclude :: boolean(),
	period :: 'Period'() | undefined}).

-type 'Group.Characteristic'() :: #'Group.Characteristic'{}.


-record('Group', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	active :: boolean() | undefined,
	type :: 'GroupType'(),
	actual :: boolean(),
	code :: 'CodeableConcept'() | undefined,
	name :: string() | undefined,
	quantity :: unsignedInt() | undefined,
	managingEntity :: 'Reference'() | undefined,
	characteristic :: ['Group.Characteristic'()] | undefined,
	member :: ['Group.Member'()] | undefined}).

-type 'Group'() :: #'Group'{}.


