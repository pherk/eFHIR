
-record('Procedure.FocalDevice', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	action :: 'CodeableConcept'() | undefined,
	manipulated :: 'Reference'()}).

-type 'Procedure.FocalDevice'() :: #'Procedure.FocalDevice'{}.


-record('Procedure.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	function :: 'CodeableConcept'() | undefined,
	actor :: 'Reference'(),
	onBehalfOf :: 'Reference'() | undefined}).

-type 'Procedure.Performer'() :: #'Procedure.Performer'{}.


-record('Procedure', {anyAttribs :: anyAttribs(),
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
	partOf :: ['Reference'()] | undefined,
	status :: 'EventStatus'(),
	statusReason :: 'CodeableConcept'() | undefined,
	category :: 'CodeableConcept'() | undefined,
	code :: 'CodeableConcept'() | undefined,
	subject :: 'Reference'(),
	encounter :: 'Reference'() | undefined,
	choice :: string() | 'Range'() | 'Period'() | dateTime() | 'Age'() | undefined,
	recorder :: 'Reference'() | undefined,
	asserter :: 'Reference'() | undefined,
	performer :: ['Procedure.Performer'()] | undefined,
	location :: 'Reference'() | undefined,
	reasonCode :: ['CodeableConcept'()] | undefined,
	reasonReference :: ['Reference'()] | undefined,
	bodySite :: ['CodeableConcept'()] | undefined,
	outcome :: 'CodeableConcept'() | undefined,
	report :: ['Reference'()] | undefined,
	complication :: ['CodeableConcept'()] | undefined,
	complicationDetail :: ['Reference'()] | undefined,
	followUp :: ['CodeableConcept'()] | undefined,
	note :: ['Annotation'()] | undefined,
	focalDevice :: ['Procedure.FocalDevice'()] | undefined,
	usedReference :: ['Reference'()] | undefined,
	usedCode :: ['CodeableConcept'()] | undefined}).

-type 'Procedure'() :: #'Procedure'{}.


