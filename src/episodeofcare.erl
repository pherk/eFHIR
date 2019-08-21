-record('EpisodeOfCare.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	condition :: 'Reference'(),
	role :: 'CodeableConcept'() | undefined,
	rank :: positiveInt() | undefined}).

-type 'EpisodeOfCare.Diagnosis'() :: #'EpisodeOfCare.Diagnosis'{}.


-record('EpisodeOfCare.StatusHistory', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	status :: 'EpisodeOfCareStatus'(),
	period :: 'Period'()}).

-type 'EpisodeOfCare.StatusHistory'() :: #'EpisodeOfCare.StatusHistory'{}.


-record('EpisodeOfCare', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: 'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: 'Narrative'() | undefined,
	contained :: ['ResourceContainer'()] | undefined,
	extension :: ['Extension'()] | undefined,
	modifierExtension :: ['Extension'()] | undefined,
	identifier :: ['Identifier'()] | undefined,
	status :: 'EpisodeOfCareStatus'(),
	statusHistory :: ['EpisodeOfCare.StatusHistory'()] | undefined,
	type :: ['CodeableConcept'()] | undefined,
	diagnosis :: ['EpisodeOfCare.Diagnosis'()] | undefined,
	patient :: 'Reference'(),
	managingOrganization :: 'Reference'() | undefined,
	period :: 'Period'() | undefined,
	referralRequest :: ['Reference'()] | undefined,
	careManager :: 'Reference'() | undefined,
	team :: ['Reference'()] | undefined,
	account :: ['Reference'()] | undefined}).

-type 'EpisodeOfCare'() :: #'EpisodeOfCare'{}.

