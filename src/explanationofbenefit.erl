-module(explanationofbenefit).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('ExplanationOfBenefit.Financial', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	choice :: unsignedInt() | string() | complex:'Money'() | undefined,
	choice1 :: unsignedInt() | complex:'Money'() | undefined}).

-type 'ExplanationOfBenefit.Financial'() :: #'ExplanationOfBenefit.Financial'{}.


-record('ExplanationOfBenefit.BenefitBalance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	category :: complex:'CodeableConcept'(),
	excluded :: boolean() | undefined,
	name :: string() | undefined,
	description :: string() | undefined,
	network :: complex:'CodeableConcept'() | undefined,
	unit :: complex:'CodeableConcept'() | undefined,
	term :: complex:'CodeableConcept'() | undefined,
	financial :: [complex:'ExplanationOfBenefit.Financial'()] | undefined}).

-type 'ExplanationOfBenefit.BenefitBalance'() :: #'ExplanationOfBenefit.BenefitBalance'{}.


-record('ExplanationOfBenefit.ProcessNote', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	number :: positiveInt() | undefined,
	type :: complex:'NoteType'() | undefined,
	text :: string() | undefined,
	language :: complex:'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.ProcessNote'() :: #'ExplanationOfBenefit.ProcessNote'{}.


-record('ExplanationOfBenefit.Payment', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	adjustment :: complex:'Money'() | undefined,
	adjustmentReason :: complex:'CodeableConcept'() | undefined,
	date :: date() | undefined,
	amount :: complex:'Money'() | undefined,
	identifier :: complex:'Identifier'() | undefined}).

-type 'ExplanationOfBenefit.Payment'() :: #'ExplanationOfBenefit.Payment'{}.


-record('ExplanationOfBenefit.Total', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	category :: complex:'CodeableConcept'(),
	amount :: complex:'Money'()}).

-type 'ExplanationOfBenefit.Total'() :: #'ExplanationOfBenefit.Total'{}.


-record('ExplanationOfBenefit.SubDetail1', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined}).

-type 'ExplanationOfBenefit.SubDetail1'() :: #'ExplanationOfBenefit.SubDetail1'{}.


-record('ExplanationOfBenefit.Detail1', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined,
	subDetail :: [complex:'ExplanationOfBenefit.SubDetail1'()] | undefined}).

-type 'ExplanationOfBenefit.Detail1'() :: #'ExplanationOfBenefit.Detail1'{}.


-record('ExplanationOfBenefit.AddItem', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	itemSequence :: [positiveInt()] | undefined,
	detailSequence :: [positiveInt()] | undefined,
	subDetailSequence :: [positiveInt()] | undefined,
	provider :: [special:'Reference'()] | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	programCode :: [complex:'CodeableConcept'()] | undefined,
	choice :: complex:'Period'() | date() | undefined,
	choice1 :: special:'Reference'() | complex:'CodeableConcept'() | complex:'Address'() | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	bodySite :: complex:'CodeableConcept'() | undefined,
	subSite :: [complex:'CodeableConcept'()] | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined,
	detail :: [complex:'ExplanationOfBenefit.Detail1'()] | undefined}).

-type 'ExplanationOfBenefit.AddItem'() :: #'ExplanationOfBenefit.AddItem'{}.


-record('ExplanationOfBenefit.SubDetail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	revenue :: complex:'CodeableConcept'() | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	programCode :: [complex:'CodeableConcept'()] | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	udi :: [special:'Reference'()] | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined}).

-type 'ExplanationOfBenefit.SubDetail'() :: #'ExplanationOfBenefit.SubDetail'{}.


-record('ExplanationOfBenefit.Detail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	revenue :: complex:'CodeableConcept'() | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	programCode :: [complex:'CodeableConcept'()] | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	udi :: [special:'Reference'()] | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined,
	subDetail :: [complex:'ExplanationOfBenefit.SubDetail'()] | undefined}).

-type 'ExplanationOfBenefit.Detail'() :: #'ExplanationOfBenefit.Detail'{}.


-record('ExplanationOfBenefit.Adjudication', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	category :: complex:'CodeableConcept'(),
	reason :: complex:'CodeableConcept'() | undefined,
	amount :: complex:'Money'() | undefined,
	value :: decimal() | undefined}).

-type 'ExplanationOfBenefit.Adjudication'() :: #'ExplanationOfBenefit.Adjudication'{}.

-record('ExplanationOfBenefit.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	careTeamSequence :: [positiveInt()] | undefined,
	diagnosisSequence :: [positiveInt()] | undefined,
	procedureSequence :: [positiveInt()] | undefined,
	informationSequence :: [positiveInt()] | undefined,
	revenue :: complex:'CodeableConcept'() | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	productOrService :: complex:'CodeableConcept'(),
	modifier :: [complex:'CodeableConcept'()] | undefined,
	programCode :: [complex:'CodeableConcept'()] | undefined,
	choice :: complex:'Period'() | date() | undefined,
	choice1 :: special:'Reference'() | complex:'CodeableConcept'() | complex:'Address'() | undefined,
	quantity :: complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | undefined,
	unitPrice :: complex:'Money'() | undefined,
	factor :: decimal() | undefined,
	net :: complex:'Money'() | undefined,
	udi :: [special:'Reference'()] | undefined,
	bodySite :: complex:'CodeableConcept'() | undefined,
	subSite :: [complex:'CodeableConcept'()] | undefined,
	encounter :: [special:'Reference'()] | undefined,
	noteNumber :: [positiveInt()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined,
	detail :: [complex:'ExplanationOfBenefit.Detail'()] | undefined}).

-type 'ExplanationOfBenefit.Item'() :: #'ExplanationOfBenefit.Item'{}.


-record('ExplanationOfBenefit.Accident', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	date :: date() | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	choice :: special:'Reference'() | complex:'Address'() | undefined}).

-type 'ExplanationOfBenefit.Accident'() :: #'ExplanationOfBenefit.Accident'{}.


-record('ExplanationOfBenefit.Insurance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	focal :: boolean(),
	coverage :: special:'Reference'(),
	preAuthRef :: [string()] | undefined}).

-type 'ExplanationOfBenefit.Insurance'() :: #'ExplanationOfBenefit.Insurance'{}.


-record('ExplanationOfBenefit.Procedure', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	type :: [complex:'CodeableConcept'()] | undefined,
	date :: dateTime() | undefined,
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	udi :: [special:'Reference'()] | undefined}).

-type 'ExplanationOfBenefit.Procedure'() :: #'ExplanationOfBenefit.Procedure'{}.


-record('ExplanationOfBenefit.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	type :: [complex:'CodeableConcept'()] | undefined,
	onAdmission :: complex:'CodeableConcept'() | undefined,
	packageCode :: complex:'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.Diagnosis'() :: #'ExplanationOfBenefit.Diagnosis'{}.


-record('ExplanationOfBenefit.SupportingInfo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	category :: complex:'CodeableConcept'(),
	code :: complex:'CodeableConcept'() | undefined,
	choice :: complex:'Period'() | date() | undefined,
	choice1 :: string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | boolean() | complex:'Attachment'() | undefined,
	reason :: complex:'Coding'() | undefined}).

-type 'ExplanationOfBenefit.SupportingInfo'() :: #'ExplanationOfBenefit.SupportingInfo'{}.

-record('ExplanationOfBenefit.CareTeam', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	provider :: special:'Reference'(),
	responsible :: boolean() | undefined,
	role :: complex:'CodeableConcept'() | undefined,
	qualification :: complex:'CodeableConcept'() | undefined}).

-type 'ExplanationOfBenefit.CareTeam'() :: #'ExplanationOfBenefit.CareTeam'{}.


-record('ExplanationOfBenefit.Payee', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	party :: special:'Reference'() | undefined}).

-type 'ExplanationOfBenefit.Payee'() :: #'ExplanationOfBenefit.Payee'{}.


-record('ExplanationOfBenefit.Related', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	claim :: special:'Reference'() | undefined,
	relationship :: complex:'CodeableConcept'() | undefined,
	reference :: complex:'Identifier'() | undefined}).

-type 'ExplanationOfBenefit.Related'() :: #'ExplanationOfBenefit.Related'{}.


-record('ExplanationOfBenefit', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'ExplanationOfBenefitStatus'(),
	type :: complex:'CodeableConcept'(),
	subType :: complex:'CodeableConcept'() | undefined,
	use :: complex:'Use'(),
	patient :: special:'Reference'(),
	billablePeriod :: complex:'Period'() | undefined,
	created :: dateTime(),
	enterer :: special:'Reference'() | undefined,
	insurer :: special:'Reference'(),
	provider :: special:'Reference'(),
	priority :: complex:'CodeableConcept'() | undefined,
	fundsReserveRequested :: complex:'CodeableConcept'() | undefined,
	fundsReserve :: complex:'CodeableConcept'() | undefined,
	related :: [complex:'ExplanationOfBenefit.Related'()] | undefined,
	prescription :: special:'Reference'() | undefined,
	originalPrescription :: special:'Reference'() | undefined,
	payee :: complex:'ExplanationOfBenefit.Payee'() | undefined,
	referral :: special:'Reference'() | undefined,
	facility :: special:'Reference'() | undefined,
	claim :: special:'Reference'() | undefined,
	claimResponse :: special:'Reference'() | undefined,
	outcome :: complex:'ClaimProcessingCodes'(),
	disposition :: string() | undefined,
	preAuthRef :: [string()] | undefined,
	preAuthRefPeriod :: [complex:'Period'()] | undefined,
	careTeam :: [complex:'ExplanationOfBenefit.CareTeam'()] | undefined,
	supportingInfo :: [complex:'ExplanationOfBenefit.SupportingInfo'()] | undefined,
	diagnosis :: [complex:'ExplanationOfBenefit.Diagnosis'()] | undefined,
	procedure :: [complex:'ExplanationOfBenefit.Procedure'()] | undefined,
	precedence :: positiveInt() | undefined,
	insurance :: [complex:'ExplanationOfBenefit.Insurance'()],
	accident :: complex:'ExplanationOfBenefit.Accident'() | undefined,
	item :: [complex:'ExplanationOfBenefit.Item'()] | undefined,
	addItem :: [complex:'ExplanationOfBenefit.AddItem'()] | undefined,
	adjudication :: [complex:'ExplanationOfBenefit.Adjudication'()] | undefined,
	total :: [complex:'ExplanationOfBenefit.Total'()] | undefined,
	payment :: complex:'ExplanationOfBenefit.Payment'() | undefined,
	formCode :: complex:'CodeableConcept'() | undefined,
	form :: complex:'Attachment'() | undefined,
	processNote :: [complex:'ExplanationOfBenefit.ProcessNote'()] | undefined,
	benefitPeriod :: complex:'Period'() | undefined,
	benefitBalance :: [complex:'ExplanationOfBenefit.BenefitBalance'()] | undefined}).

-type 'ExplanationOfBenefit'() :: #'ExplanationOfBenefit'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_explanationOfBenefit({Props}) -> to_explanationOfBenefit(Props);
to_explanationOfBenefit(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit">>),
  #'ExplanationOfBenefit'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , subType  = decode:value(<<"subType">>, Props, DT)
    , use  = decode:value(<<"use">>, Props, DT)
    , patient  = decode:value(<<"patient">>, Props, DT)
    , billablePeriod  = decode:value(<<"billablePeriod">>, Props, DT)
    , created  = decode:value(<<"created">>, Props, DT)
    , enterer  = decode:value(<<"enterer">>, Props, DT)
    , insurer  = decode:value(<<"insurer">>, Props, DT)
    , provider  = decode:value(<<"provider">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , fundsReserveRequested  = decode:value(<<"fundsReserveRequested">>, Props, DT)
    , fundsReserve  = decode:value(<<"fundsReserve">>, Props, DT)
    , related  = decode:value(<<"related">>, Props, DT)
    , prescription  = decode:value(<<"prescription">>, Props, DT)
    , originalPrescription  = decode:value(<<"originalPrescription">>, Props, DT)
    , payee  = decode:value(<<"payee">>, Props, DT)
    , referral  = decode:value(<<"referral">>, Props, DT)
    , facility  = decode:value(<<"facility">>, Props, DT)
    , claim  = decode:value(<<"claim">>, Props, DT)
    , claimResponse  = decode:value(<<"claimResponse">>, Props, DT)
    , outcome  = decode:value(<<"outcome">>, Props, DT)
    , disposition  = decode:value(<<"disposition">>, Props, DT)
    , preAuthRef  = decode:value(<<"preAuthRef">>, Props, DT)
    , preAuthRefPeriod  = decode:value(<<"preAuthRefPeriod">>, Props, DT)
    , careTeam  = decode:value(<<"careTeam">>, Props, DT)
    , supportingInfo  = decode:value(<<"supportingInfo">>, Props, DT)
    , diagnosis  = decode:value(<<"diagnosis">>, Props, DT)
    , procedure  = decode:value(<<"procedure">>, Props, DT)
    , precedence  = decode:value(<<"precedence">>, Props, DT)
    , insurance  = decode:value(<<"insurance">>, Props, DT)
    , accident  = decode:value(<<"accident">>, Props, DT)
    , item  = decode:value(<<"item">>, Props, DT)
    , addItem  = decode:value(<<"addItem">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    , total  = decode:value(<<"total">>, Props, DT)
    , payment  = decode:value(<<"payment">>, Props, DT)
    , formCode  = decode:value(<<"formCode">>, Props, DT)
    , form  = decode:value(<<"form">>, Props, DT)
    , processNote  = decode:value(<<"processNote">>, Props, DT)
    , benefitPeriod  = decode:value(<<"benefitPeriod">>, Props, DT)
    , benefitBalance  = decode:value(<<"benefitBalance">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_explanationOfBenefit.Financial({Props}) -> to_explanationOfBenefit.Financial(Props);
to_explanationOfBenefit.Financial(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit">>),
  #'ExplanationOfBenefit'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    }.


to_explanationOfBenefit.BenefitBalance({Props}) -> to_explanationOfBenefit.BenefitBalance(Props);
to_explanationOfBenefit.BenefitBalance(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Financial">>),
  #'ExplanationOfBenefit.Financial'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , excluded  = decode:value(<<"excluded">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , network  = decode:value(<<"network">>, Props, DT)
    , unit  = decode:value(<<"unit">>, Props, DT)
    , term  = decode:value(<<"term">>, Props, DT)
    , financial  = decode:value(<<"financial">>, Props, DT)
    }.


to_explanationOfBenefit.ProcessNote({Props}) -> to_explanationOfBenefit.ProcessNote(Props);
to_explanationOfBenefit.ProcessNote(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.ProcessNote">>),
  #'ExplanationOfBenefit.ProcessNote'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , number  = decode:value(<<"number">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , text  = decode:value(<<"text">>, Props, DT)
    , language  = decode:value(<<"language">>, Props, DT)
    }.


to_explanationOfBenefit.Payment({Props}) -> to_explanationOfBenefit.Payment(Props);
to_explanationOfBenefit.Payment(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Payment">>),
  #'ExplanationOfBenefit.Payment'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , adjustment  = decode:value(<<"adjustment">>, Props, DT)
    , adjustmentReason  = decode:value(<<"adjustmentReason">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , amount  = decode:value(<<"amount">>, Props, DT)
    , identifier  = decode:value(<<"identifier">>, Props, DT)
    }.


to_explanationOfBenefit.Total({Props}) -> to_explanationOfBenefit.Total(Props);
to_explanationOfBenefit.Total(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Total">>),
  #'ExplanationOfBenefit.Total'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , amount  = decode:value(<<"amount">>, Props, DT)
    }.


to_explanationOfBenefit.SubDetail1({Props}) -> to_explanationOfBenefit.SubDetail1(Props);
to_explanationOfBenefit.SubDetail1(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.SubDetail1">>),
  #'ExplanationOfBenefit.SubDetail1'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    }.


to_explanationOfBenefit.Detail1({Props}) -> to_explanationOfBenefit.Detail1(Props);
to_explanationOfBenefit.Detail1(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Detail1">>),
  #'ExplanationOfBenefit.Detail1'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    , subDetail  = decode:value(<<"subDetail">>, Props, DT)
    }.


to_explanationOfBenefit.AddItem({Props}) -> to_explanationOfBenefit.AddItem(Props);
to_explanationOfBenefit.AddItem(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.AddItem">>),
  #'ExplanationOfBenefit.AddItem'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , itemSequence  = decode:value(<<"itemSequence">>, Props, DT)
    , detailSequence  = decode:value(<<"detailSequence">>, Props, DT)
    , subDetailSequence  = decode:value(<<"subDetailSequence">>, Props, DT)
    , provider  = decode:value(<<"provider">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , programCode  = decode:value(<<"programCode">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , subSite  = decode:value(<<"subSite">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    , detail  = decode:value(<<"detail">>, Props, DT)
    }.


to_explanationOfBenefit.SubDetail({Props}) -> to_explanationOfBenefit.SubDetail(Props);
to_explanationOfBenefit.SubDetail(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefiti.SubDetail">>),
  #'ExplanationOfBenefit.SubDetail'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , revenue  = decode:value(<<"revenue">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , programCode  = decode:value(<<"programCode">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , udi  = decode:value(<<"udi">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    }.


to_explanationOfBenefit.Detail({Props}) -> to_explanationOfBenefit.Detail(Props);
to_explanationOfBenefit.Detail(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Detail">>),
  #'ExplanationOfBenefit.Detail'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , revenue  = decode:value(<<"revenue">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , programCode  = decode:value(<<"programCode">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , udi  = decode:value(<<"udi">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    , subDetail  = decode:value(<<"subDetail">>, Props, DT)
    }.


to_explanationOfBenefit.Adjudication({Props}) -> to_explanationOfBenefit.Adjudication(Props);
to_explanationOfBenefit.Adjudication(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Adjudication">>),
  #'ExplanationOfBenefit.Adjudication'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , reason  = decode:value(<<"reason">>, Props, DT)
    , amount  = decode:value(<<"amount">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    }.


to_explanationOfBenefit.Item({Props}) -> to_explanationOfBenefit.Item(Props);
to_explanationOfBenefit.Item(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Item">>),
  #'ExplanationOfBenefit.Item'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , careTeamSequence  = decode:value(<<"careTeamSequence">>, Props, DT)
    , diagnosisSequence  = decode:value(<<"diagnosisSequence">>, Props, DT)
    , procedureSequence  = decode:value(<<"procedureSequence">>, Props, DT)
    , informationSequence  = decode:value(<<"informationSequence">>, Props, DT)
    , revenue  = decode:value(<<"revenue">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , productOrService  = decode:value(<<"productOrService">>, Props, DT)
    , modifier  = decode:value(<<"modifier">>, Props, DT)
    , programCode  = decode:value(<<"programCode">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , unitPrice  = decode:value(<<"unitPrice">>, Props, DT)
    , factor  = decode:value(<<"factor">>, Props, DT)
    , net  = decode:value(<<"net">>, Props, DT)
    , udi  = decode:value(<<"udi">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , subSite  = decode:value(<<"subSite">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , noteNumber  = decode:value(<<"noteNumber">>, Props, DT)
    , adjudication  = decode:value(<<"adjudication">>, Props, DT)
    , detail  = decode:value(<<"detail">>, Props, DT)
    }.


to_explanationOfBenefit.Accident({Props}) -> to_explanationOfBenefit.Accident(Props);
to_explanationOfBenefit.Accident(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Accident">>),
  #'ExplanationOfBenefit.Accident'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.


to_explanationOfBenefit.Insurance({Props}) -> to_explanationOfBenefit.Insurance(Props);
to_explanationOfBenefit.Insurance(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Insurance">>),
  #'ExplanationOfBenefit.Insurance'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , focal  = decode:value(<<"focal">>, Props, DT)
    , coverage  = decode:value(<<"coverage">>, Props, DT)
    , preAuthRef  = decode:value(<<"preAuthRef">>, Props, DT)
    }.


to_explanationOfBenefit.Procedure({Props}) -> to_explanationOfBenefit.Procedure(Props);
to_explanationOfBenefit.Procedure(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Procedure">>),
  #'ExplanationOfBenefit.Procedure'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , udi  = decode:value(<<"udi">>, Props, DT)
    }.


to_explanationOfBenefit.Diagnosis({Props}) -> to_explanationOfBenefit.Diagnosis(Props);
to_explanationOfBenefit.Diagnosis(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Diagnosis">>),
  #'ExplanationOfBenefit.Diagnosis'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , onAdmission  = decode:value(<<"onAdmission">>, Props, DT)
    , packageCode  = decode:value(<<"packageCode">>, Props, DT)
    }.


to_explanationOfBenefit.SupportingInfo({Props}) -> to_explanationOfBenefit.SupportingInfo(Props);
to_explanationOfBenefit.SupportingInfo(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.SupportingInfo">>),
  #'ExplanationOfBenefit.SupportingInfo'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , reason  = decode:value(<<"reason">>, Props, DT)
    }.


to_explanationOfBenefit.CareTeam({Props}) -> to_explanationOfBenefit.CareTeam(Props);
to_explanationOfBenefit.CareTeam(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.CareTeam">>),
  #'ExplanationOfBenefit.CareTeam'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , provider  = decode:value(<<"provider">>, Props, DT)
    , responsible  = decode:value(<<"responsible">>, Props, DT)
    , role  = decode:value(<<"role">>, Props, DT)
    , qualification  = decode:value(<<"qualification">>, Props, DT)
    }.


to_explanationOfBenefit.Payee({Props}) -> to_explanationOfBenefit.Payee(Props);
to_explanationOfBenefit.Payee(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Payee">>),
  #'ExplanationOfBenefit.Payee'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , party  = decode:value(<<"party">>, Props, DT)
    }.


to_explanationOfBenefit.Related({Props}) -> to_explanationOfBenefit.Related(Props);
to_explanationOfBenefit.Related(Props) ->
  DT = decode:xsd_info(<<"ExplanationOfBenefit.Related">>),
  #'ExplanationOfBenefit.Related'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , claim  = decode:value(<<"claim">>, Props, DT)
    , relationship  = decode:value(<<"relationship">>, Props, DT)
    , reference  = decode:value(<<"reference">>, Props, DT)
    }.



text(#'ExplanationOfBenefit'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, explanationOfBenefit:to_explanationOfBenefit(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

explanationOfBenefit_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'ExplanationOfBenefit',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
explanationOfBenefit_toprop_test() ->
    ?asrtp({'ExplanationOfBenefit',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"ExplanationOfBenefit">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

explanationOfBenefit_json_test() ->
    ?asrtjson({'ExplanationOfBenefit',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"ExplanationOfBenefit\",\"id\":\"p-21666\"}">>).

-endif.



