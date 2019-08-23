-module(claim).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Claim.SubDetail', {anyAttribs :: anyAttribs(),
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
	udi :: [special:'Reference'()] | undefined}).

-type 'Claim.SubDetail'() :: #'Claim.SubDetail'{}.


-record('Claim.Detail', {anyAttribs :: anyAttribs(),
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
	subDetail :: [complex:'Claim.SubDetail'()] | undefined}).

-type 'Claim.Detail'() :: #'Claim.Detail'{}.


-record('Claim.Item', {anyAttribs :: anyAttribs(),
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
	detail :: [complex:'Claim.Detail'()] | undefined}).

-type 'Claim.Item'() :: #'Claim.Item'{}.


-record('Claim.Accident', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	date :: date(),
	type :: complex:'CodeableConcept'() | undefined,
	choice :: special:'Reference'() | complex:'Address'() | undefined}).

-type 'Claim.Accident'() :: #'Claim.Accident'{}.


-record('Claim.Insurance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	focal :: boolean(),
	identifier :: complex:'Identifier'() | undefined,
	coverage :: special:'Reference'(),
	businessArrangement :: string() | undefined,
	preAuthRef :: [string()] | undefined,
	claimResponse :: special:'Reference'() | undefined}).

-type 'Claim.Insurance'() :: #'Claim.Insurance'{}.


-record('Claim.Procedure', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	type :: [complex:'CodeableConcept'()] | undefined,
	date :: dateTime() | undefined,
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	udi :: [special:'Reference'()] | undefined}).

-type 'Claim.Procedure'() :: #'Claim.Procedure'{}.


-record('Claim.Diagnosis', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	type :: [complex:'CodeableConcept'()] | undefined,
	onAdmission :: complex:'CodeableConcept'() | undefined,
	packageCode :: complex:'CodeableConcept'() | undefined}).

-type 'Claim.Diagnosis'() :: #'Claim.Diagnosis'{}.


-record('Claim.SupportingInfo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	category :: complex:'CodeableConcept'(),
	code :: complex:'CodeableConcept'() | undefined,
	choice :: complex:'Period'() | date() | undefined,
	choice1 :: string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | boolean() | complex:'Attachment'() | undefined,
	reason :: complex:'CodeableConcept'() | undefined}).

-type 'Claim.SupportingInfo'() :: #'Claim.SupportingInfo'{}.


-record('Claim.CareTeam', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	provider :: special:'Reference'(),
	responsible :: boolean() | undefined,
	role :: complex:'CodeableConcept'() | undefined,
	qualification :: complex:'CodeableConcept'() | undefined}).

-type 'Claim.CareTeam'() :: #'Claim.CareTeam'{}.


-record('Claim.Payee', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	party :: special:'Reference'() | undefined}).

-type 'Claim.Payee'() :: #'Claim.Payee'{}.


-record('Claim.Related', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	claim :: special:'Reference'() | undefined,
	relationship :: complex:'CodeableConcept'() | undefined,
	reference :: complex:'Identifier'() | undefined}).

-type 'Claim.Related'() :: #'Claim.Related'{}.


-record('Claim', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'FinancialResourceStatusCodes'(),
	type :: complex:'CodeableConcept'(),
	subType :: complex:'CodeableConcept'() | undefined,
	use :: complex:'Use'(),
	patient :: special:'Reference'(),
	billablePeriod :: complex:'Period'() | undefined,
	created :: dateTime(),
	enterer :: special:'Reference'() | undefined,
	insurer :: special:'Reference'() | undefined,
	provider :: special:'Reference'(),
	priority :: complex:'CodeableConcept'(),
	fundsReserve :: complex:'CodeableConcept'() | undefined,
	related :: [complex:'Claim.Related'()] | undefined,
	prescription :: special:'Reference'() | undefined,
	originalPrescription :: special:'Reference'() | undefined,
	payee :: complex:'Claim.Payee'() | undefined,
	referral :: special:'Reference'() | undefined,
	facility :: special:'Reference'() | undefined,
	careTeam :: [complex:'Claim.CareTeam'()] | undefined,
	supportingInfo :: [complex:'Claim.SupportingInfo'()] | undefined,
	diagnosis :: [complex:'Claim.Diagnosis'()] | undefined,
	procedure :: [complex:'Claim.Procedure'()] | undefined,
	insurance :: [complex:'Claim.Insurance'()],
	accident :: complex:'Claim.Accident'() | undefined,
	item :: [complex:'Claim.Item'()] | undefined,
	total :: complex:'Money'() | undefined}).

-type 'Claim'() :: #'Claim'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_claim({Props}) -> to_claim(Props);
to_claim(Props) ->
  DT = decode:xsd_info(<<"Claim">>),
  #'Claim'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier  = decode:value(<<"identifier">>, Props, DT)
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
    , fundsReserve  = decode:value(<<"fundsReserve">>, Props, DT)
    , related  = decode:value(<<"related">>, Props, DT)
    , prescription  = decode:value(<<"prescription">>, Props, DT)
    , originalPrescription  = decode:value(<<"originalPrescription">>, Props, DT)
    , payee  = decode:value(<<"payee">>, Props, DT)
    , referral  = decode:value(<<"referral">>, Props, DT)
    , facility  = decode:value(<<"facility">>, Props, DT)
    , careTeam  = decode:value(<<"careTeam">>, Props, DT)
    , supportingInfo  = decode:value(<<"supportingInfo">>, Props, DT)
    , diagnosis  = decode:value(<<"diagnosis">>, Props, DT)
    , procedure  = decode:value(<<"procedure">>, Props, DT)
    , insurance  = decode:value(<<"insurance">>, Props, DT)
    , accident  = decode:value(<<"accident">>, Props, DT)
    , item  = decode:value(<<"item">>, Props, DT)
    , total  = decode:value(<<"total">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_claim_subDetail({Props}) ->  to_claim_subDetail(Props);
to_claim_subDetail(Props) -> 
  DT = decode:xsd_info(<<"Claim.SubDetail">>),
  #'Claim.SubDetail'{ 
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
    }.



to_claim_detail({Props}) ->  to_claim_detail(Props);
to_claim_detail(Props) -> 
  DT = decode:xsd_info(<<"Claim.Detail">>),
  #'Claim.Detail'{ 
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
    , subDetail  = decode:value(<<"subDetail">>, Props, DT)
    }.

to_claim_item({Props}) ->  to_claim_item(Props);
to_claim_item(Props) -> 
  DT = decode:xsd_info(<<"Claim.Item">>),
  #'Claim.Item'{ 
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
    , detail  = decode:value(<<"detail">>, Props, DT)
    }.


to_claim_accident({Props}) ->  to_claim_accident(Props);
to_claim_accident(Props) -> 
  DT = decode:xsd_info(<<"Claim.Accident">>),
  #'Claim.Accident'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.

to_claim_insurance({Props}) ->  to_claim_insurance(Props);
to_claim_insurance(Props) -> 
  DT = decode:xsd_info(<<"Claim.Insurance">>),
  #'Claim.Insurance'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , sequence  = decode:value(<<"sequence">>, Props, DT)
    , focal  = decode:value(<<"focal">>, Props, DT)
    , identifier  = decode:value(<<"identifier">>, Props, DT)
    , coverage  = decode:value(<<"coverage">>, Props, DT)
    , businessArrangement  = decode:value(<<"businessArrangement">>, Props, DT)
    , preAuthRef  = decode:value(<<"preAuthRef">>, Props, DT)
    , claimResponse  = decode:value(<<"claimResponse">>, Props, DT)
    }.



to_claim_procedure({Props}) ->  to_claim_procedure(Props);
to_claim_procedure(Props) -> 
  DT = decode:xsd_info(<<"Claim.Procedure">>),
  #'Claim.Procedure'{ 
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


to_claim_diagnosis({Props}) ->  to_claim_diagnosis(Props);
to_claim_diagnosis(Props) -> 
  DT = decode:xsd_info(<<"Claim.Diagnosis">>),
  #'Claim.Diagnosis'{ 
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


to_claim_supportingInfo({Props}) ->  to_claim_supportingInfo(Props);
to_claim_supportingInfo(Props) -> 
  DT = decode:xsd_info(<<"Claim.SupportingInfo">>),
  #'Claim.SupportingInfo'{ 
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



to_claim_careTeam({Props}) ->  to_claim_careTeam({Props});
to_claim_careTeam(Props) ->  
  DT = decode:xsd_info(<<"Claim.CareTeam">>),
  #'Claim.CareTeam'{ 
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



to_claim_payee({Props}) ->  to_claim_payee(Props);
to_claim_payee(Props) -> 
  DT = decode:xsd_info(<<"Claim.Payee">>),
  #'Claim.Payee'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , party  = decode:value(<<"party">>, Props, DT)
    }.


to_claim_related({Props}) ->  to_claim_related(Props);
to_claim_related(Props) -> 
  DT = decode:xsd_info(<<"Claim.Related">>),
  #'Claim.Related'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , claim  = decode:value(<<"claim">>, Props, DT)
    , relationship  = decode:value(<<"relationship">>, Props, DT)
    , reference  = decode:value(<<"reference">>, Props, DT)
    }.



text(#'Claim'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, claim:to_claim(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

claim_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Claim',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
claim_toprop_test() ->
    ?asrtp({'Claim',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Claim">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

claim_json_test() ->
    ?asrtjson({'Claim',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Claim\",\"id\":\"p-21666\"}">>).

-endif.



