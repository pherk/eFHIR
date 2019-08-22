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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
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
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_claim.SubDetail({Props}) ->  to_claim.SubDetail(Props);
to_claim.SubDetail(Props) -> 
  DT = decode:xsd_info(<<"Claim.SubDetail">>),
  #'Claim.SubDetail'{ 
    anyAttribs :: anyAttribs(),
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
    }.



to_claim.Detail({Props}) ->  to_claim.Detail(Props);
to_claim.Detail(Props) -> 
  DT = decode:xsd_info(<<"Claim.Detail">>),
  #'Claim.Detail'{ 
    anyAttribs :: anyAttribs(),
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
    }.

to_claim.Item({Props}) ->  to_claim.Item(Props);
to_claim.Item(Props) -> 
  DT = decode:xsd_info(<<"Claim.Item">>),
  #'Claim.Item'{ 
    anyAttribs :: anyAttribs(),
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
    }.


to_claim.Accident({Props}) ->  to_claim.Accident(Props);
to_claim.Accident(Props) -> 
  DT = decode:xsd_info(<<"Claim.Accident">>),
  #'Claim.Accident'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	date :: date(),
	type :: complex:'CodeableConcept'() | undefined,
	choice :: special:'Reference'() | complex:'Address'() | undefined}).
    }.

to_claim.Insurance({Props}) ->  to_claim.Insurance(Props);
to_claim.Insurance(Props) -> 
  DT = decode:xsd_info(<<"Claim.Insurance">>),
  #'Claim.Insurance'{ 
    anyAttribs :: anyAttribs(),
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
    }.



to_claim.Procedure({Props}) ->  to_claim.Procedure(Props);
to_claim.Procedure(Props) -> 
  DT = decode:xsd_info(<<"Claim.Procedure">>),
  #'Claim.Procedure'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	type :: [complex:'CodeableConcept'()] | undefined,
	date :: dateTime() | undefined,
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	udi :: [special:'Reference'()] | undefined}).


to_claim.Diagnosis({Props}) ->  to_claim.Diagnosis(Props);
to_claim.Diagnosis(Props) -> 
  DT = decode:xsd_info(<<"Claim.Procedure">>),
  #'Claim.Procedure'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	choice :: special:'Reference'() | complex:'CodeableConcept'(),
	type :: [complex:'CodeableConcept'()] | undefined,
	onAdmission :: complex:'CodeableConcept'() | undefined,
	packageCode :: complex:'CodeableConcept'() | undefined}).
    }.


to_claim.SupportingInfo({Props}) ->  to_claim.SupportingInfo(Props);
to_claim.SupportingInfo(Props) -> 
  DT = decode:xsd_info(<<"Claim.SupportingInfo">>),
  #'Claim.SupportingInfo'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	category :: complex:'CodeableConcept'(),
	code :: complex:'CodeableConcept'() | undefined,
	choice :: complex:'Period'() | date() | undefined,
	choice1 :: string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | boolean() | complex:'Attachment'() | undefined,
	reason :: complex:'CodeableConcept'() | undefined}).
    }.



to_claim.CareTeam({Props}) ->  to_claim.CareTeam({Props}) 
to_claim.CareTeam(Props) ->  
  DT = decode:xsd_info(<<"Claim.CareTeam">>),
  #'Claim.CareTeam'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	sequence :: positiveInt(),
	provider :: special:'Reference'(),
	responsible :: boolean() | undefined,
	role :: complex:'CodeableConcept'() | undefined,
	qualification :: complex:'CodeableConcept'() | undefined}).
    }.



to_claim.Payee({Props}) ->  to_claim.Payee(Props);
to_claim.Payee(Props) -> 
  DT = decode:xsd_info(<<"Claim.Payee">>),
  #'Claim.Payee'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	party :: special:'Reference'() | undefined}).


to_claim.Related({Props}) ->  to_claim.Related(Props);
to_claim.Related(Props) -> 
  DT = decode:xsd_info(<<"Claim.Related">>),
  #'Claim.Related'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	claim :: special:'Reference'() | undefined,
	relationship :: complex:'CodeableConcept'() | undefined,
	reference :: complex:'Identifier'() | undefined}).
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



