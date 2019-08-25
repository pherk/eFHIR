-module(coverage).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('Coverage.Exception', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	period :: complex:'Period'() | undefined}).

-type 'Coverage.Exception'() :: #'Coverage.Exception'{}.


-record('Coverage.CostToBeneficiary', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	value :: complex:'Quantity'() | complex:'Money'(),
	exception :: ['Coverage.Exception'()] | undefined}).

-type 'Coverage.CostToBeneficiary'() :: #'Coverage.CostToBeneficiary'{}.


-record('Coverage.Class', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	value :: string(),
	name :: string() | undefined}).

-type 'Coverage.Class'() :: #'Coverage.Class'{}.


-record('Coverage', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: code(),
	type :: complex:'CodeableConcept'() | undefined,
	policyHolder :: special:'Reference'() | undefined,
	subscriber :: special:'Reference'() | undefined,
	subscriberId :: string() | undefined,
	beneficiary :: special:'Reference'(),
	dependent :: string() | undefined,
	relationship :: complex:'CodeableConcept'() | undefined,
	period :: complex:'Period'() | undefined,
	payor :: [special:'Reference'()],
	class :: ['Coverage.Class'()] | undefined,
	order :: positiveInt() | undefined,
	network :: string() | undefined,
	costToBeneficiary :: ['Coverage.CostToBeneficiary'()] | undefined,
	subrogation :: boolean() | undefined,
	contract :: [special:'Reference'()] | undefined}).

-type 'Coverage'() :: #'Coverage'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_coverage({Props}) -> to_coverage(Props);
to_coverage(Props) ->
  DT = decode:xsd_info(<<"Coverage">>),
  #'Coverage'{ 
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
    , policyHolder  = decode:value(<<"policyHolder">>, Props, DT)
    , subscriber  = decode:value(<<"subscriber">>, Props, DT)
    , subscriberId  = decode:value(<<"subscriberId">>, Props, DT)
    , beneficiary  = decode:value(<<"beneficiary">>, Props, DT)
    , dependent  = decode:value(<<"dependent">>, Props, DT)
    , relationship  = decode:value(<<"relationship">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , payor  = decode:value(<<"payor">>, Props, DT)
    , class  = decode:value(<<"class">>, Props, DT)
    , order  = decode:value(<<"order">>, Props, DT)
    , network  = decode:value(<<"network">>, Props, DT)
    , costToBeneficiary  = decode:value(<<"costToBeneficiary">>, Props, DT)
    , subrogation  = decode:value(<<"subrogation">>, Props, DT)
    , contract  = decode:value(<<"contract">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_coverage_exception({Props}) ->  to_coverage_exception(Props);
to_coverage_exception(Props) -> 
  DT = decode:xsd_info(<<"Coverage.Exception">>),
  #'Coverage.Exception'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    }.


to_coverage_costToBeneficiary({Props}) ->  to_coverage_costToBeneficiary(Props);
to_coverage_costToBeneficiary(Props) -> 
  DT = decode:xsd_info(<<"Coverage.CostToBeneficiary">>),
  #'Coverage.CostToBeneficiary'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    , exception  = decode:value(<<"exception">>, Props, DT)
    }.



to_coverage_class({Props}) ->  to_coverage_class(Props);
to_coverage_class(Props) -> 
  DT = decode:xsd_info(<<"Coverage.Class">>),
  #'Coverage.Class'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    }.


text(#'Coverage'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, coverage:to_coverage(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

coverage_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"status">>, <<"active">>},
             {<<"beneficiary">>, {[{<<"reference">>, <<"nabu/Patient/p-21666">>}]}},
             {<<"payor">>, [{[{<<"reference">>, <<"nabu/Patient/p-21666">>}]}]}
            ],
            {'Coverage',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,undefined,undefined, undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,
             [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
             [],undefined,undefined,[],undefined,[]}
           ).

coverage_toprop_test() ->
    ?asrtp(
            {'Coverage',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,undefined,undefined, undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,
             [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
             [],undefined,undefined,[],undefined,[]},
           {[{<<"resourceType">>,<<"Coverage">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"active">>},
              {<<"beneficiary">>,
                    {[{<<"reference">>,<<"nabu/Patient/p-21666">>}]}},
              {<<"payor">>,
                    [{[{<<"reference">>,<<"nabu/Patient/p-21666">>}]}]}]}
            ).

coverage_json_test() ->
    ?asrtjson(
            {'Coverage',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,undefined,undefined, undefined,undefined,
             {'Reference',[],undefined,[],<<"nabu/Patient/p-21666">>, undefined,undefined,undefined},
             undefined,undefined,undefined,
             [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
             [],undefined,undefined,[],undefined,[]},
            <<"{\"resourceType\":\"Coverage\",\"id\":\"p-21666\",\"status\":\"active\",\"beneficiary\":{\"reference\":\"nabu/Patient/p-21666\"},\"payor\":[{\"reference\":\"nabu/Patient/p-21666\"}]}">>
      ).

-endif.



