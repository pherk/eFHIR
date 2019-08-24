-module(practitioner).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Practitioner.Qualification', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	code :: complex:'CodeableConcept'(),
	period :: complex:'Period'() | undefined,
	issuer :: special:'Reference'() | undefined}).

-type 'Practitioner.Qualification'() :: #'Practitioner.Qualification'{}.


-record('Practitioner', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	active :: boolean() | undefined,
	name :: [complex:'HumanName'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: [complex:'Address'()] | undefined,
	gender :: complex:'AdministrativeGender'() | undefined,
	birthDate :: date() | undefined,
	photo :: [complex:'Attachment'()] | undefined,
	qualification :: [complex:'Practitioner.Qualification'()] | undefined,
	communication :: [complex:'CodeableConcept'()] | undefined}).

-type 'Practitioner'() :: #'Practitioner'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_practitioner({Props}) -> to_practitioner(Props);
to_practitioner(Props) ->
  DT = decode:xsd_info(<<"Practitioner">>),
  #'Practitioner'{ 
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
    , active  = decode:value(<<"active">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , telecom  = decode:value(<<"telecom">>, Props, DT)
    , address  = decode:value(<<"address">>, Props, DT)
    , gender  = decode:value(<<"gender">>, Props, DT)
    , birthDate  = decode:value(<<"birthDate">>, Props, DT)
    , photo  = decode:value(<<"photo">>, Props, DT)
    , qualification  = decode:value(<<"qualification">>, Props, DT)
    , communication  = decode:value(<<"communication">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_practitioner_qualification({Props}) -> to_practitioner_qualification(Props);
to_practitioner_qualification(Props) ->
  DT = decode:xsd_info(<<"Practitioner.Qulification">>),
  #'Practitioner.Qualification'{
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier  = decode:value(<<"identifier">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , issuer  = decode:value(<<"issuer">>, Props, DT)
   }.

text(#'Practitioner'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, practitioner:to_practitioner(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

practitioner_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
            {'Practitioner',[],<<"p-21666">>,undefined,undefined,
                                 undefined,undefined,[],[],[],[],undefined,[],
                                 [],[],undefined,undefined,[],[],[]}).
practitioner_toprop_test() ->
    ?asrtp(
           {'Practitioner',[],<<"p-21666">>,undefined,undefined,
                                 undefined,undefined,[],[],[],[],undefined,[],
                                 [],[],undefined,undefined,[],[],[]},
           {[{<<"resourceType">>,<<"Practitioner">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

practitioner_json_test() ->
    ?asrtjson(
           {'Practitioner',[],<<"p-21666">>,undefined,undefined,
                                 undefined,undefined,[],[],[],[],undefined,[],
                                 [],[],undefined,undefined,[],[],[]},
           <<"{\"resourceType\":\"Practitioner\",\"id\":\"p-21666\"}">>).

-endif.



