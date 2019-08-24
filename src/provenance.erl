-module(provenance).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Provenance.Entity', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	role :: complex:'ProvenanceEntityRole'(),
	what :: special:'Reference'(),
	agent :: [complex:'Provenance.Agent'()] | undefined}).

-type 'Provenance.Entity'() :: #'Provenance.Entity'{}.


-record('Provenance.Agent', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined,
	role :: [complex:'CodeableConcept'()] | undefined,
	who :: special:'Reference'(),
	onBehalfOf :: special:'Reference'() | undefined}).

-type 'Provenance.Agent'() :: #'Provenance.Agent'{}.


-record('Provenance', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	target :: [special:'Reference'()],
	occurred :: complex:'Period'() | dateTime() | undefined,
	recorded :: instant(),
	policy :: [uri()] | undefined,
	location :: special:'Reference'() | undefined,
	reason :: [complex:'CodeableConcept'()] | undefined,
	activity :: complex:'CodeableConcept'() | undefined,
	agent :: [complex:'Provenance.Agent'()],
	entity :: [complex:'Provenance.Entity'()] | undefined,
	signature :: [complex:'Signature'()] | undefined}).

-type 'Provenance'() :: #'Provenance'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_provenance({Props}) -> to_provenance(Props);
to_provenance(Props) ->
  DT = decode:xsd_info(<<"Provenance">>),
  #'Provenance'{ 
      anyAttribs       = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , target  = decode:value(<<"target">>, Props, DT)
    , occurred  = decode:value(<<"occurred">>, Props, DT)
    , recorded  = decode:value(<<"recorded">>, Props, DT)
    , policy  = decode:value(<<"policy">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , reason  = decode:value(<<"reason">>, Props, DT)
    , activity  = decode:value(<<"activity">>, Props, DT)
    , agent  = decode:value(<<"agent">>, Props, DT)
    , entity  = decode:value(<<"entity">>, Props, DT)
    , signature  = decode:value(<<"signature">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_provenance_entity({Props}) -> to_provenance_entity(Props);
to_provenance_entity(Props) ->
  DT = decode:xsd_info(<<"Provenance.Entity">>),
  #'Provenance.Entity'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , role  = decode:value(<<"role">>, Props, DT)
    , what  = decode:value(<<"what">>, Props, DT)
    , agent  = decode:value(<<"agent">>, Props, DT)
    }.


to_provenance_agent({Props}) -> to_provenance_agent(Props);
to_provenance_agent(Props) ->
  DT = decode:xsd_info(<<"Provenance.Agent">>),
  io:format("to_pa: ~p~n~p~n",[Props,DT]),
  #'Provenance.Agent'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , role  = decode:value(<<"role">>, Props, DT)
    , who  = decode:value(<<"who">>, Props, DT)
    , onBehalfOf  = decode:value(<<"onBehalfOf">>, Props, DT)
    }.


text(#'Provenance'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, provenance:to_provenance(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

provenance_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"target">>, [{[{<<"reference">>,<<"nabu/Patient/p-21666">>}]}]},
             {<<"recorded">>, <<"2019-01-01T12:00:00">>},
             {<<"agent">>, [{[{<<"who">>, {[{<<"reference">>,<<"metis/Pratitioner/u-admin">>}]}}]}]}],
            {'Provenance',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                     [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
                     undefined,<<"2019-01-01T12:00:00">>,[],undefined,[], undefined,
                     [{'Provenance.Agent',[],undefined,[],[],undefined,[],
                          {'Reference',[],undefined,[], <<"metis/Pratitioner/u-admin">>,undefined, undefined,undefined},
                          undefined}],
                     [],[]}).

provenance_toprop_test() ->
    ?asrtp(
            {'Provenance',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                     [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
                     undefined,<<"2019-01-01T12:00:00">>,[],undefined,[], undefined,
                     [{'Provenance.Agent',[],undefined,[],[],undefined,[],
                          {'Reference',[],undefined,[], <<"metis/Pratitioner/u-admin">>,undefined, undefined,undefined},
                          undefined}],
                     [],[]},
           {[{<<"resourceType">>,<<"Provenance">>},
             {<<"id">>,<<"p-21666">>},
             {<<"target">>, [{[{<<"reference">>,<<"nabu/Patient/p-21666">>}]}]},
             {<<"recorded">>, <<"2019-01-01T12:00:00">>},
             {<<"agent">>, [{[{<<"who">>, {[{<<"reference">>,<<"metis/Pratitioner/u-admin">>}]}}]}]}
            ]}).

provenance_json_test() ->
    ?asrtjson(
            {'Provenance',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
                     [{'Reference',[],undefined,[], <<"nabu/Patient/p-21666">>,undefined,undefined, undefined}],
                     undefined,<<"2019-01-01T12:00:00">>,[],undefined,[], undefined,
                     [{'Provenance.Agent',[],undefined,[],[],undefined,[],
                          {'Reference',[],undefined,[], <<"metis/Pratitioner/u-admin">>,undefined, undefined,undefined}
                          , undefined}],
                     [],[]},
            <<"{\"resourceType\":\"Provenance\",\"id\":\"p-21666\",\"target\":[{\"reference\":\"nabu/Patient/p-21666\"}],\"recorded\":\"2019-01-01T12:00:00\",\"agent\":[{\"who\":{\"reference\":\"metis/Pratitioner/u-admin\"}}]}">>
           ).

-endif.


