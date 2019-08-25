-module(composition).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Composition.Section', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	title :: string() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	author :: [special:'Reference'()] | undefined,
	focus :: special:'Reference'() | undefined,
	text :: special:'Narrative'() | undefined,
	mode :: complex:'ListMode'() | undefined,
	orderedBy :: complex:'CodeableConcept'() | undefined,
	entry :: [special:'Reference'()] | undefined,
	emptyReason :: complex:'CodeableConcept'() | undefined,
	section :: ['Composition.Section'()] | undefined}).

-type 'Composition.Section'() :: #'Composition.Section'{}.


-record('Composition.Event', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	period :: complex:'Period'() | undefined,
	detail :: [special:'Reference'()] | undefined}).

-type 'Composition.Event'() :: #'Composition.Event'{}.


-record('Composition.RelatesTo', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: code(),
	target :: special:'Reference'() | complex:'Identifier'()}).

-type 'Composition.RelatesTo'() :: #'Composition.RelatesTo'{}.


-record('Composition.Attester', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	mode :: code(),
	time :: dateTime() | undefined,
	party :: special:'Reference'() | undefined}).

-type 'Composition.Attester'() :: #'Composition.Attester'{}.


-record('Composition', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: complex:'Identifier'() | undefined,
	status :: code(),
	type :: complex:'CodeableConcept'(),
	category :: [complex:'CodeableConcept'()] | undefined,
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	date :: dateTime(),
	author :: [special:'Reference'()],
	title :: string(),
	confidentiality :: code() | undefined,
	attester :: ['Composition.Attester'()] | undefined,
	custodian :: special:'Reference'() | undefined,
	relatesTo :: ['Composition.RelatesTo'()] | undefined,
	event :: ['Composition.Event'()] | undefined,
	section :: ['Composition.Section'()] | undefined}).

-type 'Composition'() :: #'Composition'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_composition({Props}) -> to_composition(Props);
to_composition(Props) ->
  DT = decode:xsd_info(<<"Composition">>),
  #'Composition'{ 
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
    , category  = decode:value(<<"category">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , date  = decode:value(<<"date">>, Props, DT)
    , author  = decode:value(<<"author">>, Props, DT)
    , title  = decode:value(<<"title">>, Props, DT)
    , confidentiality  = decode:value(<<"confidentiality">>, Props, DT)
    , attester  = decode:value(<<"attester">>, Props, DT)
    , custodian  = decode:value(<<"custodian">>, Props, DT)
    , relatesTo  = decode:value(<<"relatesTo">>, Props, DT)
    , event  = decode:value(<<"event">>, Props, DT)
    , section  = decode:value(<<"section">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_composition_section({Props}) ->  to_composition_section(Props);
to_composition_section(Props) -> 
  DT = decode:xsd_info(<<"Composition.Section">>),
  #'Composition.Section'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , title  = decode:value(<<"title">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , author  = decode:value(<<"author">>, Props, DT)
    , focus  = decode:value(<<"focus">>, Props, DT)
    , text  = decode:value(<<"text">>, Props, DT)
    , mode  = decode:value(<<"mode">>, Props, DT)
    , orderedBy  = decode:value(<<"orderedBy">>, Props, DT)
    , entry  = decode:value(<<"entry">>, Props, DT)
    , emptyReason  = decode:value(<<"emptyReason">>, Props, DT)
    , section  = decode:value(<<"section">>, Props, DT)
    }.


to_composition_event({Props}) ->  to_composition_event(Props);
to_composition_event(Props) -> 
  DT = decode:xsd_info(<<"Composition.Event">>),
  #'Composition.Event'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , detail  = decode:value(<<"detail">>, Props, DT)
    }.


to_composition_relatesTo({Props}) ->  to_composition_relatesTo(Props);
to_composition_relatesTo(Props) -> 
  DT = decode:xsd_info(<<"Composition.RelatesTo">>),
  #'Composition.RelatesTo'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , target  = decode:value(<<"target">>, Props, DT)
    }.


to_composition_attester({Props}) ->  to_composition_attester(Props);
to_composition_attester(Props) -> 
  DT = decode:xsd_info(<<"Composition.Attester">>),
  #'Composition.Attester'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , mode  = decode:value(<<"mode">>, Props, DT)
    , time  = decode:value(<<"time">>, Props, DT)
    , party  = decode:value(<<"party">>, Props, DT)
    }.



text(#'Composition'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, composition:to_composition(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

composition_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"status">>, <<"final">>},
             {<<"type">>, {[{<<"coding">>, [{[{<<"code">>, <<"11503-0">>}]}]}]}},
             {<<"date">>, <<"2019-01-01T12:00:00">>},
             {<<"author">>, [{[{<<"reference">>, <<"nabu/Practitioner/u-admin">>}]}]},
             {<<"title">>, <<"Composition Test">>}
            ],
            {'Composition',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             undefined,<<"final">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"11503-0">>,undefined,undefined}],
                 undefined},
             [],undefined,undefined,<<"2019-01-01T12:00:00">>,
             [{'Reference',[],undefined,[], <<"nabu/Practitioner/u-admin">>,undefined,undefined, undefined}],
             <<"Composition Test">>,undefined,[],undefined,[],[],[]}
           ).

composition_toprop_test() ->
    ?asrtp(
            {'Composition',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             undefined,<<"final">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"11503-0">>,undefined,undefined}],
                 undefined},
             [],undefined,undefined,<<"2019-01-01T12:00:00">>,
             [{'Reference',[],undefined,[], <<"nabu/Practitioner/u-admin">>,undefined,undefined, undefined}],
             <<"Composition Test">>,undefined,[],undefined,[],[],[]},
            {[{<<"resourceType">>,<<"Composition">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"final">>},
              {<<"type">>,
                    {[{<<"coding">>,[{[{<<"code">>,<<"11503-0">>}]}]}]}},
              {<<"date">>,<<"2019-01-01T12:00:00">>},
              {<<"author">>,
                    [{[{<<"reference">>,<<"nabu/Practitioner/u-admin">>}]}]},
              {<<"title">>,<<"Composition Test">>}]}
            ).

composition_json_test() ->
    ?asrtjson(
            {'Composition',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             undefined,<<"final">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"11503-0">>,undefined,undefined}],
                 undefined},
             [],undefined,undefined,<<"2019-01-01T12:00:00">>,
             [{'Reference',[],undefined,[], <<"nabu/Practitioner/u-admin">>,undefined,undefined, undefined}],
             <<"Composition Test">>,undefined,[],undefined,[],[],[]},
            <<"{\"resourceType\":\"Composition\",\"id\":\"p-21666\",\"status\":\"final\",\"type\":{\"coding\":[{\"code\":\"11503-0\"}]},\"date\":\"2019-01-01T12:00:00\",\"author\":[{\"reference\":\"nabu/Practitioner/u-admin\"}],\"title\":\"Composition Test\"}">>
      ).

-endif.



