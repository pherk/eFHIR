-module(group).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Group.Member', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	entity :: special:'Reference'(),
	period :: complex:'Period'() | undefined,
	inactive :: boolean() | undefined}).

-type 'Group.Member'() :: #'Group.Member'{}.


-record('Group.Characteristic', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: complex:'CodeableConcept'(),
	value :: special:'Reference'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'() | boolean(),
	exclude :: boolean(),
	period :: complex:'Period'() | undefined}).

-type 'Group.Characteristic'() :: #'Group.Characteristic'{}.


-record('Group', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	active :: boolean() | undefined,
	type :: code(),
	actual :: boolean(),
	code :: complex:'CodeableConcept'() | undefined,
	name :: string() | undefined,
	quantity :: unsignedInt() | undefined,
	managingEntity :: special:'Reference'() | undefined,
	characteristic :: ['Group.Characteristic'()] | undefined,
	member :: ['Group.Member'()] | undefined}).

-type 'Group'() :: #'Group'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_group({Props}) -> to_group(Props);
to_group(Props) ->
  DT = decode:xsd_info(<<"Group">>),
  #'Group'{ 
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
    , type  = decode:value(<<"type">>, Props, DT)
    , actual  = decode:value(<<"actual">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , quantity  = decode:value(<<"quantity">>, Props, DT)
    , managingEntity  = decode:value(<<"managingEntity">>, Props, DT)
    , characteristic  = decode:value(<<"characteristic">>, Props, DT)
    , member  = decode:value(<<"member">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_group_member({Props}) -> to_group_member(Props);
to_group_member(Props) ->
  DT = decode:xsd_info(<<"Group.Member">>),
  #'Group.Member'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , entity  = decode:value(<<"entity">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , inactive  = decode:value(<<"inactive">>, Props, DT)
    }.


to_group_characteristic({Props}) -> to_group_characteristic(Props);
to_group_characteristic(Props) ->
  DT = decode:xsd_info(<<"Group.Characteristic">>),
  #'Group.Characteristic'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    , exclude  = decode:value(<<"exclude">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    }.



text(#'Group'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, group:to_group(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

group_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"type">>, <<"practitioner">>},
             {<<"actual">>, true}
            ],
            {'Group',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,<<"practitioner">>, true,undefined,undefined,undefined,undefined,[],[]}
           ).

group_toprop_test() ->
    ?asrtp(
            {'Group',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,<<"practitioner">>, true,undefined,undefined,undefined,undefined,[],[]},
            {[{<<"resourceType">>,<<"Group">>},
                   {<<"id">>,<<"p-21666">>},
                   {<<"type">>,<<"practitioner">>},
                   {<<"actual">>,true}]}
            ).

group_json_test() ->
    ?asrtjson(
            {'Group',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],undefined,<<"practitioner">>, true,undefined,undefined,undefined,undefined,[],[]},
           <<"{\"resourceType\":\"Group\",\"id\":\"p-21666\",\"type\":\"practitioner\",\"actual\":true}">>
      ). 

-endif.



