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
	choice :: special:'Reference'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'() | boolean(),
	exclude :: boolean(),
	period :: complex:'Period'() | undefined}).

-type 'Group.Characteristic'() :: #'Group.Characteristic'{}.


-record('Group', {anyAttribs :: anyAttribs(),
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
	type :: complex:'GroupType'(),
	actual :: boolean(),
	code :: complex:'CodeableConcept'() | undefined,
	name :: string() | undefined,
	quantity :: unsignedInt() | undefined,
	managingEntity :: special:'Reference'() | undefined,
	characteristic :: [complex:'Group.Characteristic'()] | undefined,
	member :: [complex:'Group.Member'()] | undefined}).

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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	active :: boolean() | undefined,
	type :: complex:'GroupType'(),
	actual :: boolean(),
	code :: complex:'CodeableConcept'() | undefined,
	name :: string() | undefined,
	quantity :: unsignedInt() | undefined,
	managingEntity :: special:'Reference'() | undefined,
	characteristic :: [complex:'Group.Characteristic'()] | undefined,
	member :: [complex:'Group.Member'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_group.Member({Props}) -> to_group.Member(Props);
to_group.Member(Props) ->
  DT = decode:xsd_info(<<"Group.Member">>),
  #'Group.Member'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	entity :: special:'Reference'(),
	period :: complex:'Period'() | undefined,
	inactive :: boolean() | undefined}).
    }.


to_group.Characteristic({Props}) -> to_group.Characteristic(Props);
to_group.Characteristic(Props) ->
  DT = decode:xsd_info(<<"Group.Characteristics">>),
  #'Group:Characteristics'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: complex:'CodeableConcept'(),
	choice :: special:'Reference'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'() | boolean(),
	exclude :: boolean(),
	period :: complex:'Period'() | undefined}).
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
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Group',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
group_toprop_test() ->
    ?asrtp({'Group',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Group">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

group_json_test() ->
    ?asrtjson({'Group',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Group\",\"id\":\"p-21666\"}">>).

-endif.



