-module(organization).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Organization.Contact', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	purpose :: complex:'CodeableConcept'() | undefined,
	name :: complex:'HumanName'() | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: complex:'Address'() | undefined}).

-type 'Organization.Contact'() :: #'Organization.Contact'{}.


-record('Organization', {anyAttribs :: anyAttribs(),
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
	type :: [complex:'CodeableConcept'()] | undefined,
	name :: string() | undefined,
	alias :: [string()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	address :: [complex:'Address'()] | undefined,
	partOf :: special:'Reference'() | undefined,
	contact :: [complex:'Organization.Contact'()] | undefined,
	endpoint :: [special:'Reference'()] | undefined}).

-type 'Organization'() :: #'Organization'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_organization({Props}) -> to_organization(Props);
to_organization(Props) ->
  DT = decode:xsd_info(<<"Organization">>),
  #'Organization'{ 
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
    , name  = decode:value(<<"name">>, Props, DT)
    , alias  = decode:value(<<"alias">>, Props, DT)
    , telecom  = decode:value(<<"telecom">>, Props, DT)
    , address  = decode:value(<<"address">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , contact  = decode:value(<<"contact">>, Props, DT)
    , endpoint  = decode:value(<<"endpoint">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_organization.Contact([Props}) -> to_organization.Contact(Props);
to_organization.Contact(Props) ->
  DT = decode:xsd_info(<<"Organization.Contact">>),
  #'Organization.Contact'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , purpose  = decode:value(<<"purpose">>, Props, DT)
    , name  = decode:value(<<"name">>, Props, DT)
    , telecom  = decode:value(<<"telecom">>, Props, DT)
    , address  = decode:value(<<"address">>, Props, DT)
    }.

text(#'Organization'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, organization:to_organization(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

organization_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Organization',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
organization_toprop_test() ->
    ?asrtp({'Organization',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Organization">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

organization_json_test() ->
    ?asrtjson({'Organization',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Organization\",\"id\":\"p-21666\"}">>).

-endif.


