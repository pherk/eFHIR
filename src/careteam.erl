-module(careteam).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('CareTeam.Participant', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	role :: [complex:'CodeableConcept'()] | undefined,
	member :: special:'Reference'() | undefined,
	onBehalfOf :: special:'Reference'() | undefined,
	period :: complex:'Period'() | undefined}).

-type 'CareTeam.Participant'() :: #'CareTeam.Participant'{}.

-record('CareTeam', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'CareTeamStatus'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	name :: string() | undefined,
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	period :: complex:'Period'() | undefined,
	participant :: [complex:'CareTeam.Participant'()] | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	managingOrganization :: [special:'Reference'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).

-type 'CareTeam'() :: #'CareTeam'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_careTeam({Props}) -> to_careTeam(Props);
to_careTeam(Props) ->
  DT = decode:xsd_info(<<"CareTeam">>),
  #'CareTeam'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'CareTeamStatus'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	name :: string() | undefined,
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	period :: complex:'Period'() | undefined,
	participant :: [complex:'CareTeam.Participant'()] | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	managingOrganization :: [special:'Reference'()] | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_careTeam_participant({Props}) -> to_careTeam_participant(Props);
to_careTeam_participant(Props) ->
  DT = decode:xsd_info(<<"CareTeam.Participant">>),
  #'CareTeam.Participant'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	role :: [complex:'CodeableConcept'()] | undefined,
	member :: special:'Reference'() | undefined,
	onBehalfOf :: special:'Reference'() | undefined,
	period :: complex:'Period'() | undefined}).
    }.


text(#'CareTeam'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, careTeam:to_careTeam(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

careTeam_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'CareTeam',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
careTeam_toprop_test() ->
    ?asrtp({'CareTeam',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"CareTeam">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

careTeam_json_test() ->
    ?asrtjson({'CareTeam',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"CareTeam\",\"id\":\"p-21666\"}">>).

-endif.


