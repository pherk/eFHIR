-module(extract).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

%%
%% API exports
%%
-export([extract/2]).

%%======================================================================================================
%% API functions
%%======================================================================================================

extract(Path, Resource) ->
  io:format("extract ~p:~p~n",[Resource, Path]),
  [Parent|Steps] = update:split_path(Path),
  step(Steps, Resource ).

step([], Resource) ->
  io:format("step last ~p~n",[Resource]),
  Resource;
step([{Node, Index}| Tail], Resources) when is_list(Resources) ->
  io:format("step indexed list: ~p:~p-~p~n",[Node, Index, Tail]),
  Resource = match(Index, Resources),
  step(Tail, Resource);
%step(_Steps, Resource) when is_binary(Resource) ->
%  io:format("step bin ~p~n",[Resource]),
%  Resource;
step([{Node, Index}| Tail], Resource) when is_tuple(Resource) ->
  io:format("step tuple-list: ~p-~p~n~p~n",[Node, Tail, Resource]),
  {RI, XSD} = analyze(Resource),
  Field = decode:base_name(Node, XSD),
  PropInfo = decode:prop_info(Field, XSD),
  I = index_of(Field, RI) + 1,
  V = element(I, Resource),
  Child = extract_field(Node, Field, V, PropInfo),
  step(Tail, match(Index, Child));
step([Node|Tail], Resource) when is_tuple(Resource) ->
  io:format("step tuple: ~p-~p~n~p~n",[Node, Tail, Resource]),
  {RI, XSD} = analyze(Resource),
  Field = decode:base_name(Node, XSD),
  PropInfo = decode:prop_info(Field, XSD),
  I = index_of(Field, RI) + 1,
  V = element(I, Resource),
  Child = extract_field(Node, Field, V, PropInfo),
  step(Tail, Child).

%%
analyze(Resource) ->
  RT = erlang:element(1,Resource),
  RI = [atom_to_binary(F,latin1) || F <- resource:fields(RT)],
  XSD = decode:xsd_info(atom_to_binary(RT,latin1)),
  io:format("extract type: ~p~n",[RT]),
  %  io:format("extract Fields: ~p~n",[RI]),
  %  io:format("extract XSD: ~p~n",[XSD]),
  {RI, XSD}.

match(Index, Rs)  when is_integer(Index) ->
  lists:nth(Index + 1, Rs);
%% TODO match binary
match(Index, Rs)  when is_binary(Index) ->
  R = lists:nth(1, Rs),
  case erlang:element(1,R) of
      'Extension' -> lists:keyfind(Index, 5, Rs);  % url
      'Coding'    -> lists:keyfind(Index, 5, Rs);  % system
      _           -> throw("extract: match with binary only for Extension, Coding")
  end.
%%
%%
%% normal field
extract_field(Field, Field, V, PropInfo) ->
  io:format("extract_field: ~p:~p~n",[Field, PropInfo]),
  io:format("extract_field: ~p~n",[V]),
  V;
%% choice field
extract_field(PathEl, Field, V, PropInfo) ->
  Info = proplists:get_value(PathEl, PropInfo),
  io:format("extract_field choice: ~p:~p~n",[Field, Info]),
  io:format("extract_field choice: ~p~n",[V]),
  element(2,V).

%% same in fhir update.erl
-spec index_of(any(), list(any())) -> not_found | non_neg_integer().
index_of(Item, List) -> index_of(Item, List, 1).

-spec index_of(any(), list(any()), non_neg_integer()) -> not_found | non_neg_integer().
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%%
%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrte(P, A, B), ?assertEqual(B, extract(P, A))).

pc_minimal() ->
        {'Patient.Contact',[],undefined,
                    [{'Extension',[],undefined,[],<<"post">>, {valueBoolean,false}},
                     {'Extension',[],undefined,[],<<"period">>, {valuePeriod,{'Period', [], undefined, [], <<"2019-01-01">>, <<"2020-01-01">>}}}],
                    [],[],
                    {'HumanName',[],undefined,[], <<"official">>,undefined,[<<"Dummy">>],[<<"Detlef">>], [],[],undefined},
                    [{'ContactPoint',[],undefined,[],undefined,undefined,<<"0221-478 5900">>,undefined,undefined},
                     {'ContactPoint',[],undefined,[],undefined,undefined,<<"0221-478 6580">>,undefined,undefined}],
                    undefined,<<"male">>,undefined,undefined}.

extract_binary_prop_test() ->
    ?asrte('use', <<"official">>, <<"official">>).

extract_single_step_test() ->
    ?asrte('contact-gender', pc_minimal(), <<"male">>),
    ?asrte('contact-name', pc_minimal(), 
           {'HumanName',[],undefined,[],<<"official">>,undefined,[<<"Dummy">>],[<<"Detlef">>],[],[],undefined}),
    ?asrte('contact-telecom:0', pc_minimal(), 
           {'ContactPoint',[],undefined,[],undefined,undefined,
                                 <<"0221-478 5900">>,undefined,undefined}),
    ?asrte('contact-telecom:1', pc_minimal(), 
           {'ContactPoint',[],undefined,[],undefined,undefined,
                                 <<"0221-478 6580">>,undefined,undefined}).

extract_steps_test() ->
    ?asrte('contact-name-family', pc_minimal(), [<<"Dummy">>]),
    ?asrte('contact-name-family:0', pc_minimal(), <<"Dummy">>),
    ?asrte('contact-telecom:0-value', pc_minimal(), <<"0221-478 5900">>),
    ?asrte('contact-telecom:1-value', pc_minimal(), <<"0221-478 6580">>).

extract_ext_test() ->
    ?asrte('contact-extension:0-valueBoolean', pc_minimal(), false),
    ?asrte('contact-extension:1-valuePeriod-start', pc_minimal(), <<"2019-01-01">>),
    ?asrte('contact-extension:post-valueBoolean', pc_minimal(), false),
    ?asrte('contact-extension:period-valuePeriod-start', pc_minimal(), <<"2019-01-01">>).

extract_root_steps_test() ->
    ?asrte('contact:0-name-family', pc_minimal(), [<<"Dummy">>]).



-endif.
