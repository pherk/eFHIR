-module(update).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

%%
%% API exports
%%
-export([update/2, update/3]).

%%======================================================================================================
%% API functions
%%======================================================================================================

update(Resource, Props) -> update(Resource, Props, [replace]).

update(Resource, Props, Opts) ->
  GP = canonize(Props), 
  % io:format("update: ~p~n", [GP]),
  update2(Resource, GP, Opts).

%% TODO
%% manipulating tuple as list
%% lists:zip(record_info(fields, foobar), tl(tuple_to_list(Rec)));
update2(Resource, Props, Opts) ->
  RT = erlang:element(1,Resource),
  RI = [atom_to_binary(F,latin1) || F <- patient:fields(RT)],
  XSD = decode:xsd_info(atom_to_binary(RT,latin1)),
  Keys = maps:keys(Props),
%  io:format("update2: keys: ~p~n~p~n", [Keys, RI]),
  lists:foldl(fun(K, R) -> update_prop(R, {K, maps:get(K, Props)}, RI, XSD, Opts) end, Resource, Keys).

update_prop(R, KV, RI, XSD, [update]) ->
   io:format("upd: ~p~n", [R]),
   io:format("upd: ~p~n", [KV]),
  {I, P} = transform_prop(KV, RI, XSD),
  % io:format("upd: ~p:~p~n", [I, P]),
  setelement(I,R,P);
update_prop(R, KV, RI, XSD, [replace]) ->
  % io:format("upd: ~p~n", [R]),
  % io:format("upd: ~p~n", [KV]),
  {I, P} = transform_prop(KV, RI, XSD),
  % io:format("upd: ~p:~p~n", [I, P]),
  setelement(I,R,P).

transform_prop({K, V}=KV, RI, XSD) when is_binary(V) ->
    Field = decode:base_name(K,XSD),
    I = index_of(Field, RI) + 1,
  % io:format("t: ~p:~p:~p~n", [I, K, Field]),
%    PropInfo = decode:prop_info(Field, XSD),
%  io:format("t: ~p:~p~n", [K, PropInfo]),
    {I, decode:value(Field, [KV], XSD)};
transform_prop({K, V}, RI, XSD) when is_tuple(V) ->
    Field = decode:base_name(K,XSD),
    I = index_of(Field, RI) + 1,
   io:format("t: ~p:~p:~p~n", [I, K, Field]),
   io:format("t: ~p~n", [V]),
    P = to_values(V),
   io:format("t: ~p~n", [P]),
    {I, decode:value(Field, [{K,P}], XSD)};
transform_prop({K, V}, RI, XSD) when is_map(V) ->
    Field = decode:base_name(K,XSD),
    I = index_of(Field, RI) + 1,
  % io:format("t: ~p:~p:~p~n", [I, K, Field]),
  % io:format("t: ~p~n", [V]),
    P = maps:values(V),
    {I, decode:value(Field, [{K,P}], XSD)}.

to_values(R) when is_binary(R) -> R;
to_values(R) when is_tuple(R) -> [to_values(V) || V <- array:to_list(R), V=/=undefined];
to_values(R) when is_map(R) -> 
    [case K of
       <<"extension">> -> 
   io:format("2v: map ~p:~p~n", [K,V]),
            {K, maps:values(V)};
       _ -> {K,to_values(V)}
    end || {K,V} <- maps:to_list(R)];
to_values(R) when is_list(R) ->
   io:format("2v: list ~p~n", [R]),
   R.
%%
%% internal functions
%%
-spec index_of(any(), list(any())) -> not_found | non_neg_integer().
index_of(Item, List) -> index_of(Item, List, 1).

-spec index_of(any(), list(any()), non_neg_integer()) -> not_found | non_neg_integer().
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

merge(L1, L2) ->
  orddict:to_list(orddict:merge(fun(_,X,Y) -> Y end, orddict:from_list(L1), orddict:from_list(L2))).

%% path types
%% prop              -> primitive
%% prop:num          -> list primitive
%% prop-child        -> single complex
%% prop:num-child    -> list complex
%% prop:system-child -> list complex 
canonize(Props) ->
    PVs= [{split_path(P), V} || {P, V} <- Props],
    lists:foldl(fun(PV, M) -> 
                gather(PV,M) end, maps:new(), PVs).

gather({[{P, I}|T], V}, Accum) ->               % name indexed 
    gather_list(P, I, T, V, Accum); 
gather({[P|T], V}, Accum) ->
    gather_simple(P, T, V, Accum). 

gather_simple(P, [], V, Accum) -> maps:put(P, V, Accum);
gather_simple(P, Tail, V, Accum) -> 
    % io:format("gather_simple: ~p~n", [P]),
    % io:format("gather_simple: ~p~n", [Tail]),
    case maps:get(P, Accum, {badkey,P}) of
%      {badkey, K} -> maps:put(P, maps:put(Tail, V, maps:new()), Accum);
      {badkey, K} -> maps:put(P, gather({Tail, V}, maps:new()), Accum);
      Complex -> maps:update(P, gather({Tail, V}, Complex), Accum)
    end.
%% simple list
gather_list(P, Index, [], V, Accum) when is_integer(Index) ->
    % io:format("gather_list: ~p:~p:~p=~p~n",[P,Index,V,Accum]),
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, array:set(Index, V, array:new()), Accum);
      List -> maps:update(P, array:set(Index, V, List), Accum)
    end;
%% list with child
gather_list(P, Index, Tail, V, Accum) when is_integer(Index) ->
    % io:format("gather_list: wc ~p:~p~n",[P,Index]),
    % io:format("gather_list: wc ~p:~p~n",[Tail,V]),
    % io:format("gather_list: wc ~p~n",[Accum]),
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, array:set(Index, gather({Tail, V}, maps:new()), array:new()), Accum);
      List -> % io:format("gather_list: list ~p:~p~n",[Index,List]),
              Complex = case array:get(Index, List) of
                            undefined -> maps:new();
                            C -> C
                        end,
              % io:format("gather_list: list ~p~n",[Complex]),
              maps:update(P, array:set(Index, gather({Tail, V}, Complex), List), Accum)
    end;
%% list with codings Tail==[]?
gather_list(<<"extension">>, URL, Tail, V, Accum) when is_binary(URL) ->
     io:format("gather_list: extension:~p~n",[URL]),
     io:format("gather_list: ext ~p:~p~n",[Tail,V]),
     io:format("gather_list: ext ~p~n",[Accum]),
    Complex = [{<<"url">>, URL}, {lists:nth(1,Tail), V}],
    case maps:get(<<"extension">>, Accum, {badkey, extension}) of
      {badkey, K} -> maps:put(<<"extension">>, maps:put(URL, Complex, maps:new()), Accum);
      List -> NewList = case maps:get(URL, List, {badkey, extension}) of
                {badkey, K} -> maps:put(URL, Complex, List);
                Old         -> maps:update(URL, Complex, List)
              end,
              maps:update(<<"extension">>, NewList, Accum)
    end;

%% TODO
gather_list(P, S, Tail, V, Accum) when is_binary(S) ->
     io:format("gather_list: wc ~p:~p~n",[P,S]),
     io:format("gather_list: wc ~p:~p~n",[Tail,V]),
     io:format("gather_list: wc ~p~n",[Accum]),
    Complex = [{<<"system">>, S}, {<<"value">>, V}],
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, maps:put(S, Complex, maps:new()), Accum);
      List -> NewList = case maps:get(S, List, {badkey, P}) of
                {badkey, K} -> maps:put(S, Complex, List);
                Old         -> maps:update(S, Complex, List)
              end,
              maps:update(P, NewList, Accum)
    end.

%% TODO
%% convert back to atoms? 
split_path(P) when is_atom(P) ->
    [ split_index(E) || E <- binary:split(atom_to_binary(P,latin1), <<"-">>,[global])];
split_path(P) when is_binary(P) ->
    [ split_index(E) || E <- binary:split(P, <<"-">>,[global])].

split_index(E) -> 
    case binary:split(E, <<":">>,[global]) of
        [N,I|T] -> case string:to_integer(binary_to_list(I)) of
                       {Index, []} -> {N, Index};
                       {error, _} ->  {N, I}
                   end;
        [N|T] -> N
    end.
%%
%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtm(A, B, P), ?assertEqual(B, update:merge(A, P))).
-define(asrts(A, B), ?assertEqual(B, update:split_path(A))).
-define(asrtc(A, B), ?assertEqual(B, update:canonize(A))).
-define(asrtuo(A, B, P), ?assertEqual(B, update:update(A, P))).
-define(asrtuw(A, B, P, O), ?assertEqual(B, update:update(A, P, O))).

update_merge_test() ->
   ?asrtm(
          [{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-text',<<"old text">>},
           {'name:0-use',<<"official">>},
           {'name:0-suffix:0',<<"von">>}],
          [{'name:0-family',<<"Franzisi">>},
                  {'name:0-given:0',<<"Vausi">>},
                  {'name:0-suffix:0',<<"von">>},
                  {'name:0-text',<<"old text">>},
                  {'name:0-use',<<"official">>}],
          [{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Franzisi">>},
           {'name:0-use',<<"official">>}]
     ).

update_path_test() ->
   ?asrts('name:0', [{<<"name">>, 0}]), 
   ?asrts('name:0-given:0', [{<<"name">>, 0}, {<<"given">>, 0}]), 
   ?asrts('identifier:mrn-value', [{<<"identifier">>, <<"mrn">>}, <<"value">>]), 
   ?asrts('status-coding:test-code', [<<"status">>, {<<"coding">>,<<"test">>}, <<"code">>]), 
   ?asrts('birthDate', [<<"birthDate">>]), 
   ?asrts('complex-value', [<<"complex">>, <<"value">>]).

update_simple_test() ->
   ?asrtc([{birthDate,<<"2019-01-01">>},
           {multipleBirthInteger,<<"1">>}],
          #{<<"birthDate">> => <<"2019-01-01">>,
           <<"multipleBirthInteger">> => <<"1">>}
          ),
   ?asrtc([{status,<<"false">>},
           {status,<<"true">>}],
          #{<<"status">> => <<"true">>}
         ),
   ?asrtc([{'status-coding-code',<<"false">>}],
          #{<<"status">> =>
                       #{<<"coding">> => #{<<"code">> => <<"false">>}}}
         ).

update_list_test() ->
   ?asrtc([{'name:0',<<"Vausi">>},
           {'name:1',<<"Polausi">>},
           {'name:2',<<"Marabusi">>}],
          #{<<"name">> =>
                       {array,3,10,undefined,
                              {<<"Vausi">>,<<"Polausi">>,<<"Marabusi">>,
                               undefined,undefined,undefined,undefined,
                               undefined,undefined,undefined}}}
         ),
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>}],
          #{<<"name">> =>
                       {array,1,10,undefined,
                           {#{<<"family">> => <<"Polausi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                            undefined,undefined,undefined,undefined,undefined,
                            undefined,undefined,undefined,undefined}}}
         ),
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>},
           {'name:1-given:0',<<"Vausi">>},
           {'name:1-family',<<"Franzisi">>},
           {'name:1-use',<<"official">>}],
          #{<<"name">> =>
                       {array,2,10,undefined,
                           {#{<<"family">> => <<"Polausi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                            #{<<"family">> => <<"Franzisi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                            undefined,undefined,undefined,undefined,undefined,
                            undefined,undefined,undefined}}}
         ),
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>},
           {'name:2-given:0',<<"Vausi">>},
           {'name:2-family',<<"Franzisi">>},
           {'name:2-use',<<"official">>}],
          #{<<"name">> =>
                       {array,3,10,undefined,
                           {#{<<"family">> => <<"Polausi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                            undefined,
                            #{<<"family">> => <<"Franzisi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                            undefined,undefined,undefined,undefined,
                            undefined,undefined,undefined}}}
         ).

update_system_test() ->
   ?asrtc([{'identifier:orbispid-value', <<"0063730730">>}],
          #{<<"identifier">> =>
                  #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]}}
         ).

update_extension_test() ->
   ?asrtc([{'extension:rank-valueBoolean',<<"false">>}],
          #{<<"extension">> =>
                       #{<<"rank">> =>
                             [{<<"url">>,<<"rank">>},
                              {<<"valueBoolean">>,<<"false">>}]}}
         ).

update_coding_test() ->
   ?asrtc([{'status-coding:test-code', <<"completed">>}],
          #{<<"status">> =>
                       #{<<"coding">> =>
                             #{<<"test">> =>
                                   [{<<"system">>,<<"test">>},
                                    {<<"value">>,<<"completed">>}]}}}
         ). 

update_complex_test() ->
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>},
           {'identifier:orbispid-value', <<"0063730730">>},
           {birthDate,<<"2019-01-01">>},
           {multipleBirthInteger,<<"1">>}],
          #{<<"birthDate">> => <<"2019-01-01">>,
            <<"identifier">> =>
                       #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]},
            <<"multipleBirthInteger">> => <<"1">>,
            <<"name">> =>
                       {array,1,10,undefined,
                           {#{<<"family">> => <<"Polausi">>,
                              <<"given">> =>
                                  {array,1,10,undefined,
                                      {<<"Vausi">>,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined,undefined,undefined,
                                       undefined}},
                              <<"use">> => <<"official">>},
                           undefined,undefined,undefined,undefined,
                           undefined,undefined,undefined,undefined,
                           undefined}}}
          ),
   ?asrtc([{'identifier:orbispid-value', <<"0063730730">>},
           {'identifier:mrn-value', <<"1234567890">>}
          ],
          #{<<"identifier">> =>
                  #{<<"mrn">> =>
                             [{<<"system">>,<<"mrn">>},
                              {<<"value">>,<<"1234567890">>}],
                    <<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]}}
         ),
   ?asrtc([{'identifier:orbispid-value', <<"0063730730">>},
           {'identifier:orbispid-value', <<"1234567890">>}
          ],
          #{<<"identifier">> =>
                  #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"1234567890">>}]}}
         ).
update_patient1_test() ->
   ?asrtuo(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,[],[],undefined,<<"2019-01-01">>,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
            [
             {birthDate,<<"2019-01-01">>}
            ]),
   ?asrtuo(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,[],[],undefined, undefined,undefined,[],undefined,
            {<<"Integer">>,<<"1">>},[],[],[],[],undefined,[]},
            [
             {multipleBirthInteger,<<"1">>}
            ]),
   ?asrtuo(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],
            [{'Identifier',[],undefined,[],undefined,undefined, <<"orbispid">>,<<"0063730730">>,undefined, undefined}],
            undefined,[],[],undefined,undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
            [
             {'identifier:orbispid-value', <<"0063730730">>}
            ]),
   ?asrtuo(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,
            [{'HumanName',[],undefined,[],<<"official">>,undefined, <<"Polausi">>, [<<"Vausi">>], [],[],undefined}],
            [],undefined,undefined,undefined,[],undefined,undefined,[],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>}]
          ).

update_patient_ext_test() ->
   ?asrtuo(
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],[],
            undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],
                     [{'Extension',[],undefined,[],<<"rank">>,
                          {valueBoolean,<<"false">>}}],
                     [],[],undefined,[],[],undefined,undefined,undefined,[],
                     undefined,undefined,[],[],[],[],undefined,[]},
           [
             {'extension:rank-valueBoolean',<<"false">>}
           ]).

update_patient2_test() ->
   ?asrtuo(
           {'Patient',[],undefined,undefined,undefined,undefined,
                          undefined,[],[],[],[],undefined,
                          [{'HumanName',[],undefined,[],<<"official">>,undefined,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [],[],undefined}],
                          [],undefined,undefined,undefined,[],undefined,undefined,[],
                          [],[],[],undefined,[]},
           {'Patient',[],undefined,undefined,undefined,undefined,
                     undefined,[],[],[],
                     [{'Identifier',[],undefined,[],undefined,undefined,
                          <<"orbispid">>,<<"0063730730">>,undefined,
                          undefined}],
                     undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Polausi">>,
                          [<<"Vausi">>],
                          [],[],undefined}],
                     [],undefined,<<"2019-01-01">>,undefined,[],undefined,
                     {<<"Integer">>,<<"1">>},
                     [],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'identifier:orbispid-value', <<"0063730730">>},
             {birthDate,<<"2019-01-01">>},
             {multipleBirthInteger,<<"1">>}]
          ),
   ?asrtuo(
           {'Patient',[],undefined,undefined,undefined,undefined,
                          undefined,[],[],[],[],undefined,
                          [{'HumanName',[],undefined,[],<<"official">>,undefined,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [],[],undefined}],
                          [],undefined,undefined,undefined,[],undefined,undefined,[],
                          [],[],[],undefined,[]},
           {'Patient',[],undefined,undefined,undefined,undefined,
                     undefined,[],[],[],[],undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Polausi">>,
                          [<<"Vausi">>],
                          [],[],undefined},
                      {'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Franizisi">>,
                          [<<"Vausi">>],
                          [],[],undefined}],
                     [],undefined,undefined,undefined,[],undefined,undefined,
                     [],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'name:1-given:0',<<"Vausi">>},
             {'name:1-family',<<"Franizisi">>},
             {'name:1-use',<<"official">>}]
          ),
   ?asrtuo(
           {'Patient',[],undefined,undefined,undefined,undefined,
                          undefined,[],[],[],[],undefined,
                          [{'HumanName',[],undefined,[],<<"official">>,<<"old text">>,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [<<"von">>],[],undefined}],
                          [],undefined,undefined,undefined,[],undefined,undefined,[],
                          [],[],[],undefined,[]},
           {'Patient',[],undefined,undefined,undefined,undefined,
                     undefined,[],[],[],[],undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,<<"old text">>,
                          <<"Polausi">>,
                          [<<"Vausi">>],
                          [<<"von">>],[],undefined},
                      {'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Franizisi">>,
                          [<<"Vausi">>],
                          [],[],undefined}],
                     [],undefined,undefined,undefined,[],undefined,undefined,
                     [],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'name:2-given:0',<<"Vausi">>},
             {'name:2-family',<<"Franizisi">>},
             {'name:2-use',<<"official">>}]
          ).

update_partial_patient_test() ->
   ?asrtuw(
           {'Patient',[],undefined,undefined,undefined,undefined,
                          undefined,[],[],[],[],undefined,
                          [{'HumanName',[],undefined,[],<<"official">>,undefined,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [],[],undefined}],
                          [],undefined,undefined,undefined,[],undefined,undefined,[],
                          [],[],[],undefined,[]},
           {'Patient',[],undefined,undefined,undefined,undefined,
                     undefined,[],[],[],
                     [{'Identifier',[],undefined,[],undefined,undefined,
                          <<"orbispid">>,<<"0063730730">>,undefined,
                          undefined}],
                     undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Polausi">>,
                          [<<"Vausi">>],
                          [],[],undefined}],
                     [],undefined,<<"2019-01-01">>,undefined,[],undefined,
                     {<<"Integer">>,<<"1">>},
                     [],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'identifier:orbispid-value', <<"0063730730">>},
             {birthDate,<<"2019-01-01">>},
             {multipleBirthInteger,<<"1">>}],
            [update]
          ).

-endif.
