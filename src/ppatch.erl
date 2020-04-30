-module(ppatch).
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
  io:format("update:update ~p~n", [GP]),
  update2(Resource, GP, Opts).

%% TODO
%% manipulating tuple as list
%% lists:zip(record_info(fields, foobar), tl(tuple_to_list(Rec)));
%% extract Opts
update2(Resource, Props, Opts) ->
  RT = erlang:element(1,Resource),
  RI = [atom_to_binary(F,latin1) || F <- resource:fields(RT)],
  XSD = decode:xsd_info(atom_to_binary(RT,latin1)),
  Keys = maps:keys(Props),
  % io:format("update2: keys: ~p~n~p~n", [Keys, RI]),
  [Mode] = Opts,
  lists:foldl(fun(K, R) -> update_prop(Mode, K, maps:get(K, Props), R, RI, XSD) end, Resource, Keys).

update_prop(update, K, V, R, RI, XSD) ->
  io:format("update:update_prop upd ~p~n", [R]),
  io:format("update:update_prop upd ~p:~p~n", [K,V]),
  Field = decode:base_name(K,XSD),
  I = index_of(Field, RI) + 1,
  P = transform(K,V, Field, XSD),
  io:format("update:update_prop upd ~p:~p~n", [I, P]),
  setelement(I,R,P);
update_prop(merge, K, V, R, RI, XSD) ->
  io:format("update:update_prop merge ~p~n", [R]),
  io:format("update:update_prop merge ~p:~p~n", [K,V]),
  Field = decode:base_name(K,XSD),
  I = index_of(Field, RI) + 1,
  P0 = element(I,R), 
  P = merge(K,V, P0, Field, XSD),
  io:format("update:update_prop merge ~p:~p~n", [I, P]),
  setelement(I,R,P);
update_prop(replace, K, V, R, RI, XSD) ->
  io:format("upd: ~p~n", [R]),
  io:format("upd: ~p:~p~n", [K, V]),
  Field = decode:base_name(K,XSD),
  I = index_of(Field, RI) + 1,
  P = transform(K,V, Field, XSD),
  io:format("upd: ~p:~p~n", [I, P]),
  setelement(I,R,P).

transform(K, V, Field, XSD) when is_binary(V) ->
%  io:format("t: ~p:~p~n", [K, PropInfo]),
  decode:value(Field, [{K,V}], XSD);
transform(K, V, Field, XSD) when is_tuple(V) ->
  io:format("t: ~p~n", [V]),
  P = to_values(V),
  io:format("t: ~p~n", [P]),
  decode:value(Field, [{K,P}], XSD);
transform(K, V, Field, XSD) when is_list(V) ->
  % io:format("t: ~p~n", [V]),
  decode:value(Field, [{K,V}], XSD);
transform(K, V, Field, XSD) when is_map(V) ->
  io:format("t: ~p:~p~n", [K,V]),
  P = maps:values(V),
  decode:value(Field, [{K,P}], XSD).

merge(K, V, P0, Field, XSD) when is_binary(V) ->
  io:format("merge bin: ~p:~p:~p~n", [K, V, P0]),
  decode:value(Field, [{K,V}], XSD);
merge(K, V, P0, Field, XSD) when is_tuple(V) ->
  io:format("merge tuple: ~p~n", [V]),
  P = to_values(V),
  io:format("merge tuple: ~p~n", [P]),
  decode:value(Field, [{K,P}], XSD);
merge(K, V, P0, Field, XSD) when is_list(V) ->  % V is KVList
  io:format("merge list: ~p:~p~n", [K,V]),
  D = decode:value(Field, [{K,V}], XSD),
  RT = element(1,lists:nth(1,D)),
  P1 = merge2(RT,P0,D),
  io:format("merge list: ~p~n", [P1]),
  P1;
merge(K, V, P0, Field, XSD) when is_map(V) ->
  io:format("merge map: ~p~n", [V]),
  P = maps:values(V),
  decode:value(Field, [{K,P}], XSD).

merge2(Extension, E1, E2) ->
  io:format("merge2 ~p~n", [E1]),
  io:format("merge2 ~p~n", [E2]),
  O1 = ordsets:from_list(E1), 
  O2 = ordsets:from_list(E2), 
  ordsets:to_list(ordsets:union(O1,O2)).


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

gather({[{array, P, I}|T], V}, Accum) ->           % name indexed 
    G = gather_list(P, I, T, V, Accum),
    io:format("gather list ~p~n",[G]),
    G; 
gather({[{coding, S}|T], V}, Accum) ->
    G = gather_coding(S, T, V, Accum),
    io:format("gather_coding  ~p~n",[G]),
    G; 
gather({[{identifier, S}|T], V}, Accum) ->
    G = gather_identifier(S, T, V, Accum),
    io:format("gather_identifier ~p~n",[G]),
    G; 
gather({[{extension, URL, I}|T], V}, Accum) ->   
    G = gather_extension(URL, I, T, V, Accum),
    io:format("gather_extension ~p~n",[G]),
    G; 
gather({[P|T], V}, Accum) ->
    G = gather_simple(P, T, V, Accum), 
    io:format("gather simple ~p~n",[G]),
    G. 

%-spec gather_simple(binary(), list(), binary(), maps:map()) -> (tuple(), maps:map()).
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
    end.
%% list with codings Tail==[]?

gather_extension(URL, Index, Tail, V, Accum) when is_binary(URL) ->
    io:format("gather_ext ~p~n",[URL]),
    io:format("gather_ext ~p:~p~n",[Tail,V]),
    io:format("gather_ext ~p~n",[Accum]),
    KV = case Tail of 
        [Key] -> maps:put(Key, V, maps:new());
        _     -> case lists:nth(1,Tail) of
                 {extension,SubURL} -> io:format("gather_ext recur ext ~p~n",[SubURL]),
                                    SubExt = gather({Tail, V}, maps:new());
                 Prop                     -> io:format("gather_ext recur non-ext ~p~n",[Prop]),
                                             gather({Tail, V}, maps:new())
                 end
        end,
    % NewExt0 = maps:put(<<"url">>, URL, maps:new()),
    % NewExt = array:set(0,maps:merge(NewExt0, KV),array:new()),
    NewExt = array:set(0,KV,array:new()),
    io:format("gather_ext ~p~n",[NewExt]),
    case maps:get(extension, Accum, {badkey, extension}) of
      {badkey, K} -> maps:put(extension, maps:put(URL, NewExt, maps:new()), Accum);
      OldExtList -> io:format("gather_ext: found ~p~n",[OldExtList]),
              NewExtList = case maps:get(URL, OldExtList, {badkey, extension}) of
                {badkey, K} -> maps:put(URL, NewExt, OldExtList);
                OldExt      -> io:format("gather_ext: update array ~p:~p~n",[OldExt,Index]),
                               io:format("gather_ext: update array ~p~n",[NewExt]),
                               Old = array:get(Index,OldExt),
                               New = array:get(0,NewExt),
                               Ext = array:set(Index,maps:merge(New,Old),OldExt),
                               maps:update(URL, Ext, OldExtList)
              end,
              maps:update(extension, NewExtList, Accum)
    end.

gather_coding(S, Tail, V, Accum) when is_binary(S) ->
     io:format("gather_coding ~p~n",[S]),
     io:format("gather_coding ~p:~p~n",[Tail,V]),
     io:format("gather_coding ~p~n",[Accum]),
    Complex = [{<<"system">>, S}, {<<"value">>, V}],
    case maps:get(coding, Accum, {badkey, coding}) of
      {badkey, K} -> maps:put(coding, maps:put(S, Complex, maps:new()), Accum);
      List -> NewList = case maps:get(S, List, {badkey, coding}) of
                {badkey, K} -> maps:put(S, Complex, List);
                Old         -> maps:update(S, Complex, List)
              end,
              maps:update(coding, NewList, Accum)
    end.

gather_identifier(S, Tail, V, Accum) when is_binary(S) ->
     io:format("gather_identifier ~p~n",[S]),
     io:format("gather_identifier ~p:~p~n",[Tail,V]),
     io:format("gather_identifier ~p~n",[Accum]),
    Complex = [{<<"system">>, S}, {<<"value">>, V}],
    case maps:get(identifier, Accum, {badkey, identifier}) of
      {badkey, K} -> maps:put(identifier, maps:put(S, Complex, maps:new()), Accum);
      List -> NewList = case maps:get(S, List, {badkey, identifier}) of
                {badkey, K} -> maps:put(S, Complex, List);
                Old         -> maps:update(S, Complex, List)
              end,
              maps:update(identifier, NewList, Accum)
    end.

%% TODO
%% convert back to atoms? 
%%
%% split_path()
%%
%% uri component in path can't contain special chars as ':', '_', '-'.
%% CSS selectors only allow '_' and '-'
%% path is concatenated with '-'
%% 
-spec split_path( (atom()|binary()) ) -> list((binary()|tuple())).
split_path(P) when is_atom(P) ->
    [ split_prop_expr(E) || E <- binary:split(atom_to_binary(P,latin1), <<"-">>,[global])];
split_path(P) when is_binary(P) ->
    [ split_prop_expr(E) || E <- binary:split(P, <<"-">>,[global])].

%%
%% split_prop_expr()
%%
%% uri component in path can't contain special chars as ':', '-'.
%% CSS selectors only allow '_' and '-'
%% simple prop       -> prop
%% array prop:int    -> {array, prop, index)
%% array identifier:uri -> {identifier, system)
%% array coding:uri -> {coding, system)
%% extension array   -> {extension, url, index}
%%
-spec split_prop_expr( binary() ) -> (binary()|tuple()). 
split_prop_expr(E) -> 
    case binary:split(E, <<":">>,[global]) of
    [<<"extension">>,URL,I|T] when is_binary(URL) -> case string:to_integer(binary_to_list(I)) of
                   {Index, []} -> {extension, URL, Index};
                   {error, _} ->  throw("ppatch:split_index illegal extension path")
               end;
    [<<"extension">>,I|T] -> case string:to_integer(binary_to_list(I)) of
                   {Index, []} -> {extension, undefined, Index};   %% ?? useful or not
                   {error, _} ->  {extension, I, undefined}
               end;
    [<<"identifier">>,System|T] -> {identifier, System};
    [<<"coding">>,System|T]     -> {coding, System};
    [N,I|T] -> case string:to_integer(binary_to_list(I)) of
                   {Index, []} -> {array, N, Index};
                   {error, _} ->  {coding, N, I}
               end;
    [N|T] -> N
    end.
%%
%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrts(A, B), ?assertEqual(B, ppatch:split_path(A))).
-define(asrtc(A, B), ?assertEqual(B, ppatch:canonize(A))).
-define(asrtm(A, B, P), ?assertEqual(B, ppatch:merge(A, P))).
-define(asrtuo(A, B, P), ?assertEqual(B, ppatch:update(A, P))).
-define(asrtuw(A, B, P, O), ?assertEqual(B, ppatch:update(A, P, O))).

ppatch_splitpath1_test() ->
   ?asrts('birthDate', [<<"birthDate">>]), 
   ?asrts('name:0', [{array, <<"name">>, 0}]), 
   ?asrts('name:0-given:0', [{array, <<"name">>, 0}, {array, <<"given">>, 0}]), 
   ?asrts('identifier:mrn-value', [{identifier, <<"mrn">>}, <<"value">>]), 
   ?asrts('status-coding:test-code', [<<"status">>, {coding,<<"test">>}, <<"code">>]), 
   ?asrts('complex-value', [<<"complex">>, <<"value">>]).

ppatch_splitpath2_test() ->
   ?asrts('extension:url-valueBoolean', [{extension, <<"url">>, undefined},<<"valueBoolean">>]),
   ?asrts('extension:0-valueBoolean', [{extension, undefined,0},<<"valueBoolean">>]),
   ?asrts('extension:url:0-valueBoolean', [{extension, <<"url">>, 0},<<"valueBoolean">>]).

ppatch_simple_test() ->
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

ppatch_list_test() ->
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

ppatch_system_test() ->
   ?asrtc([{'identifier:orbispid-value', <<"0063730730">>}],
          #{identifier =>
                  #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]}}
         ).

ppatch_extension1_test() ->
   ?asrtc([{'extension:rank-valueBoolean',<<"false">>}],
          #{extension =>
                       #{<<"rank">> =>
                         {array,1,10,undefined,
                                    {#{<<"valueBoolean">> => <<"false">>},
                                     undefined,undefined,undefined,undefined,
                                     undefined,undefined,undefined,undefined,
                                     undefined}}}}
         ).
ppatch_extension2_test() ->
   ?asrtc( [{<<"extension:condition:0-extension:checked-valueBoolean">>,false}],
           #{extension =>
                       #{<<"condition">> =>
                         {array,1,10,undefined,
                        {#{extension =>
                            #{<<"checked">> =>
                               {array,1,10,undefined,
                                {#{<<"valueBoolean">> => false},
                                 undefined,undefined,undefined,undefined,
                                 undefined,undefined,undefined,undefined,
                                 undefined}}}},
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined}}}}
         ),
   ?asrtc( [{<<"extension:condition:0-valueReference-display">>,<<"test">>}],
           #{extension =>
                       #{<<"condition">> =>
                              {array,1,10,undefined,
                                    {#{<<"valueReference">> =>
                                           #{<<"display">> => <<"test">>}},
                                     undefined,undefined,undefined,undefined,
                                     undefined,undefined,undefined,undefined,
                                     undefined}}}}
        ).
ppatch_extension3_test() ->
   ?asrtc( [{<<"extension:condition:0-extension:checked-valueBoolean">>,false},
            {<<"extension:condition:0-valueReference-display">>,<<"test">>}],
           #{extension =>
                    #{<<"condition">> =>
                       {array,1,10,undefined,
                        {#{extension =>
                            #{<<"checked">> =>
                               {array,1,10,undefined,
                                {#{<<"valueBoolean">> => false},
                                 undefined,undefined,undefined,undefined,
                                 undefined,undefined,undefined,undefined,
                                 undefined}}},
                           <<"valueReference">> =>
                            #{<<"display">> => <<"test">>}},
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined}}}}
        ).


ppatch_coding_test() ->
   ?asrtc([{'status-coding:test-code', <<"completed">>}],
          #{<<"status">> =>
                       #{coding =>
                             #{<<"test">> =>
                                   [{<<"system">>,<<"test">>},
                                    {<<"value">>,<<"completed">>}]}}}
         ). 

ppatch_complex_test() ->
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>},
           {'identifier:orbispid-value', <<"0063730730">>},
           {birthDate,<<"2019-01-01">>},
           {multipleBirthInteger,<<"1">>}],
          #{<<"birthDate">> => <<"2019-01-01">>,
            identifier =>
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
          #{identifier =>
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
          #{identifier =>
                  #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"1234567890">>}]}}
         ).

ppatch_merge_test() ->
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

ppatch_patient1_test() ->
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

ppatch_patient_ext_test() ->
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

ppatch_patient2_test() ->
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
          ).

ppatch_patient3_test() ->
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
             {'name:2-given:0',<<"Vausi">>},
             {'name:2-family',<<"Franizisi">>},
             {'name:2-use',<<"official">>}]
          ).

ppatch_partial_patient_test() ->
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

ppatch_merge_ext_test() ->
   ?asrtuw(
           {'Patient',[],undefined,undefined,undefined,undefined,
                          undefined,[],
                     [{'Extension',[],undefined,[],<<"#recipient">>,
                          {valueReference,
                              {'Reference',[],undefined,[],<<"absd">>,
                                  undefined,undefined,
                                  <<"Lowe123, Dick987">>}}}],
                     [],[],undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,undefined,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [],[],undefined}],
                     [],undefined,undefined,undefined,[],undefined,undefined,[],
                     [],[],[],undefined,[]},
           {'Patient',[],undefined,undefined,undefined,undefined,
                     undefined,[],
                     [{'Extension',[],undefined,[],<<"#recipient">>,
                          {valueReference,
                              {'Reference',[],undefined,[],<<"absd">>,
                                  undefined,undefined,
                                  <<"Lowe123, Dick987">>}}},
                      {'Extension',[],undefined,[],<<"#recipient">>,
                          {valueReference,
                              {'Reference',[],undefined,[],<<"ertz">>,
                                  undefined,undefined,<<"Lowe345, Pussy">>}}}],
                     [],[],undefined,
                     [{'HumanName',[],undefined,[],<<"official">>,undefined,
                          <<"Dummy">>,
                          [<<"Detlef">>],
                          [],[],undefined}],
                     [],undefined,undefined,undefined,[],undefined,undefined,
                     [],[],[],[],undefined,[]},
            [
             {<<"extension">>, [
                  {[{<<"url">>, <<"#recipient">>},
                    {<<"valueReference">>,{[
                            {<<"reference">>, <<"ertz">>},
                            {<<"display">>, <<"Lowe345, Pussy">>}
                        ]}}
                   ]}
               ]}
            ],
            [merge]
          ).

-endif.
