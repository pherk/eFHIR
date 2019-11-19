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

update(Resource, Props) -> update(Resource, Props, []).

update(Resource, Props, Opts) ->
  RT = erlang:atom_to_binary(erlang:element(1,Resource), latin1),
  GP = canonize(Props), 
  Resource.

%%
%% internal functions
%%
%% path types
%% prop              -> primitive
%% prop:num          -> list primitive
%% prop-child        -> single complex
%% prop:num-child    -> list complex
%% prop:system-child -> list complex 
canonize(Props) ->
    PVs= [{split_path(P), V} || {P, V} <- Props],
    Raw = lists:foldl(fun(PV, M) -> 
                gather(PV,M) end, maps:new(), PVs),
    io:format("canonize: ~p~n", [Raw]),
    io:format("canonize array: ~p~n ", [array:to_list(maps:get(<<"name">>, Raw, array:new()))]),
    Raw.

gather({[{P, I}, C|T], V}, Accum) when is_integer(I) ->               % name indexed plus complex
    gather_list(P, I, C, V, Accum); 
gather({[{P, S}, C|T], V}, Accum) when is_binary(S) ->                % name by system plus complex
    gather_list(P, S, C, V, Accum); 
gather({[{P, I}|T], V}, Accum) when is_integer(I) ->
    gather_list(P, I, undefined, V, Accum); 
gather({[{P, S}|T], V}, Accum) when is_binary(S) ->
    gather_list(P, S, undefined, V, Accum); 
gather({[P, C|T], V}, Accum) ->
    gather_complex(P, C, V, Accum); 
gather({[P|T], V}, Accum) -> maps:put(P, V, Accum).

gather_complex(P,C, V, Accum) -> 
    case maps:get(P, Accum, {badkey,P}) of
      {badkey, K} -> maps:put(P, maps:put(C,V, maps:new()), Accum);
      Complex -> maps:update(P, gather({[C],V}, Complex), Accum)
    end.
%% simple list
gather_list(P, Index, undefined, V, Accum) when is_integer(Index) ->
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, array:set(Index, V, array:new()), Accum);
      List -> maps:update(P, array:set(Index, V, List), Accum)
    end;
%% list with child
gather_list(P, Index, C, V, Accum) when is_integer(Index) ->
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, array:set(Index, gather({[C], V}, maps:new()), array:new()), Accum);
      List -> Complex = array:get(Index, List),
              maps:update(P, array:set(Index, gather({[C], V}, Complex), List), Accum)
    end;
%% list with codings
gather_list(P, S, C, V, Accum) ->
    Complex = [{<<"system">>, S}, {<<"value">>, V}],
    case maps:get(P, Accum, {badkey, P}) of
      {badkey, K} -> maps:put(P, maps:put(S, Complex, maps:new()), Accum);
      List -> NewList = case maps:get(S, List, {badkey, P}) of
                {badkey, K} -> maps:put(S, Complex, List);
                Old         -> maps:update(S, Complex, List)
              end,
              maps:update(P, NewList, Accum)
    end.


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

-define(asrts(A, B), ?assertEqual(B, update:split_path(A))).
-define(asrtc(A, B), ?assertEqual(B, update:canonize(A))).
-define(asrtuo(A, B, P), ?assertEqual(B, update:update(A, P))).
-define(asrtuw(A, B, P, O), ?assertEqual(B, update:update(A, P, O))).

update_path_test() ->
   ?asrts('name:0', [{<<"name">>, 0}]), 
   ?asrts('name:0-given:0', [{<<"name">>, 0}, {<<"given">>, 0}]), 
   ?asrts('identifier:mrn-value', [{<<"identifier">>, <<"mrn">>}, <<"value">>]), 
   ?asrts('status-coding:test-code', [<<"status">>, {<<"coding">>,<<"test">>}, <<"code">>]), 
   ?asrts('birthDate', [<<"birthDate">>]), 
   ?asrts('complex-value', [<<"complex">>, <<"value">>]).

update_simple_test() ->
   ?asrtc([{birthDate,<<"2019-01-01">>},
           {multipleBirth,<<"1">>}],
          #{<<"birthDate">> => <<"2019-01-01">>,
           <<"multipleBirth">> => <<"1">>}
          ),
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
   ?asrtc([{'identifier:orbispid-value', <<"0063730730">>}],
          #{<<"identifier">> =>
                  #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]}}
         ),
   ?asrtc([{'status-coding:test-code', <<"completed">>}],
          #{<<"status">> => 
                  {#{<<"coding">> => [{<<"system">>, <<"test">>}, {<<"code">>, <<"completed">>]}}}
         ), 
   ?asrtc([{status,<<"false">>},
           {status,<<"true">>}],
          #{<<"status">> => <<"true">>}
         ).

update_complex_test() ->
   ?asrtc([{'name:0-given:0',<<"Vausi">>},
           {'name:0-family',<<"Polausi">>},
           {'name:0-use',<<"official">>},
           {'identifier:orbispid-value', <<"0063730730">>},
           {birthDate,<<"2019-01-01">>},
           {multipleBirth,<<"1">>}],
          #{<<"birthDate">> => <<"2019-01-01">>,
            <<"identifier">> =>
                       #{<<"orbispid">> =>
                             [{<<"system">>,<<"orbispid">>},
                              {<<"value">>,<<"0063730730">>}]},
            <<"multipleBirth">> => <<"1">>,
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
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
           {'Patient',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
            [],undefined,[],[],undefined, undefined,undefined,[],undefined,
            undefined,[],[],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'identifier:orbispid-value', <<"0063730730">>},
             {birthDate,<<"2019-01-01">>},
             {multipleBirth,<<"1">>}]
          ).
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
                          undefined,[],[],[],[],undefined,
                          [{'HumanName',[],undefined,[],<<"official">>,undefined,
                                        <<"Dummy">>,
                                        [<<"Detlef">>],
                                        [],[],undefined}],
                          [],undefined,undefined,undefined,[],undefined,undefined,[],
                          [],[],[],undefined,[]},
            [{'name:0-given:0',<<"Vausi">>},
             {'name:0-family',<<"Polausi">>},
             {'name:0-use',<<"official">>},
             {'identifier:orbispid-value', <<"0063730730">>},
             {birthDate,<<"2019-01-01">>},
             {multipleBirth,<<"1">>}]
          ).

-endif.
