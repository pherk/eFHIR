-module(special).
-compile(export_all).
-include("primitives.hrl").
%%
%% API exports
%%
-export_type([meta/0]).
-export_type([narrative/0]).
-export_type([reference_/0]).


%%
%%   - Special Datatypes
%%
-record(narrative, {
      status :: binary()
    , div_ :: binary()
    }).
-opaque narrative() :: #narrative{}.

-record(meta, {
       versionId = 0 :: positiveInt()
     , lastUpdated   :: dateTime()
     , source        :: binary()
     , profile       :: [uri()]
     , security      :: [complex:coding()]
     , tag           :: [complex:coding()]
     , extension     :: [extensions:extension()]
}).
-opaque meta()   :: #meta{}.

-record(reference, {
       reference_ :: binary()
     , display   :: binary()
}).
-opaque reference_()    :: #reference{}.


%%====================================================================
%% API functions
%%====================================================================
to_narrative({Props}) -> to_narrative(Props);
to_narrative(Props) ->
    DT = decode:xsd_info(<<"Narrative">>),
    io:format("~p~n~p~n",[Props,DT]),
    #narrative{
        status = decode:value(<<"status">>, Props, DT)
      , div_   = decode:value(<<"div">>, Props, DT)
      }.

to_meta({Props}) -> to_meta(Props);
to_meta(Props) ->
    DT = decode:xsd_info(<<"Meta">>),
    io:format("~p~n~p~n",[Props,DT]),
    #meta{
        versionId    = decode:value(<<"versionId">>, Props, DT)
      , lastUpdated  = decode:value(<<"lastUpdated">>, Props, DT)
      , source       = decode:value(<<"source">>, Props, DT)
      , profile      = decode:value(<<"profile">>, Props, DT) 
      , security     = decode:value(<<"security">>, Props, DT)
      , tag          = decode:value(<<"tag">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      }.

to_reference({Props}) -> to_reference(Props);
to_reference(Props) ->
    DT = decode:xsd_info(<<"Reference">>),
    io:format("~p~n~p~n",[Props,DT]),
    #reference{
        reference_ = decode:value(<<"reference">>, Props, DT)
      , display    = decode:value(<<"display">>, Props, DT)
      }.

%%
%% Access functions
%%
narrative(undefined) -> <<"no text">>;
narrative(#narrative{div_=Text}) -> Text.

%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, A)).
-define(asrtpr(A, B), ?assertEqual(B, utils:rec_to_prop(A))).

complex_meta_test() ->
    ?asrtto(special:to_meta({[{<<"versionId">>, <<"999">>},
                              {<<"lastUpdated">>,<<"2019-07-14T09:10:10">>},
                              {<<"tag">>,
                                   [{[{<<"system">>,<<"http://eNahar.org/test">>},
                                      {<<"code">>,<<"hello">>}]}]},
                              {<<"extension">>,
                                   [{[{<<"url">>, <<"http://eNahar.org/nabu/extension#lastUpdatedBy">>},
                                      {<<"valueReference">>,
                                             {[{<<"reference">>, <<"metis/practitioners/u-vkr">>},
                                               {<<"display">>, <<"von Kleist-Retzow, JÃ¼rgen-Christoph">>}]}}]}]}
                             ]}),
            {meta,<<"999">>,<<"2019-07-14T09:10:10">>,undefined, [], [], 
                          [{coding, <<"http://eNahar.org/test">>,undefined,<<"hello">>,undefined,undefined}],
                          [{extension,
                            <<"http://eNahar.org/nabu/extension#lastUpdatedBy">>,
                              {valueReference,
                                 {reference,<<"metis/practitioners/u-vkr">>, <<"von Kleist-Retzow, JÃ¼rgen-Christoph">>}}}]}).


-endif.

