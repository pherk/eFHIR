-module(special).
-compile(export_all).
-include("primitives.hrl").
%%
%% API exports
%%
-export_type(['Meta'/0]).
-export_type(['Narrative'/0]).
-export_type(['Reference'/0]).


%%
%%   - Special Datatypes
%%
-record('Narrative', {
      extension     :: [extensions:'Extension'()]
    , status :: binary()
    , 'div' :: binary()
    }).
-opaque 'Narrative'() :: #'Narrative'{}.

-record('Meta', {
      extension     :: [extensions:'Extension'()]
    , versionId = 0 :: positiveInt()
    , lastUpdated   :: dateTime()
    , source        :: binary()
    , profile       :: [uri()]
    , security      :: [complex:'Coding'()]
    , tag           :: [complex:'Coding'()]
    }).
-opaque 'Meta'()   :: #'Meta'{}.

-record('Reference', {
       extension   :: [extensions:'Extension'()]
     , 'reference' :: binary()
     , display     :: binary()
}).
-opaque 'Reference'()    :: #'Reference'{}.


%%====================================================================
%% API functions
%%====================================================================
to_narrative({Props}) -> to_narrative(Props);
to_narrative(Props) ->
    DT = decode:xsd_info(<<"Narrative">>),
    io:format("~p~n~p~n",[Props,DT]),
    #'Narrative'{
        extension = decode:value(<<"extension">>, Props, DT)
      , status = decode:value(<<"status">>, Props, DT)
      , 'div'   = decode:value(<<"div">>, Props, DT)
      }.

to_meta({Props}) -> to_meta(Props);
to_meta(Props) ->
    DT = decode:xsd_info(<<"Meta">>),
    io:format("~p~n~p~n",[Props,DT]),
    #'Meta'{
        extension    = decode:value(<<"extension">>, Props, DT)
      , versionId    = decode:value(<<"versionId">>, Props, DT)
      , lastUpdated  = decode:value(<<"lastUpdated">>, Props, DT)
      , source       = decode:value(<<"source">>, Props, DT)
      , profile      = decode:value(<<"profile">>, Props, DT) 
      , security     = decode:value(<<"security">>, Props, DT)
      , tag          = decode:value(<<"tag">>, Props, DT)
      }.

to_reference({Props}) -> to_reference(Props);
to_reference(Props) ->
    DT = decode:xsd_info(<<"Reference">>),
    io:format("~p~n~p~n",[Props,DT]),
    #'Reference'{
        extension  = decode:value(<<"extension">>, Props, DT)
      , 'reference' = decode:value(<<"reference">>, Props, DT)
      , display    = decode:value(<<"display">>, Props, DT)
      }.

%%
%% Access functions
%%
narrative(undefined) -> <<"no text">>;
narrative(#'Narrative'{'div'=Text}) -> Text.

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
            {'Meta',
                 [{'Extension',
                     <<"http://eNahar.org/nabu/extension#lastUpdatedBy">>,
                     {valueReference,
                         {'Reference',[], <<"metis/practitioners/u-vkr">>, <<"von Kleist-Retzow, JÃ¼rgen-Christoph">>}}}],
                 <<"999">>,<<"2019-07-14T09:10:10">>,undefined, [], [], 
                 [{'Coding', <<"http://eNahar.org/test">>,undefined,<<"hello">>,undefined,undefined}]
           }).


-endif.

