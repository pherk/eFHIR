-module(basic).
%%%
%%% FHIR 4.0.0 Basic
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Basic', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , code :: complex:'CodeableConcept'()
    , subject :: special:'Reference'() | undefined
    , created :: date() | undefined
    , author :: special:'Reference'() | undefined
    }).
-type 'Basic'() :: #'Basic'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_basic({Props}) -> to_basic(Props);
to_basic(Props) -> 
    DT = decode:xsd_info(<<"Basic">>),
    #'Basic'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    , contained = decode:value(<<"contained">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier = decode:value(<<"identifier">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , created = decode:value(<<"created">>, Props, DT)
    , author = decode:value(<<"author">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, basic:to_basic(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
