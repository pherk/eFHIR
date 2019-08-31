-module('Binary').
%%%
%%% FHIR 4.0.0 Binary
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Binary', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , contentType :: code()
    , securityContext :: special:'Reference'() | undefined
    , data :: base64Binary() | undefined
    }).
-type 'Binary'() :: #'Binary'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_binary({Props}) -> to_binary(Props);
to_binary(Props) -> 
    DT = decode:xsd_info(<<"Binary">>),
    #'Binary'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , contentType = decode:value(<<"contentType">>, Props, DT)
    , securityContext = decode:value(<<"securityContext">>, Props, DT)
    , data = decode:value(<<"data">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, 'Binary':to_binary(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
