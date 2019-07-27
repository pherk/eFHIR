-module(metadata).
-compile(export_all).
-include("primitives.hrl").
%%
%% API exports
%%
-record(relatedArtifact, {
      type :: binary()
    , display :: binary()
    , citation :: binary()
    , url :: binary()
    , document :: complex:attachment()
    , resource :: special:reference()
    }).
-opaque relatedArtifact() :: #relatedArtifact{}.

%%====================================================================
%% API functions
%%====================================================================

%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, A)).
-define(asrtpr(A, B), ?assertEqual(B, utils:rec_to_prop(A))).

-endif.

