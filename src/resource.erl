-module(resource).
-include("primitives.hrl").

-export([text/1]).

-type resourceContainer() ::
      patient:patient().
%%      careplan()
%%    | careteam()
%%    | composition()
%%    | condition()
%%    | consent()
%%    | encounter()
%%    | episodeocare()
%%    | goal()
%%    | patient()
%%    | requestgroup()
%%    | task().


text(R) ->
    Type = element(1,R),
    apply(Type,text,[R]).

