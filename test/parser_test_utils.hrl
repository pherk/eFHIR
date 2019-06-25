%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Peter Herkenrath.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-include("fhirpath_ddl.hrl").

-compile([export_all]).

-define(fpl_parser_assert(String, Expected),
        {ok,Toks,_} = fpl_lexer:string(String),
        {ok, Got} = fpl_parser:parse(Toks),
        ?assertEqual(Expected, Got)).

-define(fpl_parser_assert_match(String, Type, Expected),
        {ok,Toks,_} = fpl_lexer:string(String),
        {Type, Got} = fpl_parser:parse(Toks),
        lists:foreach(
          fun({Key, Value}) -> ?assertEqual(Value, proplists:get_value(Key, Got)) end,
          Expected)).

-define(fpl_parser_fail(String),
        {ok,Toks,_} = fpl_lexer:string(String),
        Got = fpl_parser:parse(Toks),
        ?assertMatch({error, _}, Got)).
