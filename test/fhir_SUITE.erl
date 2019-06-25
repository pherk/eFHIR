-module(fhir_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).


suite() -> [{timetrap,{seconds,30}}].
all() -> [{group, samples}].
groups() -> [{samples, [], [universal]}].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_group(samples, Config) -> Config.
end_per_group(_, _) -> ok.

universal(Config) -> process(Config,"patient-21666.json").

process(Config,Name) ->
    FileName = ?config(data_dir,Config) ++ "json/" ++ Name,
    {ok,Bin} = file:read_file(FileName),
    {Source} = jiffy:decode(Bin),
    Rec = patient:to_patient(Source),
    ct:print("-> fhir ~p ~p", [Name,Source]),
    ct:print("-> fhir ~p", [Rec]),
    ok.
