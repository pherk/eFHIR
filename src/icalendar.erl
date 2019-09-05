-module(icalendar).
-author("pmh").
-vsn("0.0.1").

-include("ecal.hrl").
-include_lib("fhir/include/fhir.hrl").
-include_lib("fhir/include/primitives.hrl").

-export([
          new_agenda/3
        ]).
-export([
          new_event/5
        , to_event/1
        , add_rrule/4
        , add_rdates/2
        , add_exdates/2
        , check_weekdays/2
        ]).
-export([
          new_schedule/0
        ]).

-export_type([agenda/0]).
-export_type([event/0]).

%%%
%%% Agendas
%%
-record('Schedule.Agenda', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , period     :: complex:'Period'()
    , blocking   :: boolean()
    , overbookable :: boolean()
    , parallelPerHour :: parallelPerHour()
    , note        :: string() | undefined
    , event  = [] :: ['Schedule.Agenda.Event'()]
}).
-opaque 'Schedule.Agenda'() :: #'Schedule.Agenda'{}.


-type freq()     :: daily | weekly | monthly | yearly | byeaster.
-type status()   :: open | confirmed | tentative | cancelled.
-type duration() :: 15 | 30 | 60 | 90 | 120.
-type priority() :: pre | post | equal.
-type overbookable() :: boolean().
-type parallelPerHour() :: string().

-type transparency() :: opaque | transparent.
-type visability()   :: default | public | private.
-type response()     :: needs_action | declined | tentative | accepted.
-type orderby()      :: start | updated.
-type access_role()  :: none | freebusy_read | read | write | owner.
-type type()   :: official | traditional | religious_rk | worktime | service | meeting.
-type rdate()  :: [interval()].
-type exdate() :: [interval()].
-type byweek() :: none | odd | even | weekno().
-type byday()  :: {nth, -52..366} | {wd, weekday()}. %% 0: means all weekdays in period

-record('Schedule.Agenda.Event.RRule', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , frequency :: freq()
    , byweekno  :: byweek()
    , byday     :: [byday()] 
    , count     :: non_neg_integer()
    }).
-opaque 'Schedule.Agenda.Event.RRule'() :: #'Schedule.Agenda.Event.RRule'{}.

-record('Schedule.Agenda.Event.RDate', {
    }).
-opaque 'Schedule.Agenda.Event.RDate'() :: #'Schedule.Agenda.Event.RDate'{}.

-record('Schedule.Agenda.Event.ExDate', {
    }).
-opaque 'Schedule.Agenda.Event.ExDate'() :: #'Schedule.Agenda.Event.ExDate'{}.

-record('Schedule.Agenda.Event', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , name                  :: string()
    , description           :: string() | undefined
    , type                  :: type()
    , location              :: special:'Reference'() | undefined
    , period                :: complex:'Period'()
    , note                  :: string() | undefined
    , rrule                 :: 'Schedule.Agenda.Event.RRule'() | undefined
    , rdate       = []      :: ['Schedule.Agenda.Event.RDate'()] 
    , exdate      = []      :: ['Schedule.Agenda.Event.ExDate'()] 
    }).
-opaque 'Schedule.Agenda.Event'() :: #'Schedule.Agenda.Event'{}.

-record('Icalendar.Schedule', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , global      :: special:'Reference'()
    , timing = #'Schedule.Timing'{overbookable = true} :: 'Schedule.Timing'()
    , agenda = [] :: ['Schedule.Agenda'()]
    }).
-opaque schedule() :: #schedule{}.

-record('Schedule.Timing', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , pre          = 15   :: duration() 
    , exam         = 30   :: duration()
    , post         = 15   :: duration()
    , overbookable = 0    :: overbookable() | undefined
    , parallelPerHour :: string()  | undefined
    , blocking        :: boolean() | undefined
    , prio         = post :: priority()
    }).
-type 'Schedule.Timing'() :: #'Schedule.Timing'{}.

-record('Schedule.CSS', {
      anyAttribs :: anyAttribs()
    , id         :: id()
    , extension  :: [extensions:'Extension'()]
    , className        = <<"service">> :: string()
    , backgroundColor  = <<"#ffffff">> :: string()
    , textColor        = <<"#ffffff">> :: string()
    , editable         = false       :: boolean()
    }).
-type 'Schedule.CSS'() :: #'Schedule.CSS'{}.

-record('Schedule', {
      anyAttribs :: anyAttribs()
    , id          :: id()
    , meta        :: complex:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language    :: code() | undefined
    , text        :: special:'Narrative'() | undefined
    , contained   :: [resource:'ResourceContainer'()]
    , extension   :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier'  :: [complex:'Identifier'()]
    , active      = true :: boolean()
    , type        :: string()
    , name        :: string()
    , description :: string() | undefined
    , fc          :: 'Schedule.CSS'() 
    , ff          = true :: boolean()
    , timing      = #'Schedule.Timing'{overbookable = true} :: 'Schedule.Timing'()
    , location    :: special:'Reference'() | undefined
    , agenda      :: ['Schedule.Agenda'()]
    }).
-type 'Schedule'() :: #'Schedule'{}.

-type cu_type()    :: person | group | resource.
-type cal_type()   :: primary | secondary.

-record('ICalendar', {
      anayAttribs :: anyAtrribs()
    , id          :: string()
    , meta        :: complex:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language    :: code() | undefined
    , text        :: special'Narrative'() | undefined
    , contained   :: [resource:'ResourceContainer'()]
    , extension   :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , active      = true    :: boolean()
    , owner                 :: special:'Reference'()
    , cutype                :: complex:'Coding'()
    , caltype     = primary :: complex:'Coding'()
    , summary               :: string() | undefined
    , description           :: string() | undefined
    , timezone    = mez     :: timezone()
    , location              :: special:'Reference'() | undefined
    , schedule    = []      :: ['ICalendar.Schedule'()]
    }).
-opaque 'ICalendar'() :: #'ICalendar'{}.


%%
%% Agenda API
%%
-spec new_agenda(complex:'Period'(), boolean(), string(),[event()]) -> agenda().
new_agenda(Period,Note,Events) -> 
    #'Scheule.Agenda'{
    , period   = Period
    , blocking = true
    , note     = Note
    , event    = Events
    }.

%%
%% Event API
%%

-spec new_event(string(),string(),type()) -> event().
new_event(Name,Description,Type) ->
    #event{
        name = Name
      , description = Description
      , type = Type
    }.

-spec add_rrule(event(), freq(),event_byweek(),[byday()]) -> event().
add_rrule(Event, Freq, Byweekno, Bydays) -> 
    Ret = check_bys(Freq,Byweekno,Bydays),
    case Ret of
      {error,_}  -> {error, #rrule{frequency=Freq,byweekno=Byweekno,byday=Bydays}};
      {ok,Byday} -> Rrule = #rrule{frequency=Freq,byweekno=Byweekno,byday=Byday},
                    Event#event{rrule=Rrule}
    end.

-spec add_rdates(event(), [rdate()]) -> event().
add_rdates(Event, Rdates) -> Event#event{rdate=Rdates}.

-spec add_exdates(event(), [exdate()]) -> event().
add_exdates(Event, Exdates) -> Event#event{exdate=Exdates}.

-spec check_bys(freq(),event_byweek(),[byday()]) -> ({ok,[byday()]}|{error,atom()}).
check_bys(daily,  none,Weekdays) -> check_weekdays(daily,Weekdays);
check_bys(daily,   odd,   _) -> {error, rrule_illegal};
check_bys(daily,  even,   _) -> {error, rrule_illegal};
check_bys(daily,     _,   _) -> {error, rrule_illegal};
check_bys(weekly,  odd,Weekdays)  -> check_weekdays(weekly,Weekdays);
check_bys(weekly, even,Weekdays)  -> check_weekdays(weekly,Weekdays);
check_bys(weekly,     _,  _) -> {error, rrule_illegal};
check_bys(monthly,  odd, _)  -> {error, rrule_illegal};
check_bys(monhtly, even, _)  -> {error, rrule_illegal};
check_bys(monthly,     _, _) -> {error, rrule_illegal};
check_bys(yearly,  none,Weekdays) -> check_weekdays(yearly,Weekdays);
check_bys(yearly,   odd,  _) -> {error, rrule_illegal};
check_bys(yearly,  even,  _) -> {error, rrule_illegal};
check_bys(yearly,Weekno,Weekdays) when is_integer(Weekno), Weekno >= 0, Weekno < 53 ->
    check_weekdays(yearly,Weekdays);
check_bys(byeaster, none, Weekdays) -> check_nth_day(hd(Weekdays),-52,60);
check_bys(_,_,_) -> {error, rrule_illegal}.

-spec check_weekdays(freq(),[byday()]) -> {ok,[byday()]} | {error,atom()}.
check_weekdays(daily,  Weekdays) -> check_single_days(Weekdays, []);
check_weekdays(weekly, Weekdays) -> check_single_days(Weekdays, []);
check_weekdays(monthly,Days) -> check_nth_days(Days,1,5, []);
check_weekdays(yearly, Days) -> check_nth_days(Days,1,366, []).

-spec check_single_days([string()],[byday()]) -> {ok,[byday()]} | {error,atom()}.
check_single_days([], Acc) -> {ok,Acc};
check_single_days([Day|T], Acc) ->
    Ret = ecal_date:name_to_weekday(Day),
    case Ret of
       {ok,WD} -> Acc1 = [{0,WD}|Acc],
                  check_single_days(T,Acc1);
       _       -> Ret
    end.

-spec check_nth_days([string()],integer(),integer(),[byday()]) -> {ok,[byday()]} | {error,atom()}.
check_nth_days(     [],    _,   _,Acc) -> {ok,Acc};
check_nth_days([Day|T],First,Last,Acc) -> 
    Ret = check_nth_day(Day,First,Last),
    case Ret of
      {ok,ND} -> Acc1 = [ND|Acc],
                 check_nth_days(T,First,Last,Acc1);
      _       -> Ret
    end. 
    

-spec check_nth_day(byday(),integer(),integer()) -> {ok,byday()} | {error,atom()}.
check_nth_day(Day,First,Last) -> 
     case in_range(Day#byday.nth,First,Last) of
       true  -> {ok,Day};
       false -> {error, nth_day_not_in_range}
     end.

-spec in_range(integer(),integer(),integer()) -> boolean().
in_range(I,Min,Max) ->
    (I >= Min) and (I =< Max).

%%
%% Schedule API
%%
to_schedule({Props}) -> to_schedule(Props);
to_schedule(Props) ->
    DT = decode:xsd_info(<<"Schedule">>),
    #'Schedule'{
         anayAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , meta = decode:value(<<"meta">>, Props, DT)
       , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
       , language = decode:value(<<"language">>, Props, DT),
       , text = decode:value(<<"text">>, Props, DT)
       , contained = decode:value(<<"contained">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
       , 'identifier' = decode:value(<<"identifier">>, Props, DT)
       , active = decode:value(<<"active">>, Props, DT)
       , type = decode:value(<<"type">>, Props, DT)
       , name = decode:value(<<"name">>, Props, DT)
       , description = decode:value(<<"description">>, Props, DT)
       , fc = decode:value(<<"fc">>, Props, DT)
       , ff = decode:value(<<"ff">>, Props, DT)
       , timing = decode:value(<<"timing">>, Props, DT)
       , location = decode:value(<<"location">>, Props, DT)
       , agenda = decode:value(<<"agenda">>, Props, DT)
    }.

to_schedule_agenda({Props}) -> to_schedule_agenda(Props);
to_schedule_agenda(Props) -> 
    DT = decode:xsd_info(<<"Schedule.Agenda">>, DT),
    #'Schedule.Agenda'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , period = decode:value(<<"period">>, Props, DT)
       , blocking = decode:value(<<"blocking">>, Props, DT)
       , overbookable = decode:value(<<"overbookable">>, Props, DT)
       , parallelPerHour = decode:value(<<"parallelPerHour">>, Props, DT)
       , note = decode:value(<<"note">>, Props, DT)
       , event = decode:value(<<"event">>, Props, DT)
    }.

to_schedule_agenda_event({Props}) -> to_schedule_agenda_event(Props);
to_schedule_agenda_event(Props) -> 
    DT = decode:xsd_info(<<"Schedule.Agenda.Event">>),
    #'Schedule.Agenda.Event'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , name = decode:value(<<"name">>, Props, DT)
       , description = decode:value(<<"description">>, Props, DT)
       , type = decode:value(<<"type">>, Props, DT)
       , location = decode:value(<<"location">>, Props, DT)
       , period = decode:value(<<"period">>, Props, DT)
       , note = decode:value(<<"note">>, Props, DT)
       , rrule = decode:value(<<"rrule">>, Props, DT)
       , rdate = decode:value(<<"rdate">>, Props, DT)
       , exdate = decode:value(<<"exdate">>, Props, DT)
    }.

to_schedule_agenda_event_rRule({Props}) -> to_schedule_agenda_event_rRule(Props);
to_schedule_agenda_event_rRule(Props) ->
    DT = decode:xsd_info(<<"Schedule.Agenda.Event.RRule">>),
    #'Schedule.Agenda.Event.RRule'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , frequency = decode:value(<<"frequency">>, Props, DT)
       , byweekno = decode:value(<<"byweekno">>, Props, DT)
       , byday = decode:value(<<"byday">>, Props, DT)
       , count = decode:value(<<"count">>, Props, DT)
    }.
to_schedule_cSS({Props}) -> to_schedule_cSS(Props);
to_schedule_cSS(Props)
    DT = decode:xsd_info(<<"Schedule.CSS">>),
    #'Schedule.CSS'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , className = decode:value(<<"className">>, Props, DT)
       , backgroundColor = decode:value(<<"backgroundColor">>, Props, DT)
       , textColor = decode:value(<<"textColor">>, Props, DT)
       , editable = decode:value(<<"editable">>, Props, DT)
    }.

to_schedule_timing({Props}) -> to_schedule_timing(Props);
to_schedule_timing(Props) ->
    DT = decode:xsd_info(<<"Schedule.Timing">>),
    #'Schedule.Timing'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , pre = decode:value(<<"pre">>, Props, DT)
       , exam = decode:value(<<"exam">>, Props, DT)
       , post = decode:value(<<"post">>, Props, DT)
       , overbookable = decode:value(<<"overbookable">>, Props, DT)
       , blocking = decode:value(<<"blocking">>, Props, DT)
       , overbookable = decode:value(<<"overbookable">>, Props, DT)
       , parallelPerHour = decode:value(<<"parallelPerHour">>, Props, DT)
       , prio = decode:value(<<"prio">>, Props, DT)
    }.


-spec new_schedule() -> schedule().
new_schedule() -> #schedule{}.

%%
%% ical API
%%
to_iCalendar({Props}) -> to_iCalendar(Props);
to_iCalendar(Props) ->
    DT = decode:xsd_info(<<"Schedule.CSS">>),
    #'ICalendar'{
         anayAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , meta = decode:value(<<"meta">>, Props, DT)
       , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
       , language = decode:value(<<"language">>, Props, DT),
       , text = decode:value(<<"text">>, Props, DT)
       , contained = decode:value(<<"contained">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
       , 'identifier'  = decode:value(<<"identifier">>, Props, DT)
       , active = decode:value(<<"active">>, Props, DT)
       , owner = decode:value(<<"owner">>, Props, DT)
       , cutype = decode:value(<<"cutype">>, Props, DT)
       , caltype = decode:value(<<"caltype">>, Props, DT)
       , summary = decode:value(<<"summary">>, Props, DT)
       , description = decode:value(<<"description">>, Props, DT)
       , timezone = decode:value(<<"timezone">>, Props, DT)
       , location = decode:value(<<"location">>, Props, DT)
       , schedule = decode:value(<<"schedule">>, Props, DT)
    }.

to_iCalendar_schedule({Props}) -> to_icalendar_schedule(Props);
to_iCalendar_schedule(Props) ->
    DT = decode:xsd_info(<<"ICalendar.Schedule">>),
    #'ICalendar.Schedule'{
         anyAttribs = decode:attrs(Props, DT)
       , id = decode:value(<<"id">>, Props, DT)
       , extension = decode:value(<<"extension">>, Props, DT)
       , global = decode:value(<<"global">>, Props, DT)
       , timing = decode:value(<<"timing">>, Props, DT)
       , agenda = decode:value(<<"agenda">>, Props, DT)
    }.

%%%
%%% Eunit tests
%%%
-ifdef(Test).


-endif.
