-module(schedule).
-export([schedule/2, update_time/2]).

-include("schedule.hrl").

% insert une nouvelle entrée dans l'échéanchier (ordre croissant de date de
% réveil)
schedule(NewEntry, []) ->
    [NewEntry];

schedule(NewEntry, [Entry | T]) ->
    if NewEntry#schedule_entry.time == infinity ->
	    lists:append([Entry | T], [NewEntry]);
       NewEntry#schedule_entry.time < Entry#schedule_entry.time ->
	    [NewEntry, Entry | T];
       Entry#schedule_entry.time == infinity -> [NewEntry, Entry | T];
       NewEntry#schedule_entry.time >= Entry#schedule_entry.time ->
	    [Entry | schedule(NewEntry, T)]
    end.

update_time(Entry, Duration) ->
    #schedule_entry{name = Entry#schedule_entry.name,
		    simulator = Entry#schedule_entry.simulator,
		    time = Duration}.
