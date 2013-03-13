-module(simulator).
-export([start/2, loop/3]).

-include("schedule.hrl").

start(Name, Class) ->
    io:format("~w: simulator::start ~w ~n",[self(), Name]),
    #schedule_entry{name = Name,
		    simulator = spawn(simulator, loop,
				      [Class:start(), null, 0]),
		    time = infinity}.

loop(Model, Coordinator, Status) ->
    io:format("~w: simulator::loop ~n",[self()]),
    receive
	{init, Coordinator2} ->
	    Model ! {self(), init},
	    loop(Model, Coordinator2, idle);
	{internal, Time} ->
	    Model ! {self(), internal, Time},
	    loop(Model, Coordinator, internal);
	{external, Time, Events} ->
	    Model ! {self(), external,Time,Events},
	    loop(Model, Coordinator, external);
	{output, Time} ->
	    Model ! {self(), output, Time},
	    loop(Model, Coordinator, Status);
	done_internal ->
	    Model ! {self(), ta},
	    loop(Model, Coordinator, Status);
	{done_ta, Time} ->
	    Coordinator ! {self(), done_ta, Time, Status},
	    loop(Model, Coordinator, idle);
	{done_init, Begin} ->
	    Coordinator ! {self(), done_init, Begin},
	    loop(Model, Coordinator, idle);
	done_external ->
	    Model ! {self(),ta},
	    loop(Model, Coordinator, Status);
	{done_output, Events} ->
	    Coordinator ! {self(), done_output, Events},
	    loop(Model, Coordinator, Status)
    end.
