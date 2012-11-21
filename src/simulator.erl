-module(simulator).
-export([start/2,loop/3]).

% status = 0 (idle), 1 (internal), 2 (external)

start(Name,Class) ->
%    io:format("simulator::start ~w ~n",[Name]),
    {
      Name,
      spawn(simulator, loop, [Class:start(),null,0]),
      -1
    }.

loop(Model,Coordinator,Status) ->
%    io:format("~w: simulator::loop ~n",[self()]),
    receive
	{init, Coordinator2} ->
	    Model ! {self(), init},
	    loop(Model,Coordinator2,0);
	{internal, Time} ->
	    Model ! {self(),internal,Time},
	    loop(Model,Coordinator,1);
	{external, Time, Events} ->
	    Model ! {self(),external,Time,Events},
	    loop(Model,Coordinator,2);
	{output, Time} ->
	    Model ! {self(),output,Time},
	    loop(Model,Coordinator,Status);
	done_internal ->
	    Model ! {self(),ta},
	    loop(Model,Coordinator,Status);
	{done_ta, Time} ->
	    Coordinator ! {self(),done_ta,Time,Status},
	    loop(Model,Coordinator,0);
	{done_init, Time} ->
	    Coordinator ! {self(),done_init,Time},
	    loop(Model,Coordinator,0);
	done_external ->
	    Model ! {self(),ta},
	    loop(Model,Coordinator,Status);
	{done_output, Events} ->
	    Coordinator ! {self(),done_output,Events},
	    loop(Model,Coordinator,Status)
    end.
