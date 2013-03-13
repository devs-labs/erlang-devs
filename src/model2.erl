-module(model2).
-export([start/0,loop/1,init/0,ta/1,internal/2,external/3]).

start() ->
    io:format("model2::start ~n",[]),
    spawn(model2,loop,[null]).

init() ->
    io:format("~w: model2: init ~n",[self()]),
    {0,0}.

ta(_) ->
    io:format("~w: model2: ta = infinity~n",[self()]),
    infinity.

internal(State,Time) ->
    io:format("~w: model2: internal at ~w ~n",[self(),Time]),
    State + 1.

external(State,Time,Events) ->
    io:format("~w: model2: external at ~w ~w ~n",[self(),Time,Events]),
    State.

output(_,Time) ->
    io:format("~w: model2: output at ~w ~n",[self(),Time]),
    [].

loop(State) ->
%    io:format("~w: model2::loop ~n",[self()]),
    receive
	{From, init} ->
	    {Begin, NewState} = init(),
	    From ! {done_init, Begin},
	    loop(NewState);
	{From, internal, Time} ->
	    NewState = internal(State, Time),
	    From ! done_internal,
	    loop(NewState);
	{From, external, Time, Events} ->
	    NewState = external(State, Time, Events),
	    From ! done_external,
	    loop(NewState);
% calcul de la fonction output et envoi du message done_output au coordinator
% avec la liste des événements générés (port + événement)
	{From, output, Time} ->
	    Events = output(State, Time),
	    From ! {done_output, Events},
	    loop(State);
	{From, ta} ->
	    From ! {done_ta, ta(State)},
	    loop(State)
    end.
