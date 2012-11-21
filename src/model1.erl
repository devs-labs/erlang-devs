-module(model1).
-export([start/0,loop/1,init/0,ta/1,internal/2,external/3]).

start() ->
%    io:format("model1::start ~w ~n",[Name]),
    spawn(model1,loop,[null]).

init() ->
%    io:format("~w: model1: init ~n",[self()]),
    {2,0}.

ta(100) ->
%    io:format("~w: model1: ta = infinity~n",[self()]),
    -1;
ta(_) ->
%    io:format("~w: model1: ta = 1~n",[self()]),
    1.

internal(State,Time) ->
%    io:format("~w: model1: internal at ~w ~n",[self(),Time]),
    State + 1.

external(State,Time,Events) ->
%    io:format("~w: model1: external at ~w ~w ~n",[self(),Time,Events]),
    State.

output(_,Time) ->
%    io:format("~w: model1: output at ~w ~n",[self(),Time]),
    [{out,[]}].

loop(State) ->
%    io:format("~w: model1::loop ~n",[self()]),
    receive
	{From, init} ->
	    {Begin,NewState} = init(),
	    From ! {done_init,Begin},
	    loop(NewState);
	{From, internal, Time} ->
	    New_state = internal(State, Time),
	    From ! done_internal,
	    loop(New_state);
	{From, external, Time, Events} ->
	    New_state = external(State, Time, Events),
	    From ! done_external,
	    loop(New_state);
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
