-module(root_coordinator).
-export([start/2,loop/1]).

%root_coordinator:start({[{m1,model1},{m2,model2}],[{{m1,out},{m2,in}}]},10).

start(G,Duration) ->
    spawn(root_coordinator, loop, [{null,G,Duration}]).

% si le process coordinator n'est pas démarré alors il est lancé
loop({null,G,Duration}) ->
%    io:format("~w: root_coordinator::init ~n", [self()]),
    Coordinator = coordinator:start(self(),G,Duration),
    Coordinator ! init,
    loop({Coordinator,G,Duration});
% boucle générale :
%  - envoi du message next_event au coordinator
%  - attend l'acquittement de fin de traitement ou de fin de simulation
loop({Coordinator,G,Duration}) ->
%    io:format("root_coordinator::loop ~n",[]),
    receive
	done_init ->
%	    io:format("root_coordinator::done_init ~n", []),
	    Coordinator ! next_event,
	    loop({Coordinator,G,Duration});
	done ->
%	    io:format("root_coordinator::done ~n", []),
	    Coordinator ! next_event,
	    loop({Coordinator,G,Duration});
	finish ->
%	    io:format("root_coordinator::finish ~n", []),
	    true
    end.
