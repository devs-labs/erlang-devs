-module(root_coordinator).
-export([start/2,loop/1]).

%root_coordinator:start({[{m1,model1},{m2,model2}],[{{m1,out},{m2,in}}]},10).

start(Graph, Duration) ->
    spawn(root_coordinator, loop, [{Graph, Duration}]).

% si le coordinator du modèle couplé racine n'est pas démarré alors il est lancé
loop({Graph, Duration}) ->
    io:format("~w: root_coordinator::init ~n", [self()]),
    Coordinator = coordinator:start(self(), Graph, Duration),
    Coordinator ! init,
    loop(Coordinator);
% boucle générale :
%  - envoi du message next_event au coordinator
%  - attend l'acquittement de fin de traitement ou de fin de simulation
loop(Coordinator) ->
    io:format("~w: root_coordinator::loop ~n",[self()]),
    receive
	done_init ->
	    io:format("~w: root_coordinator::done_init ~n", [self()]),
	    Coordinator ! next_event,
	    loop(Coordinator);
	done ->
	    io:format("~w: root_coordinator::done ~n", [self()]),
	    Coordinator ! next_event,
	    loop(Coordinator);
	finish ->
	    io:format("~w: root_coordinator::finish ~n", [self()]),
	    true
    end.
