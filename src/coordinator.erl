-module(coordinator).
-export([start/3,loop/4]).

% démarrage des simulateurs contenus dans Models et ordonnancement
start(Father,{Models,Graph},Duration) ->
    spawn(coordinator, loop, [Father,start2(Models),Graph,Duration]).

% démarrage d'un simulateur
% simulator:start retourne une liste composé de son nom, de son process et
% de sa date de réveil
start2([]) -> [];
start2([{Name,Class}|T]) -> schedule(simulator:start(Name,Class),
				     start2(T)).

% calcul des (modèle,port) connecté au port de sortie du modèle considéré
% en retour : [{model_1,port_1},...{model_n,port_n}]
target2(_,_,[]) -> [];
target2(Name,Port,[{{Name,Port},{InModel,InPort}}|T]) ->
    [{InModel,InPort}|target2(Name,Port,T)];
target2(Name,Port,[_|T]) ->
    target2(Name,Port,T).

% détermination du process Simulator
send2([],_,_,_) ->
    false;
send2([{Name,Simulator,_}|_],Time,Event,{Name,_}) ->
%    io:format("~w: coordinator::send ~w ~n",[self(),Name]),
    Simulator ! {external, Time, Event};
% le premier simulateur de la liste OtherSimulators n'est pas le destinaire
send2([_|T],Time,Event,X) ->
    send2(T,Time,Event,X).

% envoi de l'événement sur chacun des ports de sortie cible
send(_,_,_,[]) ->
    true;
send(OtherSimulators,Time,Event,[{InModel,InPort}|T]) ->
%    io:format("~w: coordinator::send ~w ~n",[self(),OtherSimulators]),
    send2(OtherSimulators,Time,Event,{InModel,InPort}),
    send(OtherSimulators,Time,Event,T).

% envoi de l'événement à l'ensemble des modèles connectés au port de sortie
% où l'événement a été déposé
target(Name,Port,Time,Event,OtherSimulators,Graph) ->
    send(OtherSimulators,Time,Event,target2(Name,Port,Graph)).

% distribution des événements
% la liste des événements est vide -> rien à faire
dispatch_event(_,_,_,[]) -> true;
% on traite le premier événement de la liste des bags à envoyer
dispatch_event({Name,Simulator,Time},OtherSimulators,Graph,[{Port,Event}|T]) ->
%    io:format("~w: coordinator::dispatch_event: ~w ~w ~w ~n",[self(),Name,Simulator,[{Port,Event}|T]]),
    target(Name,Port,Time,Event,OtherSimulators, Graph),
    dispatch_event({Name,Simulator,Time},OtherSimulators,Graph,T).

% concaténation de 2 listes

concat([],L) ->
    L;
concat([X|T],L) ->
    [X|concat(T,L)].

% distribution des événements reçu suite à un output d'un simulateur
% la liste des événements est vide -> on ne fait rien
dispatch_events(_,_,_,_,[]) -> true;
% on traite
dispatch_events(From,[{Name,From,Time}|T],OtherSimulators,Graph,Events) ->
%    io:format("~w: coordinator::dispatch_events: ~w ~w ~w ~n",[self(),Events,OtherSimulators,T]),
    dispatch_event({Name,From,Time},concat(OtherSimulators,T),Graph,Events);
% on cherche le simulateur
dispatch_events(From,[X|T],OtherSimulators,Graph,Events) ->
%    io:format("~w: coordinator::dispatch_events: ~w ~w ~n",[self(),Events,OtherSimulators]),
    dispatch_events(From,T,[X|OtherSimulators],Graph,Events).

% envoi du message output au simulateur en tête de la liste
next_event([{Name,Simulator,Time}|_],_) ->
%    io:format("~w: coordinator::next_event ~w ~n",[self(),Name]),
%    io:format("~w: coordinator::send output to ~w ~n",[self(),Simulator]),
    Simulator ! {output, Time}.

% tri les simulateurs par ordre croissant de date de réveil
schedule({Name,Simulator,Time},[]) ->
    [{Name,Simulator,Time}];
schedule({Name,Simulator,Time},[{Name2,Simulator2,Time2}|T]) ->
    if Time == -1 ->
	    concat([{Name2,Simulator2,Time2}|T],[{Name,Simulator,Time}]);
       Time < Time2 ->
	    [{Name,Simulator,Time},{Name2,Simulator2,Time2}|T];
       Time2 == -1 ->
	    [{Name,Simulator,Time},{Name2,Simulator2,Time2}|T];
       Time >= Time2 ->
	    [{Name2,Simulator2,Time2}|schedule({Name,Simulator,Time},T)]
    end.

init([],Father,Models,Graph,Duration) -> loop(Father,Models,Graph,Duration);
init([{_,Simulator,_}|T],Father,Models,Graph,Duration) ->
%    io:format("~w: coordinator::send init ~n",[self()]),
    Simulator ! {init,self()},
    init(T,Father,Models,Graph,Duration).

done(From,Father,[{Name,From,_}|T],U,Graph,-1,Duration,Status) ->
%    io:format("~w: coordinator::done_ta: -1 ~n",[self()]),
    if
	Status == 1 ->
	    Father ! done;
	true -> true
    end,
    loop(Father,schedule({Name,From,-1},concat(T,U)),Graph,Duration);
done(From,Father,[{Name,From,Time}|T],U,Graph,Ta,Duration,Status) ->
%    io:format("~w: coordinator::done_ta: ~w ~n",[self(),Ta]),
    if
	Status == 1 ->
	    Father ! done;
	true -> true
    end,
    loop(Father,schedule({Name,From,Time+Ta},concat(T,U)),Graph,Duration);
done(From,Father,[{Name,Simulator,Time}|T],U,Graph,Ta,Duration,Status) ->
    done(From,Father,T,[{Name,Simulator,Time}|U],Graph,Ta,Duration,Status).

% vérifie si tous les modèles ont donnés leur date de début
done_init2(Father,[],Models,Graph,Duration) ->
    Father ! done_init,
    loop(Father,Models,Graph,Duration);
done_init2(Father,[{_,_,Time}|T],Models,Graph,Duration) ->
    if
	Time == -1 -> loop(Father,Models,Graph,Duration);
	true -> done_init2(Father,T,Models,Graph,Duration)
    end.

done_init(Father,Models,Graph,Duration,From,Time2) ->
    Models2 = init_time(Models,From,Time2),
    done_init2(Father,Models2,Models2,Graph,Duration).

init_time([{Name,From,_}|T],From,Time2) ->
    [{Name,From,Time2}|T];
init_time([{Name,Simulator,Time}|T],From,Time2) ->
    [{Name,Simulator,Time}|init_time(T,From,Time2)].

% boucle générale du coordinateur du modèle couplé
% en entrée :
%  - process père
%  - liste des simulateurs (un simulateur = nom, process, date)
%  - graphe de connexions
% la liste des simulateurs est vide -> fin de la simulation
loop(Father,[],_,_) ->
    Father ! finish,
    true;
% on réveille le premier simulateur de la liste sur la réception
% du message next_event du root_coordinator
% puis attente de réception de done_ta ou done_output de la part du simulateur
loop(Father,[{Name,Simulator,Time}|T],Graph,Duration) ->
%    io:format("~w: coordinator::loop ~w ~n",[self(),[{Name,Simulator,Time}|T]]),
    receive
	init ->
%	    io:format("~w: coordinator::init ~n",[self()]),
	    init([{Name,Simulator,Time}|T],Father,[{Name,Simulator,Time}|T],Graph,Duration);
	next_event ->
%	    io:format("~w: coordinator::next_event ~n",[self()]),
	    if
		Time > Duration, Time > -1 -> loop(Father,[],Graph,Duration);
		true ->
		    next_event([{Name,Simulator,Time}|T],Graph),
		    loop(Father,[{Name,Simulator,Time}|T],Graph,Duration)
	    end;
	{From,done_init,Time2} ->
%	    io:format("~w: coordinator::done init from ~w ~n",[self(),From]),
	    done_init(Father,[{Name,Simulator,Time}|T],Graph,Duration,From,Time2);
	{From,done_ta,Ta,Status} ->
%	    io:format("~w: coordinator::done_ta: ~w ~w ~n",[self(),From,Ta]),
	    done(From,Father,[{Name,Simulator,Time}|T],[],Graph,Ta,Duration,Status);
% réception du message done_output avec la liste des événements
	{From,done_output,Events} ->
%	    io:format("~w: coordinator::done_output: ~w ~n",[self(),Events]),
	    dispatch_events(From,[{Name,Simulator,Time}|T], [], Graph, Events),
	    Simulator ! {internal, Time},
	    loop(Father,[{Name,Simulator,Time}|T],Graph,Duration)
    end.
