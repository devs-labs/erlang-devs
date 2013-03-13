-module(coordinator).
-export([start/3, loop/4]).

-import(schedule, [schedule/2]).

-include("schedule.hrl").

% démarrage des simulateurs contenus dans Models et ordonnancement
start(Father, {Models, Graph}, Duration) ->
    spawn(coordinator, loop, [Father, start_models(Models), Graph, Duration]).

% démarrage d'un simulateur
% simulator:start retourne une liste composée de son nom, de son PID et
% de sa date de réveil
start_models([]) -> [];
start_models([{Name, Class} | T]) -> schedule(simulator:start(Name, Class),
					      start_models(T)).

% calcul des (modèle,port) connecté au port de sortie du modèle considéré
% en retour : [{model_1,port_1},...{model_n,port_n}]
target2(_ModelName, _Port, []) -> [];
target2(ModelName, Port, [{{ModelName, Port}, {InModel, InPort}} | T]) ->
    [{InModel, InPort} | target2(ModelName, Port, T)];
target2(ModelName, Port, [_ | T]) ->
    target2(ModelName, Port, T).

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
send(OtherSimulators, Time, Event, [{InModel, InPort} | T]) ->
%    io:format("~w: coordinator::send ~w ~n",[self(),OtherSimulators]),
    send2(OtherSimulators, Time, Event, {InModel, InPort}),
    send(OtherSimulators, Time, Event, T).

% envoi de l'événement à l'ensemble des modèles connectés au port de sortie
% où l'événement a été déposé
target(Name, Port, Time, Event, OtherSimulators, Graph) ->
    send(OtherSimulators, Time, Event, target2(Name, Port, Graph)).

% distribution des événements
% la liste des événements est vide -> rien à faire
dispatch_event(_From, _OtherSimulators, _Graph, []) -> true;
% on traite le premier événement de la liste des bags à envoyer
dispatch_event({Name, Simulator, Time}, OtherSimulators, Graph,
	       [{Port, Event} | T]) ->
%    io:format("~w: coordinator::dispatch_event: ~w ~w ~w ~n",
%	      [self(), Name, Simulator, [{Port , Event} | T]]),
    target(Name, Port, Time, Event, OtherSimulators, Graph),
    dispatch_event({Name, Simulator, Time}, OtherSimulators, Graph, T).

% distribution des événements reçu suite à un output d'un simulateur
% la liste des événements est vide -> on ne fait rien
dispatch_events(_From, _Simultors, _OtherSimulators, _Graph, []) -> true;
% on traite
dispatch_events(From, [{Name, From, Time} | T], OtherSimulators, Graph,
		Events) ->
%    io:format("~w: coordinator::dispatch_events: ~w ~w ~w ~n",[self(),Events,OtherSimulators,T]),
    dispatch_event({Name, From, Time},
		   lists:concat(OtherSimulators, T), Graph, Events);
% on cherche le simulateur
dispatch_events(From, [Simulator | T], OtherSimulators, Graph, Events) ->
%    io:format("~w: coordinator::dispatch_events: ~w ~w ~n",[self(),Events,OtherSimulators]),
    dispatch_events(From, T, [Simulator | OtherSimulators], Graph, Events).

% envoi du message output au simulateur en tête de la liste
next_event([ScheduleEntry | _], _) ->
%    io:format("~w: coordinator::next_event ~w ~n",[self(),Name]),
%    io:format("~w: coordinator::send output to ~w ~n",[self(),Simulator]),
    ScheduleEntry#schedule_entry.simulator !
	{output, ScheduleEntry#schedule_entry.time}.

init([]) -> true;
init([ScheduleEntry | T]) ->
%    io:format("~w: coordinator::send init ~n",[self()]),
    ScheduleEntry#schedule_entry.simulator ! {init, self()},
    init(T).

done(_From, Father, [ScheduleEntry | T], U, Graph, infinity,
     Duration, Status) ->
%    io:format("~w: coordinator::done_ta: infinity ~n",[self()]),
    if
	Status == 1 ->
	    Father ! done;
	true -> true
    end,
    loop(Father, schedule(schedule:update_time(ScheduleEntry, infinity),
			  lists:append(T, U)), Graph, Duration);
done(From, Father, [ScheduleEntry | T], U, Graph, Ta, Duration, Status) ->
%    io:format("~w: coordinator::done_ta: ~w ~n",[self(),Ta]),
    if
	ScheduleEntry#schedule_entry.simulator == From ->
	    if
		Status == 1 ->
		    Father ! done;
		true -> true
	    end,
	    loop(Father,
		 schedule(
		   schedule:update_time(ScheduleEntry,
					ScheduleEntry#schedule_entry.time + Ta),
		   lists:append(T, U)), Graph, Duration);
	true ->
	    done(From, Father, T, [ScheduleEntry | U], Graph, Ta, Duration,
		 Status)
    end.

% vérifie si tous les modèles ont donnés leur date de début
done_init2(Father, [], Models, Graph, Duration) ->
    Father ! done_init,
    loop(Father, Models, Graph, Duration);
done_init2(Father ,[ScheduleEntry | T], Models, Graph, Duration) ->
    if
	ScheduleEntry#schedule_entry.time == infinity ->
	    loop(Father, Models, Graph, Duration);
	true -> done_init2(Father, T, Models, Graph, Duration)
    end.

done_init(Father, Models, Graph, Duration, From, Begin) ->
    Models2 = init_time(Models, From, Begin),
    done_init2(Father, Models2, Models2, Graph, Duration).

% mise à jour de la date d'activation du modèle à l'init
init_time([ScheduleEntry | T], From, Begin) ->
    if
	ScheduleEntry#schedule_entry.simulator == From ->
	    [schedule:update_time(ScheduleEntry, Begin) | T];
	true -> [ScheduleEntry | init_time(T, From, Begin)]
    end.

% boucle générale du coordinateur du modèle couplé
% en entrée :
%  - process père
%  - liste des simulateurs (un simulateur = nom, process, date)
%  - graphe de connexions
% la liste des simulateurs est vide -> fin de la simulation
loop(Father, [], _Graph, _Duration) ->
    Father ! finish,
    true;
% on réveille le premier simulateur de la liste sur la réception
% du message next_event du root_coordinator
% puis attente de réception de done_ta ou done_output de la part du simulateur
loop(Father, [ScheduleEntry | T], Graph, Duration) ->
    io:format("~w: coordinator::loop ~w ~n", [self(),
					      [ScheduleEntry | T]]),
    receive
	init ->
	    io:format("~w: coordinator::init ~n",[self()]),
	    init([ScheduleEntry | T]),
	    loop(Father, [ScheduleEntry | T], Graph, Duration);
	next_event ->
	    io:format("~w: coordinator::next_event ~n",[self()]),
	    if
		(ScheduleEntry#schedule_entry.time > Duration) and
		(ScheduleEntry#schedule_entry.time > infinity) ->
		    loop(Father, [], Graph, Duration);
		true ->
		    next_event([ScheduleEntry | T], Graph),
		    loop(Father, [ScheduleEntry | T], Graph, Duration)
	    end;
	{From, done_init, Begin} ->
	    io:format("~w: coordinator::done init from ~w at ~w~n",
		      [self(), From, Begin]),
	    done_init(Father, [ScheduleEntry | T], Graph, Duration,
		      From, Begin);
	{From, done_ta, Ta, Status} ->
	    io:format("~w: coordinator::done_ta: ~w ~w ~n",[self(),From,Ta]),
	    done(From, Father, [ScheduleEntry | T],[], Graph,
		 Ta, Duration, Status);
	{From, done_output, Events} ->
	    io:format("~w: coordinator::done_output: ~w ~n",[self(),Events]),
	    dispatch_events(From, [ScheduleEntry | T], [], Graph,
			    Events),
	    ScheduleEntry#schedule_entry.simulator !
		{internal, ScheduleEntry#schedule_entry.time},
	    loop(Father, [ScheduleEntry | T], Graph, Duration)
    end.
