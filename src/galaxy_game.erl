%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2, planet/0]).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    [spawn_planet(Planet) || Planet <- Planets],
    [activate_shield(Planet) || Planet <- Shields],
    [setup_alliance(Alliance) || Alliance <- Alliances],

    [ready_for_battle(Planet) || Planet <- Planets],
    ok.

spawn_planet(Planet) ->
    Pid = spawn(?MODULE, planet, []),
    register(Planet, Pid).

activate_shield(Planet) ->
    Planet ! {shield, true}.

setup_alliance({Planet1, Planet2}) ->
    Planet1 ! {link_to, Planet2}.

ready_for_battle(Planet) ->
    Planet ! {ready, self()},
    receive
        im_ready ->
            ok
    after
        1000 ->
            timeout
    end.

%% Planet process
planet() ->
    receive
        {shield, Activated} ->
            process_flag(trap_exit, Activated),
            planet();
        {link_to, Planet} ->
            link(whereis(Planet)),
            planet();
        {ready, From} ->
            From ! im_ready,
            planet();
        {'EXIT', From, die} ->
            From ! im_shielded,
            planet();
        _ ->
            planet()
    end.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    [kill_planet(Planet, kill) || Planet <- Planets],
    ok.

kill_planet(Planet, Reason) ->
    Pid = whereis(Planet),
    case Pid of
        undefined ->
            ok;
        _ ->
            Ref = monitor(process, Pid),
            exit(Pid, Reason),
            receive
                {'DOWN', Ref, process, Pid, _} ->
                    ok;
                im_shielded ->
                    ok
            after
                1000 ->
                    timeout
            end
    end.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    [omgLaz0rShark_planet(Action) || Action <- Actions],
    lists:filter(fun(Planet) ->
        Pid = whereis(Planet),
        case Pid of
            undefined ->
                false;
            _ ->
                is_process_alive(Pid)
        end
    end, Planets).

omgLaz0rShark_planet({laser, Planet}) ->
    kill_planet(Planet, die);
omgLaz0rShark_planet({nuclear, Planet}) ->
    kill_planet(Planet, kill).
