-module(product_dist_task3).
-export([start/3, start_trucks/2, start_conveyors/1, conveyor_belt/2, truck/3]).

%% == Task 2 ==

%%% ========================
%%% Entry Point
%%% ========================

start(NumBelts, NumTrucks, TruckCapacity) -> 
    io:format("== Product Distribution System ==~n"),
    io:format("Number of belts: ~p~n", [NumBelts]),
    io:format("Number of trucks: ~p~n", [NumTrucks]),
    io:format("Truck capacity: ~p~n", [TruckCapacity]),

    ConveyorPids = start_conveyors(NumBelts),

    TruckPids = start_trucks(NumTrucks, TruckCapacity),

    Packages = create_packages(NumTrucks * 5),

    % main loop
    loop(ConveyorPids, TruckPids, Packages).

%%% ========================
%%% Main Loop
%%% ========================

finish(Conveyors, Trucks) ->
    %% Shut down conveyor belt processes
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Conveyors),
    %% Shut down truck processes
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Trucks).

loop(Conveyors, Trucks, Packages) -> 
    if
        Packages =:= [] ->
            finish(Conveyors, Trucks),
            io:format("All packages loaded onto trucks.~n"),
            init:stop();
        Trucks =:= [] ->
            finish(Conveyors, Trucks),
            io:format("All trucks are full!~nRemaining package sizes: ~p~n", [Packages]),
            init:stop();
        true ->
            receive
                {new_package, ConveyorId} ->
                    %% New package created
                    [Size | UpdatedPackages] = Packages,
                    io:format("Conveyor ~p: Created package of size ~p~n", [ConveyorId, Size]),
                    assign_packages(Size, Trucks),
                    loop(Conveyors, Trucks, UpdatedPackages);

                {truck_full, TruckId, TruckRest, Size} ->
                    %% Truck full notification
                    io:format("Truck ~p: is full.~n Package with size ~p is being sent back.~n", [TruckId, Size]),
                    loop(Conveyors, TruckRest, [Size | Packages]) % add the package back
            end
    end.


%%% ========================
%%% Packages
%%% ========================

create_packages(NumPackages) ->
    lists:map(fun(_) ->
        rand:uniform(20)
    end, lists:seq(1, NumPackages)).

%%% ========================
%%% Conveyor Belts
%%% ========================

% Spawns conveyor belt processes.
start_conveyors(NumBelts) ->
    lists:map(fun(N) ->
        spawn(?MODULE, conveyor_belt, [N, self()])
    end, lists:seq(1, NumBelts)).

conveyor_belt(Id, MainPid) ->

    MainPid ! {new_package, Id}, 

    timer:sleep(500),
    conveyor_belt(Id, MainPid).

%%% ========================
%%% Trucks
%%% ========================

% Spawns truck processes.
start_trucks(NumTrucks, TruckCapacity) ->
    lists:map(fun(N) ->
        spawn(?MODULE, truck, [N, self(), TruckCapacity, -1])
    end, lists:seq(1, NumTrucks)).

truck(Id, MainPid, Capacity, CurrentBeltId) ->
    receive
        {load_package, Size, TruckRest} when Size =< Capacity ->
            io:format("Truck ~p: Loaded package of size ~p. Remaining capacity: ~p~n", [Id, Size, Capacity - Size]),
            truck(Id, MainPid, Capacity - Size),
            if Size >= Capacity ->
                %% Notify when truck is full
                MainPid ! {truck_full, Id, TruckRest, Size},
                exit(normal)
            end;

        {load_package, _Size, TruckRest} ->
            MainPid ! {truck_full, Id, TruckRest, _Size},
            exit(normal)
    end.

%%% ========================
%%% Package Assignment
%%% ========================

assign_packages(_, []) -> 
    [];

assign_packages(Size, [TruckPid | TruckRest]) ->
    TruckPid ! {load_package, Size, TruckRest}.

