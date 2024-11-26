-module(product_dist_task1).
-export([start/3, start_trucks/2, start_conveyors/1, conveyor_belt/2, truck/3]).

%% == Task 1 ==

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

    Packages = create_packages(NumTrucks * TruckCapacity),

    % main loop
    loop(ConveyorPids, TruckPids, Packages).

%%% ========================
%%% Main Loop
%%% ========================

loop(Conveyors, Trucks, Packages) -> 
    if
        Packages =:= [] orelse Trucks =:= [] ->
            finish(Conveyors, Trucks);
        true ->
            receive
                {new_package, ConveyorId} ->
                    %% New package created
                    [Size | UpdatedPackages] = Packages,
                    io:format("Conveyor ~p: Created package of size ~p~n", [ConveyorId, Size]),
                    assign_packages(Size, Trucks),
                    loop(Conveyors, Trucks, UpdatedPackages);

                {truck_full, TruckId, TruckRest} ->
                    %% Truck full notification
                    io:format("Truck ~p: is full.~n", [TruckId]),
                    loop(Conveyors, TruckRest, Packages)
            end
    end.

finish(Conveyors, Trucks) ->
    %% Shut down conveyor belt processes
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Conveyors),

    %% Shut down truck processes
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Trucks),

    io:format("All packages loaded onto trucks.~n"),
    init:stop().

%%% ========================
%%% Packages
%%% ========================

create_packages(NumPackages) ->
    lists:map(fun(_) ->
        1
    end, lists:seq(1, NumPackages)).

%%% ========================
%%% Conveyor Belts
%%% ========================

% Spawns conveyor belt processes.
start_conveyors(NumBelts) ->
    lists:map(fun(N) ->
        spawn(?MODULE, conveyor_belt, [N, self()]) % N will be the id of the conveyor belt.
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
        spawn(?MODULE, truck, [N, self(), TruckCapacity])
    end, lists:seq(1, NumTrucks)).

truck(Id, MainPid, Capacity) ->
    receive
        {load_package, Size, TruckRest} when Size =< Capacity ->
            io:format("Truck ~p: Loaded package of size ~p. Remaining capacity: ~p~n", [Id, Size, Capacity - Size]),
            truck(Id, MainPid, Capacity - Size),
            if Size >= Capacity ->
                %% Notify when truck is full
                MainPid ! {truck_full, Id, TruckRest},
                exit(normal)
            end;

        {load_package, _Size, TruckRest} ->
            MainPid ! {truck_full, Id, TruckRest},
            exit(normal)
    end.

%%% ========================
%%% Package Assignment
%%% ========================

assign_packages(_, []) -> 
    [];

assign_packages(Size, [TruckPid | TruckRest]) ->
    TruckPid ! {load_package, Size, TruckRest}.

