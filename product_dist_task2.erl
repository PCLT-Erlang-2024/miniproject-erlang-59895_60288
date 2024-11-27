-module(product_dist_task2).
-export([start/3, start_trucks/2, start_conveyors/3, conveyor_belt/4, truck/4, package_manager/2, truck_manager/1]).

%% == Task 2 ==

%%% ========================
%%% Entry Point
%%% ========================

start(NumBelts, NumTrucks, TruckCapacity) -> 
    io:format("== Product Distribution System ==~n"),
    io:format("Number of belts: ~p~n", [NumBelts]), 
    io:format("Number of trucks: ~p~n", [NumTrucks]), 
    io:format("Truck capacity: ~p~n", [TruckCapacity]), 

    TruckPids = start_trucks(NumTrucks, TruckCapacity),
    Packages = create_packages(NumTrucks * TruckCapacity),
    PackageManagerPid = spawn(?MODULE, package_manager, [Packages, self()]),
    TruckManagerPid = spawn(?MODULE, truck_manager, [TruckPids]),

    ConveyorPids = start_conveyors(NumBelts, PackageManagerPid, TruckManagerPid),

    Monitors = lists:map(fun(Pid) -> 
        monitor(process, Pid) 
    end, TruckPids ++ ConveyorPids),

    % Wait for all processes to terminate
    wait_for_termination(length(Monitors)),
    io:format("Distribution complete.~n").

wait_for_termination(0) -> ok;
wait_for_termination(RemainingProcesses) ->
    receive
        {'DOWN', _Ref, process, _Pid, normal} ->
            wait_for_termination(RemainingProcesses - 1);
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            io:format("Process terminated abnormally.~n"),
            wait_for_termination(RemainingProcesses - 1)
    end.


%%% ========================
%%% Packages
%%% ========================

package_manager([], MainPid) ->
    MainPid ! {no_more_packages},
    io:format("Package Manager: No more packages available.~n"), 
    exit(normal);

package_manager([Size | RestPackages], MainPid) ->
    receive
        {new_package, ConveyorId} ->
            io:format("= Package Manager: Assigning package of size ~p to Conveyor ~p.~n= Packages left ~p~n", [Size, ConveyorId, length(RestPackages)]),
            ConveyorId ! {assign_package, Size},
            package_manager(RestPackages, MainPid);
        {return_package, ReturnedSize} ->
            package_manager([ReturnedSize | [Size | RestPackages]], MainPid)
    end.


create_packages(NumPackages) ->
    lists:map(fun(_) ->
        rand:uniform(5)
    end, lists:seq(1, NumPackages)).

%%% ========================
%%% Conveyor Belts
%%% ========================

% Spawns conveyor belt processes.
start_conveyors(NumBelts, PackageManagerPid, TruckManagerPid) ->
    lists:map(fun(N) ->
        spawn(?MODULE, conveyor_belt, [N, self(), PackageManagerPid, TruckManagerPid]) 
    end, lists:seq(1, NumBelts)).

start_loading(TruckPid, PackageManagerPid, ConvId) ->
    PackageManagerPid ! {new_package, ConvId},
    receive
        {assign_package, Size} ->
            timer:sleep(100),
            TruckPid ! {load_package, Size, ConvId},
            io:format("Conveyor ~p: Loading package of size ~p onto Truck ~p.~n", [ConvId, Size, TruckPid]),
            receive
                {truck_not_full} ->
                    start_loading(TruckPid, PackageManagerPid, ConvId);
                {truck_full, Size} ->
                    PackageManagerPid ! {return_package, Size},
                    ok
            end;
        {not_more_packages} ->
            io:format("Conveyor ~p: No more packages to load. Terminating.~n", [ConvId]),
            exit(normal)
    end.

conveyor_belt(Id, MainPid, PackageManagerPid, TruckManagerPid) ->
    io:format("Conveyor ~p requested new truck.~n", [self()]),
    TruckManagerPid ! {new_truck, self()},
    receive
        {delivered_new_truck, TruckPid} ->
            start_loading(TruckPid, PackageManagerPid, self()),
            conveyor_belt(Id, MainPid, PackageManagerPid, TruckManagerPid);

        {no_more_trucks} ->
            io:format("Conveyor ~p: No more trucks available.~n", [Id]),
            exit(normal);

        {truck_full} ->
            TruckManagerPid ! {new_truck, self()}
    end.


%%% ========================
%%% Trucks
%%% ========================

% Spawns truck processes.
start_trucks(NumTrucks, TruckCapacity) ->
    lists:map(fun(N) ->
        TruckPid = spawn(?MODULE, truck, [N, self(), TruckCapacity, 0]),
        io:format("Truck ~p: Started with capacity ~p.~n", [TruckPid, TruckCapacity]),
        TruckPid
    end, lists:seq(1, NumTrucks)).

truck(Id, MainPid, Capacity, Load) ->
    receive
        {load_package, _Size, _ConvId} when _Size + Load >= Capacity ->
            io:format("Truck ~p: Full. Cannot load more packages.~n", [self()]),
            _ConvId ! {truck_full, _Size},
            exit(normal);
        {load_package, Size, ConvId} when Size + Load < Capacity ->
            io:format("Truck ~p: Loaded package of size ~p. Remaining capacity: ~p~n", [Id, Size, Capacity - Size]),
            ConvId ! {truck_not_full},
            truck(Id, MainPid, Capacity, Load + Size)
    end.

truck_manager([]) ->
    receive
        {new_truck, ConveyorId} ->
            ConveyorId ! {no_more_trucks},
            % here
            io:format("Truck Manager: No more trucks available for ~p.~n", [ConveyorId]),
            truck_manager([])
    end;

truck_manager([TruckPid | TruckRest]) ->
    receive
        {new_truck, ConveyorId} ->
            io:format("Truck Manager: Assigning Truck ~p to Conveyor ~p.~n", [TruckPid, ConveyorId]),
            ConveyorId ! {delivered_new_truck, TruckPid},
            truck_manager(TruckRest);
        stop ->
            exit(normal)
    end.
