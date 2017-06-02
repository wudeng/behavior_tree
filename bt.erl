-module(bt).

%%-compile(export_all).

-export([
    tick/2,
    test1/0,
    test2/0
]).

-record(bt, {
    id = next_id(), 
    root 
}).

-type composite() :: mem_priority | mem_sequence | priority | sequence.
-type action() :: succeeder | failer | wait.
-type decorator() :: invertor.
-type condition() :: atom().

-record(node, {
    id = next_id(),
    name :: composite() | decorator() | action() | condition(),
    children = [],
    child,
    param = []
}).

-record(tick, {
    target = 0, 
    blackboard = dict:new(),
    open_nodes = []
}).

tick(#bt{root = Root}, Tick) ->
    {_Status, NewTick} = execute(Root, Tick),
    #tick{
        open_nodes = CurOpenNodes,
        blackboard = Blackboard
    } = NewTick,
    LastOpenNodes = get_key(open_nodes, Blackboard, []),
    Tick1 = #tick{
        blackboard = NewBlackboard
    } = close_nodes(Root, CurOpenNodes, LastOpenNodes, NewTick),
    Tick1#tick{
        open_nodes = [],
        blackboard = dict:store(open_nodes, CurOpenNodes, NewBlackboard)
    }.

close_nodes(_, _, [], Tick) -> Tick;
close_nodes(undefined, _, _, Tick) -> Tick;
close_nodes(#node{children = [], child = undefined} = Node, CurOpenNodes, LastOpenNodes, Tick) ->
    close_node(Node, CurOpenNodes, LastOpenNodes, Tick);
close_nodes(#node{children = Children, child = undefined} = Node, CurOpenNodes, LastOpenNodes, Tick) ->
    NewTick = close_node(Node, CurOpenNodes, LastOpenNodes, Tick),
    lists:foldl(
        fun(Child, Acc) ->
            close_nodes(Child, CurOpenNodes, LastOpenNodes, Acc)
        end,
        NewTick,
        Children
    );
close_nodes(#node{children = [], child = Child} = Node, CurOpenNodes, LastOpenNodes, Tick) ->
    NewTick = close_node(Node, CurOpenNodes, LastOpenNodes, Tick),
    close_nodes(Child, CurOpenNodes, LastOpenNodes, NewTick).


close_node(#node{id = Id} = Node, CurOpenNodes, LastOpenNodes, #tick{blackboard = Blackboard} = Tick) ->
    case lists:member(Id, LastOpenNodes) andalso 
         not lists:member(Id, CurOpenNodes) andalso 
         get_key({is_open, Id}, Blackboard, false) of
        true -> 
            io:format("close_node ~p", [Node]),
            close_cb_(Node, Tick#tick{blackboard = dict:store({is_open, Id}, false, Blackboard)});
        false -> 
            Tick
    end.

execute(#node{id = Id} = Node, #tick{blackboard = Blackboard} = Tick) ->
    Tick1 = enter_cb(Node, Tick),
    Tick2 = case get_key({is_open, Id}, Blackboard, false) of 
        false -> open_cb(Node, Tick1);
        true -> Tick1
    end,
    {Status, Tick3} = tick_cb(Node, Tick2),
    Tick4 = case Status of 
        running -> Tick3;
        _ -> close_cb(Node, Tick3)
    end,
    Tick5 = exit_cb(Node, Tick4),
    {Status, Tick5}.


enter_cb(#node{id = Id}, #tick{open_nodes = OpenNodes} = Tick) ->
    Tick#tick{open_nodes = [Id|OpenNodes]}.

open_cb(#node{id = Id} = Node, #tick{blackboard = Blackboard} = Tick) ->
    NewBlackboard = dict:store({is_open, Id}, true, Blackboard),
    open_cb_(Node, Tick#tick{blackboard = NewBlackboard}).

open_cb_(#node{id = Id, name = wait}, #tick{blackboard = Blackboard} = Tick) ->
    NewBlackboard = dict:store({start_time, Id}, now_seconds(), Blackboard),
    Tick#tick{blackboard = NewBlackboard};
open_cb_(#node{id = Id, name = mem_priority}, #tick{blackboard = Blackboard} = Tick) ->
    NewBlackboard = dict:store({running_index, Id}, 1, Blackboard),
    Tick#tick{blackboard = NewBlackboard};
open_cb_(#node{id = Id, name = mem_sequence}, #tick{blackboard = Blackboard} = Tick) ->
    NewBlackboard = dict:store({running_index, Id}, 1, Blackboard),
    Tick#tick{blackboard = NewBlackboard};
open_cb_(_, Tick) ->
    Tick.



%% composite
tick_cb(#node{id = Id, name = mem_priority, children = Children}, #tick{blackboard = Blackboard} = Tick) ->
    RuningIdx = get_key({running_index, Id}, Blackboard, 1),
    ChildrenToRun = lists:filter(
        fun({Idx, _Node}) -> Idx >= RuningIdx end,
        lists:zip(lists:seq(1, length(Children)), Children)
    ),
    mem_priority(Id, ChildrenToRun, Tick);
tick_cb(#node{id = Id, name = mem_sequence, children = Children}, #tick{blackboard = Blackboard} = Tick) ->
    RuningIdx = get_key({running_index, Id}, Blackboard, 1),
    ChildrenToRun = lists:filter(
        fun({Idx, _Node}) -> Idx >= RuningIdx end,
        lists:zip(lists:seq(1, length(Children)), Children)
    ),
    mem_sequence(Id, ChildrenToRun, Tick);
tick_cb(#node{name = priority, children = Children}, Tick) ->
    priority(Children, Tick);
tick_cb(#node{name = sequence, children = Children}, Tick) ->
    sequence(Children, Tick);

%% decorator
tick_cb(#node{name = invertor, child = Child}, Tick) ->
    {Status, NewTick} = execute(Child, Tick),
    case Status of
        running -> {running, NewTick};
        true -> {false, NewTick};
        false -> {true, NewTick}
    end;

%% action
tick_cb(#node{id = Id, name = wait, param = [Seconds]}, #tick{blackboard = Blackboard} = Tick) ->
    StartTime = get_key({start_time, Id}, Blackboard, 0),
    Now = now_seconds(),
    Status = if 
        Now - StartTime >= Seconds -> true;
        true -> running
    end,
    io:format("wait ticking, StartTime:~p, Now:~p, Status:~p~n", [StartTime, Now, Status]),
    {Status, Tick};
tick_cb(#node{name = succeeder}, Tick) ->
    io:format("succeeder ticking~n"),
    {true, Tick};
tick_cb(#node{name = failer}, Tick) ->
    io:format("failer ticking~n"),
    {false, Tick};
tick_cb(#node{name = runner}, Tick) ->
    io:format("runner ticking~n"),
    {running, Tick}.

close_cb(#node{id = Id} = Node, #tick{open_nodes = [_|L], blackboard = Blackboard} = Tick) ->
    NewBlackboard = dict:store({is_open, Id}, false, Blackboard),
    close_cb_(Node, Tick#tick{open_nodes = L, blackboard = NewBlackboard}).

close_cb_(_Node, Tick) ->
    Tick.

exit_cb(_Node, Tick) ->
    Tick.

get_key(Key, Blackboard, Default) ->
    case dict:find(Key, Blackboard) of
        {ok, Value} -> Value;
        error -> Default
    end.

mem_priority(_Id, [], Tick) ->
    {false, Tick};
mem_priority(Id, [{I, C}|Children], Tick) ->
    case execute(C, Tick) of
        {true, NewTick} -> 
            {true, NewTick};
        {running, #tick{blackboard = Blackboard} = NewTick} ->
            NewBlackboard = dict:store({running_index, Id}, I, Blackboard),
            {running, NewTick#tick{blackboard = NewBlackboard}};
        {false, NewTick} ->
            mem_priority(Id, Children, NewTick)
    end.

mem_sequence(_Id, [], Tick) ->
    {true, Tick};
mem_sequence(Id, [{I, C}|Children], Tick) ->
    case execute(C, Tick) of
        {running, #tick{blackboard = Blackboard} = NewTick} ->
            NewBlackboard = dict:store({running_index, Id}, I, Blackboard),
            {running, NewTick#tick{blackboard = NewBlackboard}};
        {false, NewTick} -> 
            {false, NewTick};
        {true, NewTick} ->
            mem_sequence(Id, Children, NewTick)
    end.

priority([], Tick) ->
    {false, Tick};
priority([C|Children], Tick) ->
    case execute(C, Tick) of
        {true, NewTick} -> 
            {true, NewTick};
        {running, NewTick} ->
            {running, NewTick};
        {false, NewTick} ->
            priority(Children, NewTick)
    end.

sequence([], Tick) ->
    {true, Tick};
sequence([C|Children], Tick) ->
    case execute(C, Tick) of
        {running, NewTick} ->
            {running, NewTick};
        {false, NewTick} -> 
            {false, NewTick};
        {true, NewTick} ->
            sequence(Children, NewTick)
    end.

now_seconds() ->
	{MegaSecs, Secs, _MicroSecs} = os:timestamp(),
	MegaSecs * 1000000 + Secs.

next_id() -> 
    NextId = case get(id) of
        undefined -> 0;
        Id -> Id + 1
    end,
    put(id, NextId),
    NextId.

test1() -> test(bt1()).
test2() -> test(bt2()).

test(BT) ->
    lists:foldl(
        fun(_, Tick) ->
            NewTick = tick(BT, Tick),
            timer:sleep(1000),
            NewTick
        end,
        #tick{},
        lists:seq(1, 12)
    ).

bt1() ->
    #bt{root = #node{name = priority,
        children = [
            #node{name = sequence,
                children = [
                    #node{name = succeeder},
                    #node{name = failer}
                ]
            },
            #node{name = sequence,
                children = [
                    #node{name = succeeder},
                    #node{name = failer}
                ]
            },
            #node{name = wait, param = [10]}
        ]
    }}.

bt2() ->
    #bt{root = #node{name = mem_priority,
        children = [
            #node{name = sequence,
                children = [
                    #node{name = succeeder},
                    #node{name = failer}
                ]
            },
            #node{name = sequence,
                children = [
                    #node{name = succeeder},
                    #node{name = failer}
                ]
            },
            #node{name = wait, param = [10]}
        ]
    }}.

