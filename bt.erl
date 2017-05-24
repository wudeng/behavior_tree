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

-record(tick, {target = 0, open_nodes = []}).

tick(#bt{root = Root}, Target) ->
    {_Status, #tick{
        open_nodes = CurOpenNodes 
    }} = execute(Root, #tick{target = Target, open_nodes = []}),
    LastOpenNodes = get_key(open_nodes, []),
    lists:foreach(
        fun(Id) ->
            not lists:member(Id, CurOpenNodes) andalso 
            get_key({is_open, Id}, false) andalso
            put({is_open, Id}, false)
        end,
        LastOpenNodes
    ),
    put(open_nodes, LastOpenNodes),
    ok.

execute(#node{id = Id} = Node, Tick) ->
    Tick1 = enter_cb(Node, Tick),
    Tick2 = case get_key({is_open, Id}, false) of 
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

open_cb(#node{id = Id} = Node, Tick) ->
    set_key({is_open, Id}, true),
    open_cb_(Node, Tick).

open_cb_(#node{id = Id, name = wait}, Tick) ->
    set_key({start_time, Id}, now_seconds()),
    Tick;
open_cb_(#node{id = Id, name = mem_priority}, Tick) ->
    set_key({running_index, Id}, 1),
    Tick;
open_cb_(#node{id = Id, name = mem_sequence}, Tick) ->
    set_key({running_index, Id}, 1),
    Tick;
open_cb_(_, Tick) ->
    Tick.



%% composite
tick_cb(#node{id = Id, name = mem_priority, children = Children}, Tick) ->
    RuningIdx = get_key({running_index, Id}, 1),
    ChildrenToRun = lists:filter(
        fun({Idx, _Node}) -> Idx >= RuningIdx end,
        lists:zip(lists:seq(1, length(Children)), Children)
    ),
    mem_priority(Id, ChildrenToRun, Tick);
tick_cb(#node{id = Id, name = mem_sequence, children = Children}, Tick) ->
    RuningIdx = get_key({running_index, Id}, 1),
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
tick_cb(#node{id = Id, name = wait, param = [Seconds]}, Tick) ->
    StartTime = get_key({start_time, Id}, 0),
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

close_cb(#node{id = Id}, #tick{open_nodes = [_|L]} = Tick) ->
    set_key({is_open, Id}, false),
    Tick#tick{open_nodes = L}.

exit_cb(_Node, Tick) ->
    Tick.

get_key(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        Value -> Value
    end.

set_key(Key, Value) ->
    put(Key, Value).

mem_priority(_Id, [], Tick) ->
    {false, Tick};
mem_priority(Id, [{I, C}|Children], Tick) ->
    case execute(C, Tick) of
        {true, NewTick} -> 
            {true, NewTick};
        {running, NewTick} ->
            set_key({running_index, Id}, I),
            {running, NewTick};
        {false, NewTick} ->
            mem_priority(Id, Children, NewTick)
    end.

mem_sequence(_Id, [], Tick) ->
    {true, Tick};
mem_sequence(Id, [{I, C}|Children], Tick) ->
    case execute(C, Tick) of
        {running, NewTick} ->
            set_key({running_index, Id}, I),
            {running, NewTick};
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
    Target = 1,
    lists:foreach(
        fun(_) ->
            tick(BT, Target),
            timer:sleep(1000)
        end,
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

