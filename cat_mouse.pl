:- encoding(utf8).

/* Cat & Mouse Game
   Grid: 5x8, coordinates (X,Y) where X=1..5, Y=1..8
   Rooms: Bedroom(1-3,1-3), Kitchen(4-5,1-3), Living room(1-5,5-8)
   Doors: bedroom_door at (3,4), kitchen_door at (5,4)
   Wall at (4,4)
   Mouse starts at hole (1,8)
   Items randomly placed: bedroom_key, kitchen_key, cookie, 2 traps
*/

:- use_module(library(random)).
:- use_module('C:/Users/ziyou/Desktop/CI/pyperplan_runner.pl').

% Wrapper for calling pyperplan planner
pyperplan_solve(DomainFile, ProblemFile, Plan) :-
    Exe = 'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
    run_pyperplan_soln(Exe, DomainFile, ProblemFile, Plan).

:- dynamic at/3.          % at(Agent, X, Y)
:- dynamic item_at/3.     % item_at(Item, X, Y)
:- dynamic has/2.         % has(Agent, Item)
:- dynamic trap/2.        % trap(X, Y)
:- dynamic locked/1.      % locked(DoorID)
:- dynamic turn/1.        % turn(TurnNumber)
:- dynamic game_over/1.   % game_over(win/lose)
:- dynamic door_origin/4. % door_origin(Agent, DoorID, FromX, FromY)

% Coordinate conversion helpers
% Convert (X,Y) to cell name atom like c1_8

coord_cell_name(X, Y, CellAtom) :-
    number_string(X, SX),
    number_string(Y, SY),
    string_concat("c", SX, T1),
    string_concat(T1, "_", T2),
    string_concat(T2, SY, SCell),
    atom_string(CellAtom, SCell).

cell_name_coord(CellAtom, X, Y) :-
    % Parse cell name back to coordinates
    atom_string(CellAtom, S),
    string_codes(S, Codes),
    Codes = [0'c | Rest],          % remove leading 'c'
    phrase(split_xy(X, Y), Rest).

split_xy(X, Y) -->
    digits(DX),
    "_",
    digits(DY),
    { number_codes(X, DX),
      number_codes(Y, DY) }.

digits([D|Ds]) --> [D], { char_type(D, digit) }, !, digits(Ds).
digits([])     --> [].

% Map definitions and helpers

% Room types
room(bedroom).
room(kitchen).
room(living_room).

% Determine which room a coordinate is in
room_of(X,Y, bedroom) :-
    between(1,3,X),
    between(1,3,Y).
room_of(X,Y, kitchen) :-
    between(4,5,X),
    between(1,3,Y).
room_of(X,Y, living_room) :-
    between(1,5,X),
    between(5,8,Y).
room_of(3,4, living_room).   % bedroom door cell
room_of(5,4, living_room).   % kitchen door cell

% Valid playable coordinates
within_bounds(X,Y) :-
    between(1,5,X),
    between(1,8,Y).

% Solid wall
wall(4,4).

% Check if cell is walkable
walkable(X,Y) :-
    within_bounds(X,Y),
    \+ wall(X,Y),
    room_of(X,Y,_).

% Door locations
door_cell(bedroom_door, 3,4).
door_cell(kitchen_door, 5,4).

% Key-door pairs
needs_key(bedroom_door, bedroom_key).
needs_key(kitchen_door, kitchen_key).

% Locked door crossings
locked_crossing(bedroom_door, 3,4, 3,5).
locked_crossing(bedroom_door, 3,5, 3,4).
locked_crossing(kitchen_door, 5,4, 5,5).
locked_crossing(kitchen_door, 5,5, 5,4).

% Movement directions
direction(north).
direction(south).
direction(west).
direction(east).

step(north,X,Y, X,Y2) :- Y2 is Y-1.
step(south,X,Y, X,Y2) :- Y2 is Y+1.
step(west, X,Y, X2,Y) :- X2 is X-1.
step(east, X,Y, X2,Y) :- X2 is X+1.

% Game initialization

start :-
    init_game,
    writeln('Welcome to Cat & Mouse!'),
    help,
    describe_current_cell.

help :-
    nl,
    writeln('--- Commands ---'),
    writeln('look.                % describe your current cell'),
    writeln('sense.               % sense the four neighbouring cells'),
    writeln('move(north|south|east|west).'),
    writeln('take(Item).          % pick up bedroom_key | kitchen_key | cookie'),
    writeln('unlock(bedroom_door|kitchen_door).'),
    writeln('status.              % show position, inventory, doors'),
    writeln('start.               % restart a new game'),
    writeln('cat_turn.               % cat turn'),
    writeln('------------------'),
    nl.

% Game setup with randomization

init_game :-
    retractall(at(_,_,_)),
    retractall(item_at(_,_,_)),
    retractall(has(_,_)),
    retractall(trap(_,_)),
    retractall(locked(_)),
    retractall(turn(_)),
    retractall(game_over(_)),
    retractall(door_origin(_,_,_,_)),

    % Initial positions
    assert(at(mouse, 1,8)),
    assert(at(cat,   1,1)),

    % Doors start locked
    assert(locked(bedroom_door)),
    assert(locked(kitchen_door)),

    % Place items and traps randomly
    place_bedroom_key,
    place_kitchen_key,
    place_cookie,
    place_traps,

    assert(turn(0)).

% Place bedroom key randomly (not at cat start)
place_bedroom_key :-
    findall((X,Y),
            ( between(1,3,X),
              between(1,3,Y),
              \+ (X=1, Y=1)
            ),
            Cells),
    random_member((BX,BY), Cells),
    assert(item_at(bedroom_key, BX,BY)).

% Place kitchen key in living room
place_kitchen_key :-
    findall((X,Y),
            ( between(1,5,X),
              between(5,8,Y)
            ),
            Cells),
    random_member((KX,KY), Cells),
    assert(item_at(kitchen_key, KX,KY)).

% Place cookie in kitchen
place_cookie :-
    findall((X,Y),
            ( between(4,5,X),
              between(1,3,Y)
            ),
            Cells),
    random_member((CX,CY), Cells),
    assert(item_at(cookie, CX,CY)).

% Place two traps (avoiding safe zones and kitchen key)
place_traps :-
    (   item_at(kitchen_key, KX, KY)
    ->  true
    ;   KX = -1, KY = -1
    ),
    findall((X,Y),
            ( between(1,5,X),
              between(5,8,Y),
              \+ (X = 1, Y = 8),        % mouse hole
              \+ (X = 1, Y = 7),        % safe cell
              \+ (X = 2, Y = 8),        % safe cell
              \+ (X = 5, Y = 5),        % safe cell
              \+ (X = KX, Y = KY)       % avoid kitchen key
            ),
            Cells0),
    random_permutation(Cells0, [C1,C2|_]),
    maplist(assert_trap, [C1,C2]).



assert_trap((X,Y)) :-
    assert(trap(X,Y)).

% Game state management

check_not_over :-
    (   game_over(_)
    ->  writeln('Game is over. Type start. to play again.'), fail
    ;   true).

after_player_action :-
    (   game_over(_)
    ->  true
    ;   retract(turn(N)),
        N1 is N + 1,
        assert(turn(N1)),
        check_game_over
    ).

% Check win/lose conditions
check_game_over :-
    (   game_over(_)
    ->  true

    ;   % Win: get the cookie
        has(mouse,cookie)
    ->  assert(game_over(win)),
        nl,
        writeln('You grab the cookie and start eating it.'),
        writeln('You win!')

    ;   % Lose: trap
        at(mouse,X,Y),
        trap(X,Y)
    ->  assert(game_over(lose)),
        nl,
        writeln('Snap! You stepped on a mousetrap. Game over.')

    ;   % Lose: caught
        at(mouse,X,Y),
        at(cat,X,Y)
    ->  assert(game_over(lose)),
        nl,
        writeln('The cat catches you. Game over.')

    ;   true
    ).

% Player commands: status, look, sense

status :-
    check_not_over,
    at(mouse,X,Y),
    room_of(X,Y,Room),
    format('You are at (~w,~w) in the ~w.~n',[X,Y,Room]),
    (   has(mouse,Item),
        format('You carry: ~w~n',[Item]),
        fail
    ;   true
    ),
    (   \+ has(mouse,_)
    ->  writeln('You carry nothing.')
    ;   true
    ),
    (   locked(bedroom_door) -> BD = locked ; BD = unlocked ),
    (   locked(kitchen_door) -> KD = locked ; KD = unlocked ),
    format('Bedroom door is ~w.~n',[BD]),
    format('Kitchen door is ~w.~n',[KD]),
    turn(N),
    format('Turn: ~w~n',[N]).

look :-
    check_not_over,
    describe_current_cell.

describe_current_cell :-
    at(mouse,X,Y),
    room_of(X,Y,Room),
    format('You are at (~w,~w) in the ~w.~n',[X,Y,Room]),
    (   door_cell(Door,X,Y)
    ->  format('You are standing on the ~w.~n',[Door])
    ;   true
    ),
    (   item_at(Item,X,Y)
    ->  format('There is a ~w here.~n',[Item])
    ;   true
    ),
    (   trap(X,Y)
    ->  writeln('You see a deadly mousetrap right under your paws!')
    ;   true
    ),
    (   mouse_hole(X,Y)
    ->  writeln('This is your cosy mouse hole.')
    ;   true
    ).

% Mouse starting position
mouse_hole(1,8).

sense :-
    check_not_over,
    at(mouse,X,Y),
    writeln('You sniff around...'),
    forall(direction(D), describe_neighbour(D,X,Y)),
    after_player_action.

describe_neighbour(Dir,X,Y) :-
    step(Dir,X,Y, X2,Y2),
    (   \+ within_bounds(X2,Y2)
    ->  format('To the ~w: the outer wall.~n',[Dir])
    ;   \+ walkable(X2,Y2)
    ->  format('To the ~w: a solid wall.~n',[Dir])
    ;   describe_cell_short(Dir,X2,Y2)
    ).

describe_cell_short(Dir,X,Y) :-
    findall(Item, item_at(Item,X,Y), Items),
    (   trap(X,Y)       -> Trap = yes ; Trap = no ),
    (   door_cell(D,X,Y)-> Door = D   ; Door = none ),
    (   at(cat,X,Y)     -> Cat = yes  ; Cat = no ),
    (   Items \= [] ; Trap = yes ; Door \= none ; Cat = yes )
    ->  format('To the ~w: ',[Dir]),
        describe_features(Items,Trap,Door,Cat),
        nl
    ;   format('To the ~w: empty floor.~n',[Dir]).

describe_features(Items,Trap,Door,Cat) :-
    (   Items \= []
    ->  write('items '), write(Items), write(' ')
    ;   true
    ),
    (   Trap = yes
    ->  write('a mousetrap ')
    ;   true
    ),
    (   Door \= none
    ->  write('a door('), write(Door), write(') ')
    ;   true
    ),
    (   Cat = yes
    ->  write('the Cat ')
    ;   true
    ).

% PDDL problem file generation for AI planning

generate_pddl_problem(File) :-
    open(File, write, S),
    write_problem_header(S),
    write_objects(S),
    write_init_block(S),
    write_goal(S),
    write_problem_footer(S),
    close(S).

write_problem_header(S) :-
    format(S, "(define (problem catmouse-current)~n", []),
    format(S, "  (:domain catmouse)~n", []).

write_problem_footer(S) :-
    format(S, ")~n", []).

write_objects(S) :-
    format(S, "  (:objects~n", []),
    % all cell objects
    format(S, "    c1_1 c2_1 c3_1 c4_1 c5_1~n", []),
    format(S, "    c1_2 c2_2 c3_2 c4_2 c5_2~n", []),
    format(S, "    c1_3 c2_3 c3_3 c4_3 c5_3~n", []),
    format(S, "    c3_4 c5_4~n", []),
    format(S, "    c1_5 c2_5 c3_5 c4_5 c5_5~n", []),
    format(S, "    c1_6 c2_6 c3_6 c4_6 c5_6~n", []),
    format(S, "    c1_7 c2_7 c3_7 c4_7 c5_7~n", []),
    format(S, "    c1_8 c2_8 c3_8 c4_8 c5_8 - cell~n", []),
    % doors and keys
    format(S, "    bedroom_door kitchen_door - door~n", []),
    format(S, "    bedroom_key kitchen_key - key~n", []),
    format(S, "  )~n", []).

write_init_block(S) :-
    format(S, "  (:init~n", []),
    write_init_dynamic_positions(S),
    write_init_keys_state(S),
    write_init_static_map(S),
    format(S, "  )~n", []).

write_init_dynamic_positions(S) :-
    % cat position
    at(cat, Xc, Yc),
    coord_cell_name(Xc, Yc, CatCell),
    format(S, "    (cat-at ~w)~n", [CatCell]),
    % mouse position (for reference)
    at(mouse, Xm, Ym),
    coord_cell_name(Xm, Ym, MouseCell),
    format(S, "    (mouse-at ~w)~n", [MouseCell]).

write_init_keys_state(S) :-
    % which key opens which door
    format(S, "    (key-for bedroom_key bedroom_door)~n", []),
    format(S, "    (key-for kitchen_key kitchen_door)~n", []),
    % keys on the floor
    (   item_at(bedroom_key, Xb, Yb)
    ->  coord_cell_name(Xb, Yb, BCell),
        format(S, "    (key-at bedroom_key ~w)~n", [BCell])
    ;   true
    ),
    (   item_at(kitchen_key, Xk, Yk)
    ->  coord_cell_name(Xk, Yk, KCell),
        format(S, "    (key-at kitchen_key ~w)~n", [KCell])
    ;   true
    ),
    % keys already held by the cat
    (   has(cat, bedroom_key)
    ->  format(S, "    (has-key bedroom_key)~n", [])
    ;   true
    ),
    (   has(cat, kitchen_key)
    ->  format(S, "    (has-key kitchen_key)~n", [])
    ;   true
    ),
    % door-open flags
    (   \+ locked(bedroom_door)
    ->  format(S, "    (door-open bedroom_door)~n", [])
    ;   true
    ),
    (   \+ locked(kitchen_door)
    ->  format(S, "    (door-open kitchen_door)~n", [])
    ;   true
    ).

write_init_static_map(S) :-
    % ---- bedroom adjacency (1..3 x 1..3) + bedroom door ----
    % horizontal
    format(S, "    (adj c1_1 c2_1) (adj c2_1 c1_1)~n", []),
    format(S, "    (adj c2_1 c3_1) (adj c3_1 c2_1)~n", []),
    format(S, "    (adj c1_2 c2_2) (adj c2_2 c1_2)~n", []),
    format(S, "    (adj c2_2 c3_2) (adj c3_2 c2_2)~n", []),
    format(S, "    (adj c1_3 c2_3) (adj c2_3 c1_3)~n", []),
    format(S, "    (adj c2_3 c3_3) (adj c3_3 c2_3)~n", []),
    % vertical
    format(S, "    (adj c1_1 c1_2) (adj c1_2 c1_1)~n", []),
    format(S, "    (adj c1_2 c1_3) (adj c1_3 c1_2)~n", []),
    format(S, "    (adj c2_1 c2_2) (adj c2_2 c2_1)~n", []),
    format(S, "    (adj c2_2 c2_3) (adj c2_3 c2_2)~n", []),
    format(S, "    (adj c3_1 c3_2) (adj c3_2 c3_1)~n", []),
    format(S, "    (adj c3_2 c3_3) (adj c3_3 c3_2)~n", []),
    % bedroom door (3,3)<->(3,4)
    format(S, "    (adj c3_3 c3_4) (adj c3_4 c3_3)~n", []),

    % ---- kitchen adjacency (4..5 x 1..3) + kitchen door ----
    % horizontal
    format(S, "    (adj c4_1 c5_1) (adj c5_1 c4_1)~n", []),
    format(S, "    (adj c4_2 c5_2) (adj c5_2 c4_2)~n", []),
    format(S, "    (adj c4_3 c5_3) (adj c5_3 c4_3)~n", []),
    % vertical
    format(S, "    (adj c4_1 c4_2) (adj c4_2 c4_1)~n", []),
    format(S, "    (adj c4_2 c4_3) (adj c4_3 c4_2)~n", []),
    format(S, "    (adj c5_1 c5_2) (adj c5_2 c5_1)~n", []),
    format(S, "    (adj c5_2 c5_3) (adj c5_3 c5_2)~n", []),
    % kitchen door (5,3)<->(5,4)
    format(S, "    (adj c5_3 c5_4) (adj c5_4 c5_3)~n", []),

    % ---- living room adjacency (1..5 x 5..8) ----
    % row y=5
    format(S, "    (adj c1_5 c2_5) (adj c2_5 c1_5)~n", []),
    format(S, "    (adj c2_5 c3_5) (adj c3_5 c2_5)~n", []),
    format(S, "    (adj c3_5 c4_5) (adj c4_5 c3_5)~n", []),
    format(S, "    (adj c4_5 c5_5) (adj c5_5 c4_5)~n", []),
    % row y=6
    format(S, "    (adj c1_6 c2_6) (adj c2_6 c1_6)~n", []),
    format(S, "    (adj c2_6 c3_6) (adj c3_6 c2_6)~n", []),
    format(S, "    (adj c3_6 c4_6) (adj c4_6 c3_6)~n", []),
    format(S, "    (adj c4_6 c5_6) (adj c5_6 c4_6)~n", []),
    % row y=7
    format(S, "    (adj c1_7 c2_7) (adj c2_7 c1_7)~n", []),
    format(S, "    (adj c2_7 c3_7) (adj c3_7 c2_7)~n", []),
    format(S, "    (adj c3_7 c4_7) (adj c4_7 c3_7)~n", []),
    format(S, "    (adj c4_7 c5_7) (adj c5_7 c4_7)~n", []),
    % row y=8
    format(S, "    (adj c1_8 c2_8) (adj c2_8 c1_8)~n", []),
    format(S, "    (adj c2_8 c3_8) (adj c3_8 c2_8)~n", []),
    format(S, "    (adj c3_8 c4_8) (adj c4_8 c3_8)~n", []),
    format(S, "    (adj c4_8 c5_8) (adj c5_8 c4_8)~n", []),
    % vertical 5<->6
    format(S, "    (adj c1_5 c1_6) (adj c1_6 c1_5)~n", []),
    format(S, "    (adj c2_5 c2_6) (adj c2_6 c2_5)~n", []),
    format(S, "    (adj c3_5 c3_6) (adj c3_6 c3_5)~n", []),
    format(S, "    (adj c4_5 c4_6) (adj c4_6 c4_5)~n", []),
    format(S, "    (adj c5_5 c5_6) (adj c5_6 c5_5)~n", []),
    % vertical 6<->7
    format(S, "    (adj c1_6 c1_7) (adj c1_7 c1_6)~n", []),
    format(S, "    (adj c2_6 c2_7) (adj c2_7 c2_6)~n", []),
    format(S, "    (adj c3_6 c3_7) (adj c3_7 c3_6)~n", []),
    format(S, "    (adj c4_6 c4_7) (adj c4_7 c4_6)~n", []),
    format(S, "    (adj c5_6 c5_7) (adj c5_7 c5_6)~n", []),
    % vertical 7<->8
    format(S, "    (adj c1_7 c1_8) (adj c1_8 c1_7)~n", []),
    format(S, "    (adj c2_7 c2_8) (adj c2_8 c2_7)~n", []),
    format(S, "    (adj c3_7 c3_8) (adj c3_8 c3_7)~n", []),
    format(S, "    (adj c4_7 c4_8) (adj c4_8 c4_7)~n", []),
    format(S, "    (adj c5_7 c5_8) (adj c5_8 c5_7)~n", []),

    % ---- doors ----
    % bedroom door at c3_4 connecting c3_4 <-> c3_5
    format(S, "    (door-cell bedroom_door c3_4)~n", []),
    format(S, "    (door-edge bedroom_door c3_4 c3_5)~n", []),
    format(S, "    (door-edge bedroom_door c3_5 c3_4)~n", []),
    % kitchen door at c5_4 connecting c5_4 <-> c5_5
    format(S, "    (door-cell kitchen_door c5_4)~n", []),
    format(S, "    (door-edge kitchen_door c5_4 c5_5)~n", []),
    format(S, "    (door-edge kitchen_door c5_5 c5_4)~n", []).

write_goal(S) :-
    % goal: cat reaches the current mouse cell
    at(mouse, Xm, Ym),
    coord_cell_name(Xm, Ym, MouseCell),
    format(S, "  (:goal~n", []),
    format(S, "    (cat-at ~w)~n", [MouseCell]),
    format(S, "  )~n", []).

% Player actions

% Move mouse in given direction
move(Dir) :-
    check_not_over,
    direction(Dir),
    at(mouse,X,Y),
    step(Dir,X,Y, X2,Y2),
    (   \+ within_bounds(X2,Y2)
    ->  writeln('You bump into the outer wall.'), !
    ;   \+ walkable(X2,Y2)
    ->  writeln('You bump into a solid wall.'), !
    ;   \+ can_pass(mouse,X,Y,X2,Y2)
    ->  writeln('The door in that direction is locked.'), !
    ;   update_position(mouse,X,Y,X2,Y2),
        format('You move to (~w,~w).~n',[X2,Y2]),
        describe_current_cell,
        after_player_action
    ).

% Handle invalid direction
move(Dir) :-
    \+ direction(Dir),
    writeln('Unknown direction. Use north/south/east/west.').

% Check if movement is allowed (handles door logic)
can_pass(Agent,X1,Y1,X2,Y2) :-
    (   door_cell(_Door, X2,Y2)
    ->  true    % Moving into door cell always OK

    ;   door_cell(Door, X1,Y1)
    ->  (   locked(Door)
        ->  door_origin(Agent,Door,FromX,FromY),
            X2 =:= FromX, Y2 =:= FromY    % Can only go back if locked
        ;   true    % Can go anywhere if unlocked
        )

    ;   true    % No door involved
    ).

% Update position and track door entry/exit
update_position(Agent,X1,Y1,X2,Y2) :-
    retract(at(Agent,X1,Y1)),
    assert(at(Agent,X2,Y2)),
    (   door_cell(Door, X2,Y2)
    ->  retractall(door_origin(Agent,Door,_,_)),
        assert(door_origin(Agent,Door,X1,Y1))
    ;   door_cell(Door, X1,Y1)
    ->  retractall(door_origin(Agent,Door,_,_))
    ;   true
    ).

% Take item from current cell
take(Item) :-
    check_not_over,
    at(mouse,X,Y),
    (   item_at(Item,X,Y)
    ->  retract(item_at(Item,X,Y)),
        assert(has(mouse,Item)),
        format('You pick up the ~w.~n',[Item]),
        after_player_action
    ;   format('There is no ~w here.~n',[Item])
    ).

% Unlock door (must stand on it with correct key)
unlock(Door) :-
    check_not_over,
    door_cell(Door, DX,DY),
    at(mouse, X,Y),
    (   X =:= DX, Y =:= DY
    ->  needs_key(Door, Key),
        (   has(mouse,Key)
        ->  (   locked(Door)
            ->  retract(locked(Door)),
                format('You use the ~w to unlock the ~w.~n',[Key,Door]),
                after_player_action
            ;   writeln('This door is already unlocked.')
            )
        ;   writeln('You do not have the right key.')
        )
    ;   writeln('You must stand on the door cell to unlock it.')
    ).

% Test function for debugging planner
test_plan :-
    catch(
        (
            pyperplan_runner:run_pyperplan_soln(
                'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
                'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
                'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
                Plan
            ),
            writeln('PLAN ='),
            maplist(writeln, Plan)
        ),
        E,
        (
            writeln('ERROR ====='),
            writeln(E)
        )
    ).

% Cat AI using PDDL planner

% Execute one turn for the cat
cat_turn :-
    check_not_over,
    at(cat,CX,CY),
    at(mouse,MX,MY),
    format('Cat is at (~w,~w), mouse at (~w,~w).~n',[CX,CY,MX,MY]),

    % Generate PDDL problem for current state
    generate_pddl_problem('C:/Users/ziyou/Desktop/CI/catmouse-current.pddl'),

    % Call planner and execute first action
    (   run_pyperplan_soln(
            'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
            'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
            'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
            Plan)
    ->  cat_apply_first_action(Plan)
    ;   writeln('Planner failed: cat skips this turn.')
    ).

% Apply first action from plan
cat_apply_first_action([]) :-
    writeln('Planner returned empty plan: cat stays this turn.').

cat_apply_first_action([Action|_]) :-
    format('Cat planned first action: ~w~n',[Action]),
    cat_do_action(Action),
    after_player_action.

% Parse and execute planner actions

% Normal movement
cat_do_action(move_cat(FromCell, ToCell)) :-
    cell_name_coord(FromCell, FX, FY),
    cell_name_coord(ToCell,   TX, TY),
    update_position(cat, FX,FY, TX,TY),
    format('The cat moves to (~w,~w).~n',[TX,TY]).

% Pick up key
cat_do_action(pickup_key(Key, Cell)) :-
    cell_name_coord(Cell, X, Y),
    (   item_at(Key, X, Y)
    ->  retract(item_at(Key, X, Y)),
        assert(has(cat, Key)),
        format('The cat picks up the ~w at (~w,~w).~n',[Key,X,Y])
    ;   format('Cat wanted to pick up ~w at (~w,~w), but it is not there.~n',
               [Key,X,Y])
    ).

% Unlock door
cat_do_action(unlock_door(Door, Cell, Key)) :-
    cell_name_coord(Cell, X, Y),
    (   door_cell(Door, X, Y),
        has(cat, Key),
        locked(Door)
    ->  retract(locked(Door)),
        format('The cat uses ~w to unlock ~w at (~w,~w).~n',[Key,Door,X,Y])
    ;   format('Cat tried to unlock ~w at (~w,~w) with ~w but failed.~n',
               [Door,X,Y,Key])
    ).

% Move through door
cat_do_action(move_through_door(Door, FromCell, ToCell)) :-
    cell_name_coord(FromCell, FX, FY),
    cell_name_coord(ToCell,   TX, TY),
    (   \+ locked(Door)
    ->  update_position(cat, FX,FY, TX,TY),
        format('The cat moves through ~w to (~w,~w).~n',[Door,TX,TY])
    ;   format('Cat tried to move through locked door ~w.~n',[Door])
    ).

% Handle unknown actions
cat_do_action(Other) :-
    format('Unknown planner action for the cat: ~w~n',[Other]).
