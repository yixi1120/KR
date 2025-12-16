:- encoding(utf8).

/* Cat & Mouse Game - HTTP Server
   Provides REST API for web frontend
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(cat_mouse).

% Log file configuration
:- dynamic log_file_path/1.
log_file_path('game_log.txt').

% Initialize log file
init_log_file :-
    log_file_path(LogFile),
    get_time(Time),
    format_time(string(TimeStr), '%Y-%m-%d %H:%M:%S', Time),
    open(LogFile, write, Stream),
    format(Stream, '========================================~n', []),
    format(Stream, 'Cat & Mouse Game Log~n', []),
    format(Stream, 'Started at: ~w~n', [TimeStr]),
    format(Stream, '========================================~n~n', []),
    close(Stream).

% Write to the log (automatically add timestamp)
log_to_file(Message) :-
    log_file_path(LogFile),
    get_time(Time),
    format_time(string(TimeStr), '%H:%M:%S', Time),
    open(LogFile, append, Stream),
    format(Stream, '[~w] ~w~n', [TimeStr, Message]),
    flush_output(Stream),
    close(Stream).

log_to_file(Format, Args) :-
    format(string(Message), Format, Args),
    log_to_file(Message).

% Log helpers
log_game(Message) :- log_to_file(string('[GAME] ~w', [Message])).
log_game(Format, Args) :- 
    format(string(Message), Format, Args),
    log_to_file(string('[GAME] ~w', [Message])).

log_api(Message) :- log_to_file(string('[API] ~w', [Message])).
log_api(Format, Args) :- 
    format(string(Message), Format, Args),
    log_to_file(string('[API] ~w', [Message])).

% HTTP routes
:- http_handler(root(.), http_reply_file('game_frontend_connected.html', []), []).
:- http_handler(root('background.jpg'), http_reply_file('background.jpg', []), []).
:- http_handler(root('api/init'), handle_init, []).
:- http_handler(root('api/state'), handle_state, []).
:- http_handler(root('api/move'), handle_move, [method(post)]).
:- http_handler(root('api/take'), handle_take, [method(post)]).
:- http_handler(root('api/unlock'), handle_unlock, [method(post)]).
:- http_handler(root('api/sense'), handle_sense, [method(post)]).
:- http_handler(root('api/cat_turn'), handle_cat_turn, [method(post)]).
:- http_handler(root('api/look'), handle_look, [method(post)]).

% Start server
start_server(Port) :-
    init_log_file,
    log_file_path(LogFile),
    http_server(http_dispatch, [port(Port)]),
    format('~n========================================~n', []),
    format('  Cat & Mouse Game Server Started~n', []),
    format('========================================~n', []),
    format('  URL: http://localhost:~w~n', [Port]),
    format('  Log File: ~w~n', [LogFile]),
    format('  Open the log file to see real-time game events!~n', []),
    format('========================================~n~n', []),
    
    log_api('========================================', []),
    log_api('Server started on port ~w', [Port]),
    log_api('========================================', []).

% Default port
start_server :- start_server(9178).

% Initialize game
handle_init(Request) :-
    cors_enable(Request, [methods([get,post])]),
    log_to_file(''),
    log_to_file(''),
    log_api('========================================', []),
    log_api('INITIALIZING NEW GAME', []),
    log_api('========================================', []),
    with_output_to(string(_), init_game),
    get_game_state(State),
    log_game('Mouse starts at (~w,~w)', [State.mouse.x, State.mouse.y]),
    log_game('Cat starts at (~w,~w)', [State.cat.x, State.cat.y]),
    log_game('Items placed on map', []),
    log_game('Traps placed on map', []),
    log_api('========================================', []),
    log_to_file('Game ready! Good luck!'),
    reply_json_dict(State).

% Get game state
handle_state(Request) :-
    cors_enable(Request, [methods([get,post])]),
    get_game_state(State),
    reply_json_dict(State).

% Move action
handle_move(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Data),
    Direction = Data.direction,
    atom_string(DirAtom, Direction),
    
    log_to_file(''),
    log_api('========================================', []),
    log_api('Player command: move(~w)', [DirAtom]),
    log_api('----------------------------------------', []),
    
    at(mouse, OldX, OldY),
    log_game('Mouse at (~w,~w)', [OldX, OldY]),
    
    % Capture all output
    with_output_to(string(_), (
        catch(move(DirAtom), E, (
            log_game('ERROR: Move failed - ~w', [E]),
            fail
        ))
    )),
    
    at(mouse, NewX, NewY),
    log_game('Mouse moved to (~w,~w)', [NewX, NewY]),
    
    % Check the game status
    (   game_over(Result)
    ->  log_game('GAME OVER: ~w', [Result])
    ;   true
    ),
    
    log_api('----------------------------------------', []),
    
    get_game_state(State),
    reply_json_dict(State).

% Take item
handle_take(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Data),
    log_to_file(''),
    log_api('Player command: take', []),
    
    at(mouse, X, Y),
    % Capture all output
    with_output_to(string(_), (
        (   get_dict(item, Data, ItemStr)
        ->  atom_string(Item, ItemStr),
            log_game('Attempting to take: ~w', [Item]),
            catch(take(Item), _, true)
        ;   (   item_at(Item, X, Y)
            ->  log_game('Found item at (~w,~w): ~w', [X, Y, Item]),
                catch(take(Item), _, true),
                log_game('Picked up: ~w', [Item])
            ;   log_game('No item at (~w,~w)', [X, Y])
            )
        )
    )),
    
    (   game_over(win)
    ->  log_game('YOU WIN! Got the cookie!', [])
    ;   true
    ),
    
    get_game_state(State),
    reply_json_dict(State).

% Unlock door
handle_unlock(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Data),
    log_to_file(''),
    log_api('Player command: unlock', []),
    at(mouse, X, Y),
    with_output_to(string(_), (
        (   get_dict(door, Data, DoorStr)
        ->  atom_string(Door, DoorStr),
            log_game('Attempting to unlock: ~w', [Door]),
            catch(unlock(Door), _, true)
        ;   (   door_cell(Door, X, Y)
            ->  log_game('Unlocking door: ~w at (~w,~w)', [Door, X, Y]),
                catch(unlock(Door), _, true),
                log_game('Door ~w is now unlocked!', [Door])
            ;   log_game('Not standing on a door at (~w,~w)', [X, Y])
            )
        )
    )),
    get_game_state(State),
    reply_json_dict(State).

% Sense action
handle_sense(Request) :-
    cors_enable(Request, [methods([post])]),
    log_to_file(''),
    log_api('Player command: sense', []),
    at(mouse, X, Y),
    log_game('Sensing from position (~w,~w)', [X, Y]),
    with_output_to(string(_), catch(sense, _, true)),
    get_game_state(State),
    reply_json_dict(State).

% Look action
handle_look(Request) :-
    cors_enable(Request, [methods([post])]),
    log_to_file(''),
    log_api('Player command: look', []),
    at(mouse, X, Y),
    log_game('Looking at position (~w,~w)', [X, Y]),
    with_output_to(string(_), catch(look, _, true)),
    get_game_state(State),
    reply_json_dict(State).

% Cat's turn
handle_cat_turn(Request) :-
    cors_enable(Request, [methods([post])]),
    log_to_file(''),
    log_api('========================================', []),
    log_api('CAT AI TURN', []),
    log_api('========================================', []),
    
    at(cat, OldCX, OldCY),
    at(mouse, MX, MY),
    log_game('Cat at (~w,~w), Mouse at (~w,~w)', [OldCX, OldCY, MX, MY]),
    
    with_output_to(string(_), (
        catch(cat_turn, E, (
            log_game('ERROR: Cat AI failed - ~w', [E]),
            true
        ))
    )),
    
    at(cat, NewCX, NewCY),
    log_game('Cat moved to (~w,~w)', [NewCX, NewCY]),
    
    (   game_over(Result)
    ->  log_game('GAME OVER: ~w', [Result])
    ;   true
    ),
    
    log_api('========================================', []),
    
    get_game_state(State),
    reply_json_dict(State).

% Get complete game state as JSON
get_game_state(State) :-
    at(mouse, MX, MY),
    at(cat, CX, CY),
    
    findall(
        _{item: Item, x: X, y: Y},
        item_at(Item, X, Y),
        Items
    ),
    
    findall(Item, has(mouse, Item), Inventory),
    
    findall(
        _{x: TX, y: TY},
        trap(TX, TY),
        Traps
    ),
    
    (locked(bedroom_door) -> BedroomLocked = true ; BedroomLocked = false),
    (locked(kitchen_door) -> KitchenLocked = true ; KitchenLocked = false),
    (game_over(Result) -> GameOver = Result ; GameOver = false),
    turn(Turn),
    State = _{
        mouse: _{x: MX, y: MY},
        cat: _{x: CX, y: CY},
        items: Items,
        inventory: Inventory,
        traps: Traps,
        doors: _{
            bedroom_door: _{locked: BedroomLocked, x: 3, y: 4},
            kitchen_door: _{locked: KitchenLocked, x: 5, y: 4}
        },
        game_over: GameOver,
        turn: Turn
    }.

