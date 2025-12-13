:- encoding(utf8).

:- module(pyperplan_runner,
          [ run_pyperplan_soln/4    % +Exe, +DomainFile, +ProblemFile, -Plan
          ]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(strings)).

% Call pyperplan and parse the solution file
% Returns plan as list of actions like [move_cat(c1_1,c1_2), ...]

run_pyperplan_soln(Exe, DomainFile, ProblemFile, Plan) :-
    % Execute pyperplan
    process_create(Exe,
                   [DomainFile, ProblemFile],
                   [ stdout(null),
                     stderr(pipe(Err)),
                     process(PID)
                   ]),
    read_string(Err, _, ErrStr),
    close(Err),
    % Wait for the process to end
    process_wait(PID, Status),
    (   Status = exit(0)
    ->  soln_file_from_problem(ProblemFile, SolnFile),
        read_plan_file(SolnFile, Plan)
    ;   format(user_error,
              'pyperplan failed (~w):~n~s~n',
              [Status, ErrStr]),
        fail
    ).

% Get solution file path from problem file
soln_file_from_problem(ProblemFile, SolnFile) :-
    atom_concat(ProblemFile, '.soln', SolnFile).

% Read and parse solution file line by line
read_plan_file(SolnFile, Plan) :-
    setup_call_cleanup(
        open(SolnFile, read, S),
        read_all_plan_lines(S, Plan),
        close(S)
    ).

read_all_plan_lines(S, Plan) :-
    read_line_to_string(S, Line0),
    (   Line0 == end_of_file
    ->  Plan = []
    ;   parse_plan_line(Line0, Action),
        read_all_plan_lines(S, Rest),
        (   Action == none
        ->  Plan = Rest
        ;   Plan = [Action|Rest]
        )
    ).

% Parse a single line into an action term
parse_plan_line(Line, Action) :-
    normalize_space(string(Trim), Line),
    (   Trim == ""
    ->  Action = none
    ;   sub_string(Trim, 0, 1, _, "("),
        sub_string(Trim, _, 1, 0, ")")
    ->  sub_string(Trim, 1, _, 1, Inside),
        split_string(Inside, " ", "", Parts),
        Parts = [NameStr|ArgStrs],
        hyphen_to_underscore(NameStr, FunctorAtom),
        maplist(string_to_atom, ArgStrs, ArgAtoms),
        Action =.. [FunctorAtom|ArgAtoms]
    ;   Action = none
    ).

hyphen_to_underscore(Str, Atom) :-
    string_codes(Str, Codes),
    maplist(replace_hyphen, Codes, Codes2),
    string_codes(Str2, Codes2),
    atom_string(Atom, Str2).

replace_hyphen(0'-, 0'_) :- !.
replace_hyphen(C, C).

string_to_atom(Str, Atom) :-
    atom_string(Atom, Str).
