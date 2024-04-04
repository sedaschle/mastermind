:- use_module(library(random)).
:- use_module(library(lists)).

% citation: main, play, and make_guess functions based on https://github.com/AhmedNassar7/Prolog-Wordle/blob/main/Wordle.pl
main:-
    write('Welcome to Mastermind!'), nl,
    write('----------------------'), nl,
    write('Enter the desired length of the sequence: '), read(RequiredLength),
    play(RequiredLength).

play(RequiredLength):-
    generate_goal(Goal, RequiredLength),
	colours(Colours),
    Guesses is 10,
    write('Game started. You have '), write(Guesses), write(' guesses.'), nl, nl,
    write('Colour options are: '), nl, write(Colours), nl,
    make_guess(Goal, RequiredLength, Guesses).

make_guess(Goal, RequiredLength, Guesses):-
    write('Enter a guess composed of '), write(RequiredLength), write(' colours, in the form ['),
    generate_print_color_list(RequiredLength, Placeholder),
    write(Placeholder), write('].'), nl,
    % write(' colours, in the form [colour1, colour2, colour3, colour4]. :'), nl,
    read(Guess),
    (
        var(Guess),
        write('You cannot enter variables, try again.'), nl,
        make_guess(Goal, RequiredLength, Guesses)
    ;
        Guess = Goal,
        get_feedback(Goal, Guess, Feedback, RequiredLength),
        write(Feedback), nl,
        write('You won!'), nl
    ;
        Guesses = 1,
        write('You lost!'), nl,
        write('Correct answer: '), nl,
        write(Goal), nl
    ;
        (
            length(Guess, RequiredLength),            
            get_feedback(Goal, Guess, Feedback, RequiredLength),
            write(Feedback), nl,
            NewGuesses is Guesses - 1
        ;
            write('Guess is not composed of '), write(RequiredLength), write(' colours. Try again.'), nl,
            NewGuesses is Guesses
        ),
        write('Remaining Guesses are '), write(NewGuesses), nl, nl,
        make_guess(Goal, RequiredLength, NewGuesses)
    ).


% Returns true if Goal contains N elements selected at random from List (colours)
pick_colours(_, 0, []).
pick_colours(List, N, [X|Goal]) :-
    N > 0,
    length(List, Length),
    random(0, Length, Index),
    nth0(Index, List, X),
    select(X, List, Remaining), 
    N1 is N - 1,
    pick_colours(Remaining, N1, Goal).

% pick RequiredLength random colours from list of all colours
generate_goal(Goal, RequiredLength) :-
	colours(C),
    pick_colours(C, RequiredLength, Goal).

colours(C) :- 
  C = [red, blue, green, yellow, orange, purple, pink, grey].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERATING FEEDBACK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rightcolour_rightspot([], [], 0).
rightcolour_rightspot([H1|T1], [H1|T2], Count) :-
    rightcolour_rightspot(T1, T2, C2),
    Count is C2 + 1.
rightcolour_rightspot([H1|T1], [H2|T2], Count) :-
    dif(H1, H2),
    rightcolour_rightspot(T1, T2, Count).

common_colours(_, [], 0).
common_colours(Goal, [H2|Ans], Count) :- 
    member(H2, Goal),
    common_colours(Goal, Ans, C2), 
    Count is C2 + 1.
common_colours(Goal, [H2|Ans], Count) :- 
    not(member(H2, Goal)),
    common_colours(Goal, Ans, Count).

% CC is all common colours. RC is right colour right spot, WC is wrong colour right spot.
feedback_counts(Goal, Ans, CC, RC, WC) :-
    rightcolour_rightspot(Goal, Ans, RC),
    common_colours(Goal, Ans, CC),
    WC is CC - RC.

check([], 0, 0).

% Recursive case: if RC and WC are both greater than 0, and the Head of the list is 'a',
% recursively check the Tail with decremented RC.
check(['!'|T], RC, WC) :-
    RC > 0,
    RC1 is RC - 1,
    check(T, RC1, WC).

% Recursive case: if RC is 0 and WC is greater than 0, and the Head of the list is 'b',
% recursively check the Tail with decremented WC.
check(['?'|T], 0, WC) :-
    WC > 0,
    WC1 is WC - 1,
    check(T, 0, WC1).

append_wrongs(List, Result, RequiredLength) :-
    length(List, Length),
    Length < RequiredLength,
    append(List, ['-'], NewList),
    append_wrongs(NewList, Result, RequiredLength).

append_wrongs(List, List, RequiredLength) :-
    length(List, RequiredLength).

generate_list(RC, WC, Temp, Result, RequiredLength) :-
    check(Temp, RC, WC),
    append_wrongs(Temp, Result, RequiredLength),
    length(Result, RequiredLength).

get_feedback(Goal, Ans, Feedback, RequiredLength) :-
    feedback_counts(Goal, Ans, _, RC, WC),
    generate_list(RC, WC, _, Feedback, RequiredLength).

generate_print_color_list(RequiredLength, Placeholder) :-
    findall(X, (between(1, RequiredLength, Num), atom_concat('color', Num, X)), ColorList),
    atomic_list_concat(ColorList, ', ', Placeholder).
