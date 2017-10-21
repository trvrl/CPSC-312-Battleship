:- dynamic game_won/0.

%% Runs the game
run :-
    start_game,
    play_game,
    end_game.

%% Initializes the game
start_game :-
    write('Welcome to Battleship'),
    nl,
    write('Enter \'Y.\' to start...'),
    nl,
    read(Yes),
    (Yes = 'Y' ; Yes = 'y').

%% Starts the play sequence
play_game :-
    write('Let\'s play!'),
    nl,
    open('./board.txt', read, Stream),
    readStream(Stream, Board),
    assert( board(player_primary, Board) ),
    empty_board(EB),
    assert( board(player_tracking, EB) ),
    player_turn.

%% Ends the game
end_game :-
    write('Thanks for Playing!'),
    nl,
    retract( game_won ).

%% Reads the move of a player
read_move :-
    write('Make your next move!'), nl,
    read(_).

validate_move :-
    write('Your move was valid!'), nl.

update_boards.

check_win :-
    write('Wanna win?'), nl,
    read(X),
    X = 'Y',
    assert( game_won ).

player_turn :- game_won.

player_turn :-
    \+ game_won,
    write('Player\'s turn.'),
    read_move,
    validate_move,
    check_win,
    update_boards,
    computer_turn.

computer_turn :- game_won.

computer_turn :-
    \+ game_won,
    write('Computer\'s turn.'),
    read_move,
    validate_move,
    check_win,
    update_boards,
    player_turn.


% Reads a single list from the board file.
readStream(Stream, Result) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Result),
    at_end_of_stream(Stream).

% A board coordinate
coord(Row, Col) :-
    atomic(Row), atomic(Col),
    Row > -1, Row < 5,
    Col > -1, Col < 5.

% Converts row and column into an index.
toIndex(coord(Row, Col), Index) :-
    coord(Row, Col),
    Index is (Row * 5) + Col.

% Converts index to row and column
toCoord(Index, coord(Row, Col)) :-
    atomic(Index),
    Index > -1, Index < 25,
    Row is div(Index, 5),
    Col is mod(Index, 5).

get(Board, Index, Space) :-
    Index > -1, Index < 25,
    get4(Board, 0, Index, Space),
    !
    .

get4([H|_], I, I, H).

get4([_|T], Curr, Index, Space) :-
    dif(Curr, Index),
    Next is Curr + 1,
    get4(T, Next, Index, Space),
    !
    .

/*
    Space Codes
    0 - Empty, no hit
    1 - Occupied, no hit
    2 - Empty, hit attempted
    3 - Occupied, hit
*/
attempted(2).
attempted(3).

canAttempt(Coord, B) :-
    toIndex(Coord, Index),
    get(B, Index, Space),
    \+ attempted(Space).

empty_board(EB) :-
    EB = [
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
    ].