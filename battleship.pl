main(X) :-
    open('./board.txt', read, B),
    readStream(B, X).

run :-
    open('./board.txt', read, B),
    readStream(B, X),
    write(X),
    nl,
    write('Welcome to Battleship!'), nl.

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