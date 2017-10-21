main(X) :-
    open('./board.txt', read, B),
    readStream(B, X).

run :-
    open('./board.txt', read, B),
    readStream(B, X),
    write(X),
    validateBoard(X),
    !
    .
    
% Determines whether a board is valid for this game
validateBoard(B) :-
    length(B,25),
    spacesOccupied(B,6),
    occupiedValid(B,3),
    nl,
    write('Welcome to Battleship!');
    nl,
    write("Board invalid. Please try again.").

% Determines if the spaces in a given board are occupied in a valid way.
% In other words, are the 2X1 ship placements valid?
occupiedValid(B,NST) :-
    getRow(0,B,R0),
    countShips(R0,CR0,0,NS1),
    getRow(1,B,R1),
    countShips(R1,CR1,NS1,NS2),
    getRow(2,B,R2),
    countShips(R2,CR2,NS2,NS3),
    getRow(3,B,R3),
    countShips(R3,CR3,NS3,NS4),
    getRow(4,B,R4),
    countShips(R4,CR4,NS4,NS5),
    append(CR0,CR1,CRN1),
    append(CRN1,CR2,CRN2),
    append(CRN2,CR3,CRN3),
    append(CRN3,CR4,CRN4),
    getColumn(0,CRN4,C0),
    countShips(C0,_,NS5,NS6),
    getColumn(1,CRN4,C1),
    countShips(C1,_,NS6,NS7),
    getColumn(2,CRN4,C2),
    countShips(C2,_,NS7,NS8),
    getColumn(3,CRN4,C3),
    countShips(C3,_,NS8,NS9),
    getColumn(4,CRN4,C4),
    countShips(C4,_,NS9,NS10),
    NS10 = NST.

% Counts the number of ships on the given board
countShips([],[],A,A).
countShips([H|T],[H|CR],A,N) :- dif(H,1),countShips(T,CR,A,N).
countShips([1],[1],A,A).
countShips([1,0|T],[1,0|CR],A,N) :- countShips(T,CR,A,N).
countShips([1,1,1|T],[1,0,0|CR],A,N) :- A1 is A+1, countShips(T,CR,A1,N).
countShips([1,1,1|T],[0,0,1|CR],A,N) :- A1 is A+1, countShips(T,CR,A1,N).
countShips([1,1,1|T],[1,1,1|CR],A,N) :- countShips(T,CR,A,N).
countShips([1,1|T],[0,0|CR],A,N) :- A1 is A+1, countShips(T,CR,A1,N).

% Finds the elements for the given column number in the 5x5 board; zero indexed
getColumn(N,_,[]) :- N>24.
getColumn(N,B,C) :-
    nth0(N,B,A), N1 is N+5, getColumn(N1,B,C1), C = [A|C1].

% Finds the elements for the given row number in the 5x5 board; zero indexed
getRow(N,B,R) :-
    N1 is N*5, N2 is N1+5, sliceList(R,N1,N2,B).

% Slices a list to create a smaller list given starting and ending positions
sliceList(L,S,E,[_|T]):- 
    S>0, S<E, sliceList(L,S-1,E-1,T).
sliceList(L,S,E,[H|T]):- 
    0 is S, S<E, E2 is E-1, L=[H|T1], sliceList(T1,0,E2,T).
sliceList([],0,0,_).

% Determines how many spaces are occupied on the board
spacesOccupied([],0).
spacesOccupied([S|B],N) :- S = 1, spacesOccupied(B,N1), N is N1+1.
spacesOccupied([S|B],N) :- S \= 1, spacesOccupied(B,N).

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