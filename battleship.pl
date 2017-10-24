:- dynamic game_won/0, current_move/2, turn/1, hit_attempt/1, turn_result/1, computer_mode/1.

current_move(Index) :- current_move(Row, Col), toIndex(coord(Row, Col), Index).

%% Runs the game
run :-
    clean,
    start_game,
    play_game,
    end_game,
    !.

%% Initializes the game
start_game :-
    write('Welcome to Battleship'),
    nl,
    !.

%% Starts the play sequence
play_game :-
    catch(readFile(Board),error(_,_),fail),
    validateBoard(Board),
    write('Let\'s play!'), nl,

    board(empty_board, Empty),

    % Generates human boards
    assert( board(player_primary, Board) ),
    assert( board(player_tracking, Empty) ),

    % Generates computer boards
    generateComputerBoard(ComputerBoard),
    assert( board(computer_primary, ComputerBoard) ),
    assert( board(computer_tracking, Empty) ),
    assert( computer_mode(hunt) ),

    % Player Starts First
    show_player,
    player_turn;
    write('Invalid board input.'), nl,
    write('Please correct the input board and type \'run.\' again.'),
    fail.

%% Ends the game
end_game :-
    write('Thanks for Playing!'),
    nl,
    retract( game_won ).

%% Reads the move of a player
read_validate_move :-
    write('Make your next move! fire(Row, Col)'), nl,
    repeat, (
        (catch(read(fire(Row, Col)), error(_, _), false),
        validate(Row, Col),
        assert( current_move(Row, Col) )
        ) -> true;
        (write('That\'s not a valid move. :( Try again.'), nl, fail)
    ),!.

validate(Row, Col) :-
    number(Row), number(Col),
    turn(player),
    board(computer_primary, ComputerBoard),
    canAttempt(coord(Row, Col), ComputerBoard).

validate(Row, Col) :-
    number(Row), number(Col),
    turn(computer),
    board(player_primary, PlayerBoard),
    canAttempt(coord(Row, Col), PlayerBoard).


% On player turn; If the chosen square is empty (has a value of 0), 
% updates computer_primary board and player_tracking board to a miss
% (to a value of 2)
update_boards :-
    turn(player),
    current_move(Index),
    board(computer_primary,ComputerBoard),
    nth0(Index,ComputerBoard,0),
    board(player_tracking,PlayerTrackingBoard),
    nth0(Index,PlayerTrackingBoard,0),
    replaceNth(ComputerBoard,Index,2,NewComputerBoard),
    replaceNth(PlayerTrackingBoard,Index,2,NewPlayerTrackingBoard),
    update_attempt_miss,
    retract(board(computer_primary,_)),
    retract(board(player_tracking,_)),
    assert(board(computer_primary,NewComputerBoard)),
    assert(board(player_tracking,NewPlayerTrackingBoard)). 

% On player turn; If the chosen square has a ship (has a value of 1), 
% updates computer_primary board and player_tracking board to a hit
% (to a value of 3) 
update_boards :-
    turn(player),
    current_move(Index),
    board(computer_primary,ComputerBoard),
    nth0(Index,ComputerBoard,1),
    board(player_tracking,PlayerTrackingBoard),
    nth0(Index,PlayerTrackingBoard,0),
    replaceNth(ComputerBoard,Index,3,NewComputerBoard),
    replaceNth(PlayerTrackingBoard,Index,3,NewPlayerTrackingBoard),
    update_attempt_hit,
    retract(board(computer_primary,_)),
    retract(board(player_tracking,_)),
    assert(board(computer_primary,NewComputerBoard)),
    assert(board(player_tracking,NewPlayerTrackingBoard)).

% On computer turn; If the chosen square is empty (has a value of 0), 
% updates player_primary board and computer_tracking board to a miss
% (to a value of 2)  
update_boards :-
    turn(computer),
    current_move(Index),
    board(player_primary,PlayerBoard),
    nth0(Index,PlayerBoard,0),
    board(computer_tracking,ComputerTrackingBoard),
    nth0(Index,ComputerTrackingBoard,0),
    replaceNth(PlayerBoard,Index,2,NewPlayerBoard),
    replaceNth(ComputerTrackingBoard,Index,2,NewComputerTrackingBoard),
    update_attempt_miss,
    retract( board(player_primary,_) ),
    retract( board(computer_tracking,_) ),
    assert( board(player_primary,NewPlayerBoard) ),
    assert( board(computer_tracking,NewComputerTrackingBoard) ). 

% On computer turn; If the chosen square has a ship (has a value of 1), 
% updates player_primary board and computer_tracking board to a hit
% (to a value of 3) 
update_boards :-
    turn(computer),
    current_move(Index),
    board(player_primary,PlayerBoard),
    nth0(Index,PlayerBoard,1),
    board(computer_tracking,ComputerTrackingBoard),
    nth0(Index,ComputerTrackingBoard,0),
    replaceNth(PlayerBoard,Index,3,NewPlayerBoard),
    replaceNth(ComputerTrackingBoard,Index,3,NewComputerTrackingBoard),
    update_attempt_hit,
    retract( board(player_primary,_) ),
    retract( board(computer_tracking,_) ),
    assert( board(player_primary,NewPlayerBoard) ),
    assert( board(computer_tracking,NewComputerTrackingBoard) ).

update_boards :- turn(player), \+ current_move(_), read_validate_move.

check_win :-
    turn(player),
    write('Checkng if you won...'), nl,
    board(computer_primary,Board),
    spacesOccupied(Board, 6, 3),
    assert( game_won ),
    !
    .

check_win :-
    turn(player),
    board(computer_primary,Board),
    \+ spacesOccupied(Board, 6, 3),
    write('Player hasn\'t won...:(\n'),
    !
    .

check_win :-
    turn(computer),
    write('Checking if your enemy has won...'), nl,
    board(player_primary,Board),
    spacesOccupied(Board, 6, 3),
    assert( game_won ),
    !
    .

check_win :-
    turn(computer),
    board(player_primary,Board),
    \+ spacesOccupied(Board, 6, 3),
    write('Computer hasn\'t won...:D\n'),
    !
    .

player_turn :- game_won.

player_turn :-
    \+ game_won,
    write('Your turn!'), nl,
    assert( turn(player) ),
    read_validate_move,
    update_boards,
    turn_result,
    check_win,
    retract( turn(player) ),
    retract( current_move(_,_) ),
    computer_turn.

computer_turn :- game_won.

computer_turn :-
    \+ game_won,
    write('Your enemy\'s turn. >:('), nl,
    assert( turn(computer) ),
    computer_move,
    update_boards,
    show_player,
    turn_result,
    check_win,
    react,
    retract( turn(computer) ),
    retract( current_move(_,_) ),
    player_turn,!.

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

check_board(Board) :- check_board(Board, 0).
check_board([H|T], N) :- (H = 1; H = 2; H = 0), check_board(T, N).
check_board([3|T], N) :- N1 is N + 1, check_board(T, N1).
check_board([], 6).

board(
    empty_board, 
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ).

%% Displays boards to the player
toDisplay(0, ' ').
toDisplay(1, '◼︎').
toDisplay(2, '◎').
toDisplay(3, '✕').

show_board(B) :- show_line, show_board(B, 0).

show_board([H|T], N) :-
    next_line(N),
    toDisplay(H, Disp),
    show_space(Disp),
    N1 = N + 1,
    show_board(T, N1).

show_board([], _) :- write('|'), nl, show_line.

show_space(Space) :- write('| '), write(Space), write(' ').
show_line :- write('+-------------------+'), nl.
next_line(N) :- ((X is mod(N, 5), X > 0); N = 0).
next_line(N) :- \+ N = 0, 0 is mod(N, 5), write('|'), nl, show_line.

update_attempt_hit :- retract( hit_attempt(_) ), update_attempt_hit.
update_attempt_hit :- \+ retract( hit_attempt(_) ), assert( hit_attempt(hit) ).

update_attempt_miss :- retract( hit_attempt(_) ), update_attempt_miss.
update_attempt_miss :- \+ retract( hit_attempt(_) ), assert( hit_attempt(miss) ).

show_player :-
    board(player_tracking, PlayerTracking),
    board(player_primary, PlayerBoard),
    write('+---- Your Enemy ---+'), nl,
    show_board(PlayerTracking), nl,
    write('+---- Your Ships ---+'), nl,
    show_board(PlayerBoard).

% Shows the player the result of the current turn.
turn_result :- 
    hit_attempt(hit),
    turn(player),
    current_move(Row, Col),
    write('\n<<<Your attempt hit a ship at ('),
    write(Row), write(', '), write(Col),
    write(')! :D>>>\n'),
    sleep(2).

turn_result :-
    hit_attempt(miss),
    turn(player),
    write('\n<<<You missed!>>>\n'),
    sleep(2).

turn_result :- 
    hit_attempt(hit),
    turn(computer),
    current_move(Row, Col),
    write('\n<<<Your enemy hit you at ('),
    write(Row), write(', '), write(Col),
    write(')! :(>>>\n').

turn_result :- 
    hit_attempt(miss),
    turn(computer),
    current_move(Row, Col),
    write('\n<<<Your enemy fired at ('),
    write(Row), write(', '), write(Col),
    write(') but they missed! :D>>>\n').
    

% Determines whether a board is valid for this game
validateBoard(B) :-
    length(B,25),
    spacesOccupied(B,6,1),
    occupiedValid(B,3).

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

% Determines how many spaces are occupied with value V on the board
spacesOccupied([],0,_).
spacesOccupied([S|B],N,V) :- number(V), S = V, spacesOccupied(B,N1,V), N is N1+1.
spacesOccupied([S|B],N,V) :- number(V), S \= V, spacesOccupied(B,N,V).

% Generates a valid random board for the computer opponent
generateComputerBoard(B) :- repeat, N = 3, createEmptyBoard(BA),placeShips(N,BA,B),(validateBoard(B) -> true, ! ; fail).

% Places multiple ships on a board
placeShips(0,B,B).
placeShips(N,BA,B) :- placeShip(BA,BR), N1 is N-1, placeShips(N1,BR,B).

% Places a single ship on a board
placeShip(B,BR) :-
    randomPosition(P), 
    positionAvailable(B,P), 
    findAdjacents(P,L), 
    length(L,LL), 
    LL1 is LL-1,
    random_between(0,LL1,I1),
    nth0(I1,L,P1),
    positionAvailable(B,P1),
    replaceNth(B,P,1,BT),
    replaceNth(BT,P1,1,BR).
    
% Finds the adjacent squares
findAdjacents(P,L) :- member(P,[6,7,8,11,12,13,16,17,18]), P1 is P-5, P2 is P-1, P3 is P+1, P4 is P+5, L = [P1,P2,P3,P4].
findAdjacents(P,L) :- member(P,[1,2,3]), P1 is P-1, P2 is P+1, P3 is P+5, L = [P1,P2,P3].
findAdjacents(P,L) :- member(P,[5,10,15]), P1 is P-5, P2 is P+1, P3 is P+5, L = [P1,P2,P3].
findAdjacents(P,L) :- member(P,[9,14,19]), P1 is P-5, P2 is P-1, P3 is P+5, L = [P1,P2,P3].
findAdjacents(P,L) :- member(P,[21,22,23]), P1 is P-5, P2 is P-1,P3 is P+1, L = [P1,P2,P3].
findAdjacents(P,L) :- P = 0, L = [1,5].
findAdjacents(P,L) :- P = 4, L = [3,9].
findAdjacents(P,L) :- P = 20, L = [15,21].
findAdjacents(P,L) :- P = 24, L = [19,23].

% Returns true if the position chosen is unused
positionAvailable(B,P) :- nth0(P,B,0).

% replaceNth(L,P,V,R). Replaces nth element in list with a given value
replaceNth([_|T],0,V,[V|T]).
replaceNth([H|T],P,V,[H|R]) :- P > 0, P < 25, P1 is P - 1, replaceNth(T,P1,V,R). 

% Choose a random position on the board
randomPosition(P) :- random_between(0,24,P).

% Creates an empty board
createEmptyBoard(R) :- listOfZeros(25,R).

% Creates a list of zeros
listOfZeros(0,[]).
listOfZeros(N,[0|R]) :- N1 is N-1, N1 >= 0, listOfZeros(N1,R).

readFile(Board) :-
    open('./board.txt', read, Stream),
    readStream(Stream, Board).

%% Cleans up all the asserted predicates.
clean :-
    clean_player_primary,
    clean_player_tracking,
    clean_computer_primary,
    clean_computer_tracking,
    clean_turn,
    clean_attempt,
    clean_current_move,
    clean_computer_mode,
    !.

clean_player_primary :- repeat, (retract( board(player_primary,_) ) -> false; true).
clean_player_tracking :- repeat, (retract( board(player_tracking,_) ) -> false; true).
clean_computer_primary :- repeat, (retract( board(computer_primary,_) ) -> false; true).
clean_computer_tracking :- repeat, (retract( board(computer_tracking,_) ) -> false; true).
clean_turn :- repeat, (retract( turn(_) ) -> false; true).
clean_attempt :- repeat, (retract( hit_attempt(_) ) -> false; true).
clean_current_move :- repeat, (retract( current_move(_, _) ) -> false; true).
clean_computer_mode :- repeat, (retract( computer_mode(_) ) -> false; true).

%% AI
computer_move :-
    turn(computer),
    computer_mode(destroy(Index)),
    
    write('Selecting DESTROY move...\n'),
    findAdjacents(Index, Neighbours),
    board(computer_tracking, TrackingBoard),
    canAttemptAdjacents(Neighbours, TrackingBoard, ValidNeighbours),
    nth0(0, ValidNeighbours, Value),
    toCoord(Value, coord(Row, Col)),
    assert( current_move(Row, Col) ).

computer_move :-
    turn(computer),
    computer_mode(hunt),

    write('Selecting HUNT move...\n'),
    board(computer_tracking, TrackingBoard),
    calculate(TrackingBoard, L),
    sort(2, @>=, L, SortedList),
    take(SortedList, 10, Index, Taken),
    IndexTaken is Taken - 1,
    random_between(0, IndexTaken, MoveIndex),
    get(Index, MoveIndex, Value),
    toCoord(Value, coord(Row, Col)),
    assert( current_move(Row, Col) ).

% If computer is in destroy mode and misses, keep going.
react :-
    turn(computer),
    computer_mode(destroy(_)),
    hit_attempt(miss),
    write('Staying in DESTROY mode.\n'),
    !
    .

react :-
    turn(computer),
    computer_mode(destroy(_)),
    hit_attempt(hit),
    retract( computer_mode(_) ),
    assert( computer_mode(hunt) ),
    write('Switching to HUNT mode.\n'),
    !
    .

react :-
    turn(computer),
    computer_mode(hunt),
    hit_attempt(miss),
    write('Staying in HUNT mode.\n'),
    !
    .

react :-
    turn(computer),
    computer_mode(hunt),
    hit_attempt(hit),
    retract( computer_mode(_) ),
    current_move(Index),
    assert( computer_mode(destroy(Index)) ),
    write('Switching to DESTROY mode.\n'),
    !
    .

calculate(Board, L) :- calculate(0, Board, [], L).

calculate(Index, Board, L1, [(Index, Num)|L2]) :-
    Index > -1, Index < 25,
    get(Board, Index, Value),
    \+ attempted(Value),
    findAdjacents(Index, Neighbours),
    calculateAdjacents(Neighbours, Board, Num),
    NextIndex is Index + 1,
    calculate(NextIndex, Board, L1, L2).

calculate(Index, Board, L1, L2) :-
    Index > -1, Index < 25,
    get(Board, Index, Value),
    attempted(Value),
    NextIndex is Index + 1, 
    calculate(NextIndex, Board, L1, L2).

calculate(Index, _, L, L) :- (Index < 0; Index > 24).

calculateAdjacents([H|T], Board, Num) :-
    get(Board, H, Value),
    \+ attempted(Value),
    calculateAdjacents(T, Board, X),
    Num is X + 1.

calculateAdjacents([H|T], Board, Num) :-
    get(Board, H, Value),
    attempted(Value),
    calculateAdjacents(T, Board, Num).

calculateAdjacents([], _, 0).

canAttemptAdjacents(List, Board, L) :- checkNeighbours(List, Board, [], L).
checkNeighbours([H|T], Board, L1, L2) :-
    get(Board, H, Value),
    attempted(Value),
    checkNeighbours(T, Board, L1, L2).

checkNeighbours([H|T], Board, L1, [H|L2]) :-
    get(Board, H, Value),
    \+ attempted(Value),
    checkNeighbours(T, Board, L1, L2).

checkNeighbours([], _, L, L).

take(Spaces, Num, L, X) :- take(Spaces, Num, [], L, X).

take(_, Num, L, L, 0) :- Num < 1.
take([(Index, _)|T], Num, L1, [Index|L2], X1) :-
    Num1 is Num - 1,
    take(T, Num1, L1, L2, X),
    X1 is X + 1.