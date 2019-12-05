open! CS17SetupGame;   
open SigGame; 

/* data definitions and examples
 
 * whichPlayer: a representation of one of the two players, which is one
 *    of P1 or P2 where P1 is player 1 and P2 is player 2
 * examples:
 *    P1
 *    P2

 * status: an indicator of either the result of the game or whose turn
 *    is next, which is one of Win(whichPlayer), Draw, or Ongoing(whichPlayer)
 * examples:
 *    Win(P1)
 *    Draw

 * state: the progress of the game represented by State(status, list(list(int)))
 *    where status is the current status of the game and list(list(int)) is a
 *    representation of the board where each list of ints is a column of the
 *    board from top to bottom (unoccupied spaces are represented by 0,
 *    player 1's spaces are represented by 1, and player 2's spaces are
 *    represented by 2)
 * examples:
 *    State(Draw, [[1, 2], [1, 2]])
 *    State(Ongoing(P1), [[0, 0], [0, 0]])

 * move: a player move represented by Move(int) where int represents the column
 *    that the player's piece is being dropped into and where the first column
 *    is represented by 1
 * examples:
 *    Move(1)
 *    Move(7)
 */

module Connect4 = {

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

    /* the state of the game: the position, status, anything else associated
       with the game at a given turn */
    type state = State(status, list(list(int)));

    /* describes a move that a player can make */ 
    type move = Move(int);

    /* initializing values */
    let initialRows = 5;
    let initialCols = 7;
    let names = "jli277 and claserso"

    /* ---------------------------------------------- */

    /* printing functions */

    /* stringOfPlayer: whichPlayer => string
     * input: a type whichPlayer, p
     * output: a string representation of the player p
     */
    let stringOfPlayer: whichPlayer => string = p => switch(p) {
    | P1 => "Player 1"
    | P2 => "Player 2"};

    /* stringOfPiece: int => string
     * input: an int, n, which is the representation of a space on the board
     * output: a string of n where 1 is blue, 2 is red, and 0 is white
     */
    let stringOfPiece: int => string = n => switch(n) {
    | 1 => "\027[36m1\027[0m"
    | 2 => "\027[91m2\027[0m"
    | _ => "0"};

    /* transpose: list(list(int)) => list(list(int))
     * input: a list of lists of type int, cols, representing a matrix
     * output: a list of lists of type int representing the transpose of cols
     *
    recursion diagram:
    OI: [[1, 3], [2, 4]]
      RI: [[3], [4]]
      RO: [[3, 4]] 
    OO: [[1, 2], [3, 4]]  */
    let rec transpose : list(list(int)) => list(list(int)) = cols => switch (cols) {
      | [] => failwith("A board cannot be empty.")
      | [[], ..._] => failwith("A board cannot be 0-dimensional.")
      | [[_], ..._] => [List.map(List.hd, cols)]
      | [[_, ..._], ..._] => [List.map(List.hd, cols), ...
                               transpose(List.map(List.tl, cols))]};

    /* stringOfState: state => string
     * input: a state, s
     * output: a string representation of s displayed as a matrix where white
     *         0's represent unoccupied spaces, blue 1's represent player 1's
     *         spaces and red 2's represent player 2's spaces
     * 
    recursion diagram for stringOfStateHelper:
    OI: [[0, 0]]
      RI: []
      RO: ""
    OO: "0 0 \n"  */
    let stringOfState: state => string = (State(_, cols)) => {
      let rec stringOfStateHelper: list(list(int)) => string = rows =>
        switch(rows) {
          | [] => ""
          | [hd, ... tl] => List.fold_right((x, y) =>
            stringOfPiece(x) ++ " " ++ y, hd, "") ++ "\n" ++
            stringOfStateHelper(tl)}
      stringOfStateHelper(transpose(cols))};
    
    /* stringOfMove: move => string
     * input: a move, m
     * output: a string representation of the column chosen in m
     */
    let stringOfMove: move => string = (Move(i)) =>
      "Column " ++ string_of_int(i);

    /* ---------------------------------------------- */

    /* Game Logic */

    /* fill: ('a, int) => list('a)
     * input: element, an item with type 'a, and length, an integer that
     *        specifies the number of times you want element to appear in the 
     *        output list
     * output: a list that contains element repeated length times
     *
    recursion diagram:
    OI: (0, 3) 
      RI: (0, 2)
      RO: [0, 0]
      => cons the element to the recursive output
    OO: [0, 0, 0]
     *
    recursion diagram2:
    OI: ([0, 0], 3)
      RI: ([0, 0], 2)
      RO: [[0, 0], [0, 0]]
    OO: [[0, 0], [0, 0], [0, 0]]  */
    let rec fill: ('a, int) => list('a) = (element, length) =>
        if (length <= 0) {
            [];
          } else {
            [element, ...fill(element, length-1)];
          };

    /* the state of the game when it begins */
    let initialState: state =
      State(Ongoing(P1), fill(fill(0, initialRows), initialCols)); 

    /* legalMoves: state => list(move)
     * input: s, a game state
     * output: the list of legal moves that can be made during s
     *
     * legalColumns: (list(list(int)), int) => list(move)
     * input: board, the list representation of a game board, and currentCol, 
     *        an integer that keeps track of the current column being checked
     *        for empty spaces
     * output: a list of moves that consist of the columns that have empty
     *        spaces where a player can go
     *
    recursion diagram for legalColumns:
    OI: ([[0, 1], [2, 1], [0, 2]], 1)
      RI: ([[2, 1], [0, 2]], 2)
      RO: [Move(3)]
    OO: [Move(1), Move(3)]  */
    let legalMoves: state => list(move) = s => {
      let rec legalColumns: (list(list(int)), int) => list(move) = 
        (board, currentCol) => switch(board) {
          | [] => []
          | [hd, ... tl] => if (List.hd(hd) == 0) {
            [Move(currentCol), ... legalColumns(tl, currentCol + 1)]
          } else { legalColumns(tl, currentCol + 1) }};
      switch(s) {
        | State(Win(_), _) => []
        | State(Draw, _) => []
        | State(Ongoing(_), b) => legalColumns(b, 1)}};

    /* returns the status of the game at the given state */
    /* gameStatus: state => status
     * input: a state, s
     * output: the status of the game at s
     */
    let gameStatus: state => status = (State(st, _)) => st;

    /* diagonals: list(list(int)) => list(list(int))
     * input: b, a list of int lists, that represents a game board where each
     *        int list represents a column
     * output: a list of int lists where each int list contains the values in 
     *        each of the possible diagonals of the game board
     *
     * diagHelper: (list(list(int)), int) => list(list(int))
     * input: board, a list of int lists that represents a game board where each
     *        int list represents a column, and rows, the number of rows board 
     *        has
     * output: a list of int lists where each int list contains the values in 
     *        each diagonal that runs from the lower left to the upper right in
     *        board
     *
    recursion diagram for diagHelper:
     1 4 7 
     2 5 8 
     3 6 9
    OI: ([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 3)
      RI:([[4, 5, 6], [7, 8, 9]], 3)
      RO: [[4], [5, 7], [6, 8], [9]]
    OO: [[1], [2, 4], [3, 5, 7], [6, 8], [9]]   */
    let diagonals: list(list(int)) => list(list(int)) = b => {
      let numRows = List.length(List.hd(b));
      let rec diagHelper: (list(list(int)), int) => list(list(int)) = (board, rows) => {
        let rec take = (alod, n) => switch(alod, n) {
          | (_, 0) => []
          | ([], _) => failwith("Can't take from an empty list")
          | ([hd, ... tl], n) => [hd, ... take(tl, n-1)]
        };
        let rec drop = (alod, n) => switch(alod, n) {
          | (_, 0) => alod
          | ([], _) => failwith("Can't drop from an empty list")
          | ([_, ... tl], n) => drop(tl, n-1)
        };
        switch(board) {
        | [] => failwith("board is empty")
        | [[], ..._] => failwith("column cannot be empty")
        | [col] => List.map(x => [x], col)
        | [[hd, ... tl], ... otherCols] =>
          let rr = diagHelper(otherCols, rows);
          let topCorner = take(rr, rows-1);
          let back = drop(rr, rows-1);
          [[hd], ... List.map2((x, y) => [x, ... y], tl, topCorner)] @ back
      }};
      List.append(diagHelper(b, numRows), diagHelper(List.rev(b), numRows))
    };

    /* check: (float, list(list(int)), list(int)) => float
     * input: a list of lists of type int, set, representing a set of lists to
     *        search in, a list of ints, f, representing the subset to look for
     *        in set, and a float, n
     * output: a float representing the number of times f is a subset of one of
     *         the lists of set, plus n
     * 
    recursion diagram:
    OI: (0, [[1, 1, 0], [1, 1, 0]], [1, 1])
      RI: (1, [[1, 1, 0]], [1, 1])
      RO: 2
    OO: 2  */
    let rec check: (float, list(list(int)), list(int)) => float =
    (n, set, f) => {
      let rec find: (list(int), list(int), list(int)) => bool =
      (set, sub, rest) => switch(set) {
          | [] => false
          | [hd, ... tl] when List.hd(rest) == hd => switch(rest) {
            | [_] => true
            | _ => find(tl, sub, List.tl(rest))}
          | [_, ... tl] => find(tl, sub, sub)};
      switch(set) {
        | [] => n
        | [hd, ... tl] when List.length(hd) < List.length(f) => check(n, tl, f)
        | [hd, ... tl] => if (find(hd, f, f)) { check(n +. 1.0, tl, f)
          } else { check(n, tl, f) }}
    };

    /* allCombinations: list(list(int)) => list(list(int)) 
     * input: a list of lists of type int, board, representing a game board
     * output: a lists of lists of type int representing a list containing
     *         all rows, columns, and diagonals of board
     */
    let allCombinations: list(list(int)) => list(list(int)) = board => {
      let verticals = board;
      let horizontals = transpose(board);
      List.append(List.append(verticals, horizontals), diagonals(board))
    };

    /* given a state and a legal move, yields the next state */
    /* nextState: (state, move) => state
     * input: a state s, and a legal move, m
     * output: the next state that results from starting at s and making m
     * 
    recursion diagram for addToColumn:
    OI: ([0, 0], 1)
      RI: ([0], 1)
      RO: [1]
    OO: [0, 1]
    */
    let nextState: (state, move) => state = (s, m) => {
      let rec addToColumn: (list(int), int) => list(int) = (col, ins) =>
        switch(col) {
          | [] => failwith("Move is not legal.")
          | [_] => [ins]
          | [hd,... tl] =>
            if (List.hd(tl) == 0) {[hd, ... addToColumn([List.hd(tl), ...List.tl(tl)], ins)]
            } else {[ins, ... tl]}};
      switch(s, m) {
        | (State(Ongoing(P1), b), Move(n)) => 
          let newBoard = List.mapi((i, col) => if (i == n - 1) {
            addToColumn(col, 1) } else {col}, b);
          if (check(0.0, allCombinations(newBoard), [1, 1, 1, 1]) > 0.0)
            {State(Win(P1), newBoard)} 
          else if (check(0.0, allCombinations(newBoard), [0]) == 0.0)
            {State(Draw, newBoard)} 
          else {State(Ongoing(P2), newBoard)}
        | (State(Ongoing(P2), b), Move(n)) => 
          let newBoard = List.mapi((i, col) => if (i == n - 1) {
            addToColumn(col, 2) } else {col}, b);
          if (check(0.0, allCombinations(newBoard), [2, 2, 2, 2]) > 0.0)
            {State(Win(P2), newBoard)} 
          else if (check(0.0, allCombinations(newBoard), [0]) == 0.0)
            {State(Draw, newBoard)} 
          else {State(Ongoing(P1), newBoard)}
        | _ => failwith("Move is not legal.")}};
    
    /* for transforming human player input into
    internal representation of move */
    /* moveOfString: string => move
     * input: a string, s, representing human input
     * output: an internal representation of the move implied by s
     */
    let moveOfString: string => move = s =>
      Move(try(int_of_string(s)) {
      | _ => failwith("Move must be represented by an integer.")});

    /* estimates the value of a given state (static evaluation) */
    /* estimateValue: state => float
     * input: a state, s
     * output: the value of s to player 1, represented as a positive float when
     *         player 1 is winning and a negative float when player 1 is losing
     *         with values ranging from infinity to negative infinity
     */
    let estimateValue: state => float = s => 
    switch(s) {
    | State(Win(P1), _) => infinity
    | State(Win(P2), _) => neg_infinity
    | State(Draw, _) => 0.0
    | State(Ongoing(_), b) => {
      let combos = allCombinations(b);
      (check(0.0, combos, [0, 1, 1, 1]))*.250.0 +.
      (check(0.0, combos, [1, 1, 1, 0]))*.250.0 +.
      (check(0.0, combos, [1, 1, 0, 1]))*.250.0 +.
      (check(0.0, combos, [1, 0, 1, 1]))*.250.0 +.

      (check(0.0, combos, [0, 0, 1, 1]))*.100.0 +.
      (check(0.0, combos, [1, 1, 0, 0]))*.100.0 +.
      (check(0.0, combos, [0, 1, 0, 1]))*.100.0 +.
      (check(0.0, combos, [1, 0, 1, 0]))*.100.0 +.
      (check(0.0, combos, [0, 1, 1, 0]))*.100.0 +.
      (check(0.0, combos, [1, 0, 0, 1]))*.100.0 +.

      (check(0.0, combos, [0, 0, 0, 1]))*.25.0 +.
      (check(0.0, combos, [0, 0, 1, 0]))*.25.0 +.
      (check(0.0, combos, [0, 1, 0, 0]))*.25.0 +.
      (check(0.0, combos, [1, 0, 0, 0]))*.25.0 -.

      (check(0.0, combos, [0, 2, 2, 2]))*.(250.0) -.
      (check(0.0, combos, [2, 2, 2, 0]))*.(250.0) -.
      (check(0.0, combos, [0, 2, 0, 2]))*.(250.0) -.
      (check(0.0, combos, [2, 0, 2, 0]))*.(250.0) -.

      (check(0.0, combos, [0, 0, 2, 2]))*.100.0 -.
      (check(0.0, combos, [2, 2, 0, 0]))*.100.0 -.
      (check(0.0, combos, [0, 2, 0, 2]))*.100.0 -.
      (check(0.0, combos, [2, 0, 2, 0]))*.100.0 -.
      (check(0.0, combos, [0, 2, 2, 0]))*.100.0 -.
      (check(0.0, combos, [2, 0, 0, 2]))*.100.0 -.

      (check(0.0, combos, [0, 0, 0, 2]))*.25.0 -.
      (check(0.0, combos, [0, 0, 2, 0]))*.25.0 -.
      (check(0.0, combos, [0, 2, 0, 0]))*.25.0 -.
      (check(0.0, combos, [2, 0, 0, 0]))*.25.0 }};
}
module MyGame : Game = Connect4;
open Connect4;

/* test cases */

/* stringOfPlayer test cases */
checkExpect(stringOfPlayer(P1), "Player 1", "stringOfPlayer test1");
checkExpect(stringOfPlayer(P2), "Player 2", "stringOfPlayer test2");

/* stringOfPiece test cases */
checkExpect(stringOfPiece(1), "\027[36m1\027[0m", "stringOfPiece test1");
checkExpect(stringOfPiece(2), "\027[91m2\027[0m", "stringOfPiece test2");
checkExpect(stringOfPiece(0), "0", "stringOfPiece test3");

/* transpose test cases */
checkExpect(transpose([[1, 3], [2, 4]]), [[1, 2], [3, 4]], "transpose test1");
checkExpect(transpose([[1, 1, 1, 1], [1, 1, 1, 1]]),
  [[1, 1], [1, 1], [1, 1], [1, 1]], "transpose test2");
checkError(() => transpose([]), "A board cannot be empty.");
checkError(() => transpose([[], []]), "A board cannot be 0-dimensional.");

/* stringOfState test cases */
checkExpect(stringOfState(State(Ongoing(P1), [[0], [0]])), "0 0 \n",
  "stringOfState test1");
checkExpect(stringOfState(State(Ongoing(P1), [[0, 0]])),
  "0 \n0 \n", "stringOfState test2");
checkExpect(stringOfState(State(Draw, [[1]])),
  "\027[36m1\027[0m \n", "stringOfState test3");

/* stringOfMove test cases */
checkExpect(stringOfMove(Move(1)), "Column 1", "stringOfMove test1");
checkExpect(stringOfMove(Move(7)), "Column 7", "stringOfMove test2");

/* fill test cases */
checkExpect(fill(0, 3), [0, 0, 0], "fill test1");
checkExpect(fill([0, 0], 3), [[0, 0], [0, 0], [0, 0]], "fill test2");

/* legalMoves test cases */
checkExpect(legalMoves(State(Ongoing(P1), [[0, 1], [2, 1], [0, 2]])), 
  [Move(1), Move(3)], "legalMoves test 1");
checkExpect(legalMoves(State(Win(P1), [[1, 1, 1, 1], [0, 2, 2, 2]])), [], 
  "legalMoves test2"); 
  
/* gameStatus test cases */
checkExpect(gameStatus(State(Win(P1), [[1, 1, 1, 1]])), Win(P1),
  "gameStatus test1");
checkExpect(gameStatus(State(Win(P2), [[2, 2, 2, 2]])), Win(P2),
  "gameStatus test2");
checkExpect(gameStatus(State(Draw, [[1]])), Draw,
  "gameStatus test3");
checkExpect(gameStatus(State(Ongoing(P2), [[0, 1, 0, 0]])), Ongoing(P2),
  "gameStatus test4");
checkExpect(gameStatus(State(Ongoing(P1), [[0, 2, 0, 0]])), Ongoing(P1),
  "gameStatus test5");

/* diagonals test cases */
checkExpect(diagonals([[1, 2, 3], [4, 5, 6], [7, 8, 9]]), 
  [[1], [2, 4], [3, 5, 7], [6, 8], [9], [7], [8, 4], [9, 5, 1], [6, 2], [3]], 
  "diagonals test1");
checkExpect(diagonals([[1, 2], [3, 4]]), [[1], [2, 3], [4], [3], [4, 1], [2]], 
  "diagonals test2");

/* check test cases */
checkExpect(check(0.0, [[0, 1, 1], [0, 1, 1]], [2, 1]), 0.0, "check test1");
checkExpect(check(0.0, [[0, 1, 1], [0, 1, 1]], [0, 1]), 2.0, "check test2");

/* allCombinations test cases */
checkExpect(allCombinations([[1, 2], [3, 4]]), 
  [[1, 2], [3, 4], [1, 3], [2, 4], [1], [2, 3], [4], [3], [4, 1], [2]],
  "allCombinations test1");
checkExpect(allCombinations([[0, 1]]), [[0, 1], [0], [1], [0], [1], [0], [1]],
  "allCombinations test2");

/* nextState test cases */
let testBoard = [[0, 1, 1, 1], [0, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]; 
checkExpect(nextState(State(Ongoing(P1), [[0, 1, 2], [0, 1, 1], [0, 2, 2]]), 
  Move(2)), State(Ongoing(P2), [[0, 1, 2], [1, 1, 1], [0, 2, 2]]), 
  "nextState test 1");
checkExpect(nextState(State(Ongoing(P1), testBoard), Move(1)), 
  State(Win(P1), [[1, 1, 1, 1], [0, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]), 
  "nextState test 2");
checkError(() => nextState(State(Draw, [[1]]), Move(1)), "Move is not legal.");

/* moveOfString test cases */
checkExpect(moveOfString("1"), Move(1), "moveOfString test1");
checkExpect(moveOfString("0"), Move(0), "moveOfString test2");
checkError(() => moveOfString("a"), "Move must be represented by an integer.")

/* estimateValue test cases */
checkExpect(estimateValue(State(Win(P1), [[1, 1, 1, 1]])), infinity,
  "estimateValue test1");
checkExpect(estimateValue(State(Win(P2), [[2, 2, 2, 2]])), neg_infinity,
  "estimateValue test2");
checkExpect(estimateValue(State(Draw, [[1]])), 0.0,
  "estimateValue test3");
checkExpect(estimateValue(State(Ongoing(P2), [[0, 1, 0, 0]])), 25.0,
  "estimateValue test4");
checkExpect(estimateValue(State(Ongoing(P1), [[0, 2, 0, 0]])), -25.0,
  "estimateValue test5"); 