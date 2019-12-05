open CS17SetupGame;   
open SigGame; 

module Connect4 = {
  let initialRows = 5; 
  let initialCols = 7; 
  let names = "wchen77 jlim37"

                                /* TYPES */
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's still ongoing */
  type status =
    | Win(whichPlayer) /* this says who wins */
    | Draw
    | Ongoing(whichPlayer); /* this says whose turn it is */

  /* this says status of the game and how the connect 4 board currently looks 
  like */  
  type state = State(status, list(list(int))); 

  /* describes a move that a player can make */
  type move = Move(int);

                             /* HELPER FUNCTIONS */
  /* TRANPOSE
  data definition
  example data:
  list(list(int)): [[1,2,3,4,5]], [[1,2,3,4],[5,6,7,8],[9,10,11,12]], [[1]]

  transpose: list(list(int)) -> list(list(int))

  input: a list(list(int)), matrix, a list of list of numbers, first list of 
  numbers represents the first row of matrix, second list of numbers represents 
  the second row of the matrix, and so on

  output: a list(list(int)) that results from transposing the input matrix

  Recursion Diagram:
  OI:[[1,2],[3,4]]
  RI:[[2],[4]]
  RO:[[2,4]]
  IS: append the "list containing the list of first elements of OI" to "RO"
  OO:[[1,3],[2,4]]
  */

  let rec transpose: list(list(int)) => list(list(int)) = matrix => 
  switch (matrix) {
  | [[_], ..._] => [List.flatten(matrix)]
  | [[_, ..._], ..._] => 
      [List.map(List.hd, matrix), ...transpose(List.map(List.tl, matrix))]
  | _ => failwith("tranpose: matrix invalid")
  };

  /* VERTFLIP
  vertFlip: list(list(int)) -> list(list(int))

  input: a list(list(int)), matrix, a list of list of numbers, first list of 
  numbers represents the first row of matrix, second list of numbers represents 
  the second row of the matrix, and so on

  output: a list(list(int)) that results from reversing the order of the 
  columns in the input matrix
  */

  let vertFlip: list(list(int)) => list(list(int)) = matrix => 
  switch (matrix) {
  | [_, ..._] => List.map(List.rev,matrix)
  | [] => failwith("vertFlip: matrix invalid")
  };

                            /* TYPE CONVERSIONS */
  /* STRINGOFPLAYER
  stringOfPlayer: whichPlayer -> string
  
  input: a whichPlayer
  
  output: the string form of the player in the input
  */

  let stringOfPlayer: whichPlayer => string = fun
  | P1 => "Player 1 (Red)"
  | P2 => "Player 2 (Yellow)";
  
  /* STRINGOFSTATE
  stringOfState: state ->  string
  
  input: a state, currState
  
  output: the string form of representing the current board and the static 
  evaluation of the current board.
  */

  let stringOfState: state => string = currState => 
  {
    let State(_, board) = currState;
    /* string form of one row on the board */
    let rowString: list(int) => string = rowStr => 
      "| " ++ String.concat(" ",   
      List.map(x => 
      if (x == 1) { "\027[31m" ++ string_of_int(0) ++ "\027[0m"}
      else if (x == 2) { "\027[33m" ++  string_of_int(0) ++ "\027[0m" }
      else { string_of_int(x) }, rowStr)) ++ " |";
    /* layers each row into the right position on the board */
    let completeString: list(list(int)) => string = complete => 
      String.concat("\n", List.map(rowString, complete));
    /* rest of program for stringOfState  */
    completeString(transpose(board));
  };
  
  /* STRINGOFMOVE
  stringOfMove: move -> string
  
  input: a move
  
  output: the string form of the move, indicating where the checker was dropped
  */

  let stringOfMove: move => string = fun
  | Move(int) => string_of_int(int);
   
  /* MOVEOFSTRING for transforming human player input into internal 
  representation of move
  
  moveOfString: string -> move
  
  input: a string, representing a player's move
  
  output: a move, the abstract form representing the player's move
  */

  let moveOfString: string => move = input => Move(int_of_string(input));



                                /* GAME LOGIC */

  /* INITIALSTATE the state of the game when it begins.
  Uses makeColumns and makeRows to create a board of desired size.
  Abstract form of board is the tranposed version. 
  The first list(int) represents the first column, and so on.*/

  let initialState: state = 
  {
    /* makes a list representing each column using number of rows on the board*/
    let rec makeColumns: int => list(int) = y =>
    if (y == 1) {[0]} else if (y > 1) {[0, ...makeColumns(y - 1)]} 
    else {failwith("invalid row size")};
    /* completes the board-making by creating the correct number of columns*/
    let rec makeRows: (int, list(int)) => list(list(int)) = (x, col) =>
    if (x == 1) {[col]} else if (x > 1) {[col, ...makeRows(x - 1, col)]} 
    else {failwith("invalid column size")};    
    /* rest of program for initialState */
    State(Ongoing(P1), makeRows(initialCols, makeColumns(initialRows)))
  };

  /* LEGALMOVES produces the list of legal moves at a state 

  legalMoves: state -> list(move)
  
  input: a state, input
  
  output: a list of moves, representing the list of legal moves for the given 
  input state.
  */

  let legalMoves: state => list(move) = input => 
  {
    let rec movesHelper: (list(list(int)), int) => list(move) = (st, num) =>
    switch (st) {  
    | [] => [] 
    | [col, ...rest] => 
        if (List.hd(col) == 0) {[Move(num), ...movesHelper(rest, num + 1)]}
        else {movesHelper(rest, num + 1)}                    
    };
    switch(input) {
    | State(_, board) => movesHelper(board, 1)
    }
  };

  /* GAMESTATUS returns the status of the game at the given state

  gameStatus: state -> status
  
  input: a state
  
  output: extracts the status from the input state
  */

  let gameStatus: state => status = fun | State(stat, _) => stat;

  /* ZAP creates a list of int lists that represents the diagonals of the board
  
  zap : list('a)* list('b) * (('a,'b) -> 'b) -> list('b)
  
  input: a list('a), items, a k-element list. a list('b), things, an n-element 
  list where n>=k. a (('a,'b) -> 'b) procedure, op.
  
  output: a list of length n, whose first k elements are the result of applying 
  op to corresponding elements in items and things. The final n-k elements are 
  the last n-k elements in the original input things list.

  Recursion Diagram:
  OI: [1, 2, 4], [3, 4, 6, 7], (+)
   RI: [2, 4], [4, 6, 7], (+)
   RO: [6, 10, 7]
  IS: cons "the output of applying procedure to head of items and head of 
      things" to RO
  OO: [4, 6, 10, 7]
  */

  let rec zap: (list('a), list('b), ('a,'b) => 'b) => list('b) = 
    (items, things, op) =>
  switch(items, things) {
  | ([], _) => things
  | ([hd1, ...tl1], [hd2, ...tl2]) => [op(hd1, hd2), ...zap(tl1, tl2, op)] 
  | (_, []) => failwith("First list must be shorter!")
  };

  /* makeLeftDiag makes a list of int lists representing diagonals from
  upper right to bottom left of board*/
  /* 
  makeLeftDiag: list(list(int)) -> list(list(int))
  
  input: a list(list(int)) with each outer list element the same length, board
  
  output: a list(list(int)) with each element
  representing board diagonals from the upper right 
  corner to bottom left corner

  Recursion Diagram:
  OI: 
  [[1, 2, 3, 4],
   [2, 3, 4, 5],
   [3, 4, 5, 6]]
   RI: 
   [[2, 3, 4, 5],
   [3, 4, 5, 6]]
   RO: [[2], [3, 3], [4, 4], [5, 5], [6]]
  IS: apply "zap" proc to head of orig input and rec output with an empty element cons on to it
  OO: [[1], [2, 2], [3, 3, 3], [4, 4, 4], [5, 5, 5], [6]]
  */

  let rec makeLeftDiag: list(list(int)) => list(list(int)) = board =>
  switch (board) {
  | [col1] => List.map((x) => [x], col1)  
  | [hdCol, ...tl] => 
    zap(hdCol, [[], ...makeLeftDiag(tl)], (x, y) => [x, ...y])
  | _ => failwith("Invalid board in make diagonals")
  };
  
 /* makeRightDiag makes a list of int lists representing diagonals from
  upper left to bottom right of board*/
  /* 
  makeRightDiag: list(list(int)) -> list(list(int))
  
  input: a list(list(int)), board
  
  output: a list(list(int)) with each element 
  representing board diagonals from the upper left 
  corner to bottom right corner
  */

  let makeRightDiag: list(list(int)) => list(list(int)) = board =>
  {
    let flippedBoard = vertFlip(board);
    makeLeftDiag(flippedBoard)
  };
  
  /* NEXTSTATE given a state and a legal move, yields the next state
  
  nextState: state * move -> list(list(int))
  
  input: a state, oldState. a move, playerMove, this move must be a legal move 
  for the given oldState.
  
  output: the state of the game after the the legal move has been made
  */
  let nextState: (state, move) => state = (oldState, playerMove) => 
  {
    /* newBoard outputs the board after legal move has been made*/
    let rec newBoard: (state, move) => list(list(int)) = (st, moveNum) => 
    {
      let rec addPiece: (whichPlayer, list(int)) => list(int) = 
        (player, column) =>
      switch (column) {
      | [0] => if (player == P1) {[1]} else {[2]};
      | [hd, ...tl] => if (List.hd(tl) == 0) {[hd, ...addPiece(player, tl)]} 
                       else if (player == P1) {[1, ...tl]} else {[2, ...tl]};
      | _ => failwith("addpiece error")                  
      };
      /* rest of program for newBoard */
      switch (st, moveNum) {
      | (State(Ongoing(player), [hd, ...tl]), Move(1)) => 
          [addPiece(player, hd), ...tl]
      | (State(Ongoing(player), [hd, ...tl]), Move(n)) => 
          [hd, ...newBoard(State(Ongoing(player), tl), Move(n - 1))]
      | _ => failwith("newboard error")
      }
    };
    /* checkWin determines if the board, after the legal move, has any winning 
    combinations */
    let checkWin: (whichPlayer, list(list(int)), status) => status = 
      (player, board, prevStat) => 
    {
      /* fourConnect checks if any list of integers has a winning combination */
      let rec fourConnect: (list(int), int, int) => bool = 
        (input, pl, count) => 
      switch (input, count) {
      | (_, 0) => true
      | ([], _) => false
      | ([hd, ...tl], _) => if (hd == pl) {fourConnect(tl, pl, count - 1)} 
                            else {fourConnect(tl, pl, 4)}
      };
      
      /* fourConnectList checks if any list of integers has a winning 
      combination */
      let rec fourConnectList: (list(list(int)), int) => bool = (input, pl) =>
      switch (input) {
      | [] => false
      | [hd, ...tl] => fourConnect(hd, pl, 4) || fourConnectList(tl, pl)
      };
      
      /* colCheck checks if any column has a winning combination */
      let colCheck: (list(list(int)), int) => bool = (board, player) => 
        fourConnectList(board, player);
      
      /* rowCheck checks if any row has a winning combination */
      let rowCheck: (list(list(int)), int) => bool = (board, player) => 
        fourConnectList(transpose(board), player);
      
      /* diagCheck checks if any diagonal has a winning combination */
      let diagCheck: (list(list(int)), int) => bool = (board, player) => 
        fourConnectList(makeRightDiag(board), player) || 
          fourConnectList(makeLeftDiag(board), player);

      /* rest of program for checkWin */
      if (player == P1) {
        if (colCheck(board, 1) || rowCheck(board, 1) || diagCheck(board, 1)) {
          Win(P1)
        } else if (legalMoves(State(prevStat, board)) == []) {Draw}
          else {Ongoing(P2)}
      } else {
        if (colCheck(board, 2) || rowCheck(board, 2) || diagCheck(board, 2)) {
          Win(P2)
        } else if (legalMoves(State(prevStat, board)) == []) {Draw}
          else {Ongoing(P1)}
      };
    }
    /* rest of program for nextstate */
    switch (oldState){
    | State(Ongoing(player), _) => 
        State(checkWin(player, newBoard(oldState, playerMove), Ongoing(player)),
              newBoard(oldState, playerMove))
    | State(Win(player), board) => State(Win(player), board)
    | State(Draw, board) => State(Draw, board)
    }
  };

  
  /* ESTIMATEVALUE the value of a given state (static evaluation)
  
  estimateValue: state -> float
  
  input: a state, input. 
  
  output: a float representing the estimated value of the input state. This is 
  a static evaluation.
  */

  let estimateValue: state => float = input =>
  {
    /* vertEst estimates a score from all the vertical (column) combinations 
    on the board */
    let rec vertEst: list(list(int)) => float = stat =>
    switch (stat) {
    | [] => 0.0
    | [hd, ...tl] => 
      {
        let rec vertEstHelper: (list(int), int) => float = (aloi, zeroes) =>
        switch (aloi, zeroes) {
        | ([0], _) => 0.0
        | ([0, ...tl], n) => vertEstHelper(tl, n + 1)
        | ([1, 1, 1, 1, ..._], _) => infinity
        | ([1, 1, 1, ..._], n) => if (n >= 1) {150.0} else {0.0};       
        | ([1, 1, ..._], n) => if (n >= 2) {20.0} else {0.0};
        | ([1, ..._], n) => if (n >= 3) {2.0} else {0.0};
        | ([2, 2, 2, 2, ..._], _) => neg_infinity
        | ([2, 2, 2, ..._], n) => if (n >= 1) {-150.0} else {0.0}  ;      
        | ([2, 2, ..._], n) => if (n >= 2) {-20.0} else {0.0};
        | ([2, ..._], n) => if (n >= 3) {-2.0} else {0.0};
        | (_, _) => 0.0   
        };
        vertEstHelper(hd, 0) +. vertEst(tl)
      }
    };
  
    /* horDiagEst estimates scores from either horizontals or diagonals by 
    assessing every four adjacent/diagonal pieces on the board */
    let rec horDiagEst: list(list(int)) => float = stat =>
    switch (stat) {
    | [] => 0.0
    | [hd, ...tl] => 
      {
        let rec horDiagEstHelper: (list(int), float) => float = (aloi, score) =>
        switch (aloi) {
        | [first, second, third, fourth, ...rest] => 
            switch (first, second, third, fourth) {
            | (1, 1, 1, 1) => infinity
            | (1, 1, 1, 0) | (1, 1, 0, 1) | (1, 0, 1, 1) |(0, 1, 1, 1) => 
                horDiagEstHelper([second, third, fourth, ...rest], 
                                  120.0 +. score)
            | (1, 1, 0, 0) | (1, 0, 0, 1) | (0, 0, 1, 1) | (1, 0, 1, 0) 
                | (0, 1, 0, 1) | (0, 1, 1, 0) => 
                horDiagEstHelper([second, third, fourth, ...rest], 
                                  15.0 +. score)
            | (1, 0, 0, 0) | (0, 1, 0, 0) | (0, 0, 1, 0) | (0, 0, 0, 1) => 
                horDiagEstHelper([second, third, fourth, ...rest], 2.0 +. score)
            | (2, 2, 2, 2) => neg_infinity
            | (2, 2, 2, 0) | (2, 2, 0, 2) | (2, 0, 2, 2) |(0, 2, 2, 2) => 
                horDiagEstHelper([second, third, fourth, ...rest], 
                                  -120.0 +. score)
            | (2, 2, 0, 0) | (2, 0, 0, 2) | (0, 0, 2, 2) | (2, 0, 2, 0) 
                | (0, 2, 0, 2) | (0, 2, 2, 0) => 
                horDiagEstHelper([second, third, fourth, ...rest], 
                                  -15.0 +. score)
            | (2, 0, 0, 0) | (0, 2, 0, 0) | (0, 0, 2, 0) | (0, 0, 0, 2) => 
                horDiagEstHelper([second, third, fourth, ...rest], 
                                  -2.0 +. score) 
            | (_, _, _, _) => horDiagEstHelper([second, third, fourth, ...rest],
                                                score)
            }
        | _ => score    
        };
        horDiagEstHelper(hd, 0.0) +. horDiagEst(tl)
      }
    };
    /* rest of program for estimateValue */
    switch (input) {
    | State(_, board) => vertEst(board) +. horDiagEst(transpose(board)) +. 
                         horDiagEst(makeRightDiag(board)) +. 
                         horDiagEst(makeLeftDiag(board))
    }
  };
};

module MyGame : Game = Connect4;
open Connect4;


/* test cases for stringOfPLayer */
checkExpect(stringOfPlayer(P1), "Player 1 (Red)","stringOfPlayer player 1");

checkExpect(stringOfPlayer(P2), "Player 2 (Yellow)","stringOfPlayer player 2");

/* test cases for stringOfMove */
checkExpect(stringOfMove(Move(1)), "Checker dropped into column 1",
  "stringOfMove Move1");

checkExpect(stringOfMove(Move(5)), "Checker dropped into column 5",
  "stringOfMove Move5");

/*Test Cases for transpose*/
checkExpect(transpose([[1]]), [[1]], "check 1by1  matrix");

checkExpect(transpose([[1],[2],[3],[4],[5]]), [[1,2,3,4,5]], 
  "check one column matrix");

checkExpect(transpose([[1,2,3,4,5]]), [[1],[2],[3],[4],[5]], 
  "check one row matrix");

checkExpect(transpose([[1,2],[3,4]]), [[1,3],[2,4]], 
  "check 2by2 square matrix");

checkExpect(transpose([[1,2,3],[4,5,6],[7,8,9]]), [[1,4,7],[2,5,8],[3,6,9]], 
  "check 3by3 square matrix");

checkExpect(transpose([[1,2,3],[4,5,6],[7,8,9],[10,11,12]]), 
  [[1,4,7,10],[2,5,8,11],[3,6,9,12]], "check tall rectangular matrix");

checkExpect(transpose([[1,2,3,4],[5,6,7,8],[9,10,11,12]]), 
  [[1,5,9],[2,6,10],[3,7,11],[4,8,12]], "check long rectangular matrix");

/* test cases for zap */
checkExpect(zap([], [2, 3], (-)), [2, 3],"zap empty items list");

checkExpect(zap([], [], (+)), [], "zap empty things list");

checkExpect(zap([1], [1, 2, 3], (+)), [2, 2, 3], "zap one element items list");

checkExpect(zap([3], [5], ( * )), [15], "zap one element things list");

checkExpect(zap([1, 2, 4], [3, 4, 6, 7], (+)), [4, 6, 10, 7], 
  "zap multi element items list");

checkExpect(zap([1, 2, 4, 6], [3, 4, 6, 7], (+)), [4, 6, 10, 13], 
  "zap lists of equal length");

checkError(() => zap([1, 2, 4, 6], [3, 4, 6], (-)), 
  "First list must be shorter!");

/* test cases for makeRightDiag */
checkExpect(makeRightDiag([[3, 2, 1],[6, 5, 4],[9, 8, 7]]), 
  [[1], [2, 4], [3, 5, 7], [6, 8], [9]], "3x3 rightdiag"); 

checkExpect(makeRightDiag([[3, 2, 1],[6, 5, 4],[9, 8, 7],[12, 11, 10]]), 
  [[1], [2, 4], [3, 5, 7], [6, 8, 10], [9, 11], [12]], "4x3 rightdiag"); 

checkExpect(makeRightDiag([[4, 3, 2, 1],[8, 7, 6, 5],[12, 11, 10, 9]]), 
  [[1], [2, 5], [3, 6, 9], [4, 7, 10], [8, 11], [12]], "3x4 rightdiag");

checkExpect(makeRightDiag([[2, 1],[4, 3]]), 
  [[1], [2, 3], [4]], "2x2 rightdiag"); 

checkExpect(makeRightDiag([[2],[4]]), 
  [[2], [4]], "2x1 rightdiag"); 

checkExpect(makeRightDiag([[5, 4, 3, 2, 1]]), 
  [[1], [2], [3], [4], [5]], "1x5 rightdiag");

checkError(() => makeRightDiag([]), "vertFlip: matrix invalid");

/* test cases for makeLeftDiag */
checkExpect(makeLeftDiag([[1,2,3],[4,5,6],[7,8,9]]), 
  [[1], [2, 4], [3, 5, 7], [6, 8], [9]], "3x3 leftdiag"); 

checkExpect(makeLeftDiag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]]), 
  [[1], [2, 4], [3, 5, 7], [6, 8, 10], [9, 11], [12]], "4x3 leftdiag"); 

checkExpect(makeLeftDiag([[1,2,3,4],[5,6,7,8],[9,10,11,12]]), 
  [[1], [2, 5], [3, 6, 9], [4, 7, 10], [8, 11], [12]], "3x4 leftdiag");

checkExpect(makeLeftDiag([[1,2],[3,4]]), 
  [[1], [2, 3], [4]], "2x2 leftdiag"); 

checkExpect(makeLeftDiag([[2],[4]]), 
  [[2], [4]], "2x1 leftdiag"); 

checkExpect(makeLeftDiag([[1,2,3,4,5]]), 
  [[1], [2], [3], [4], [5]], "1x5 leftdiag");

checkError(() => makeLeftDiag([]), "Invalid board in make diagonals");

/* test cases for moveOfString */
checkExpect(moveOfString("1"), Move(1), "moveOfString Column 1");

checkExpect(moveOfString("4"), Move(4), "moveOfString Column 4");

checkExpect(moveOfString("6"), Move(6), "moveOfString Column 6");