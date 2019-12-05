open!  CS17SetupGame;   
open SigGame; 

module Connect4 = 
{
  /* Configurable dimensions (rows and columns) for the game */
  let initialRows = 5;
  let initialCols = 7;

  let names = "aswanso2 mlou2";

  /* Data Definition: a whichPlayer represents the 2 players in Connect 4: 
   * P1 is player 1, P2 is player 2 */
  type whichPlayer =
    | P1
    | P2;

  /* Data Definition: a status specifies the status of the game with 3 cases: 
   * Win(whichPlayer), Draw, Ongoing(whichPlayer), with the whichPlayer 
   * constructor indicating which player is affected by the status */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer); 

  /* Data Definition: a board represents a Connect 4 board using a list of list 
   *  of ints. Each inner list represents a column; the first list represents 
   *  the leftmost column in a Connect 4 board. The last elements within each  
   *  inner list represent the bottom of the board and the first elements the  
   *  top. The int elements represent the state of the position in the board, 
   *  with 0 representing empty, 1 representing the position being taken by 
   *  Player 1, 2 representing the position being taken by player 2
   * Minimum board size is 4 x 4 for Connect 4. Any smaller dimensions are
   * considered invalid boards. 
   * Example boards: [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
   *      [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]];
   *  [[0, 0, 1, 2, 1], [0, 0, 2, 2, 1], [0, 0, 0, 0, 2], 
   *      [0, 0, 0, 0, 1], [0, 0, 0, 0, 0], [0, 1, 2, 2, 2], [0, 0, 2, 1, 1]]
   */
  type board = list(list(int));
  
  /* Data Definition: a state includes the status of the game and the board at 
   *  a given instant. Status describes whether the same is at a draw, win, 
   *  or ongoing, indicating the current player for the later two. 
   *  The board describes the current board.
   *
   * Example Data: 
   *  State(Win(P1), [[0, 0, 0, 0, 0], [0, 1, 1, 1, 1], [0, 0, 0, 2, 2], 
   *      [0, 0, 0, 0, 0], [0, 0, 0, 2, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
   *  State(Win(P2), [[0, 0, 0, 0, 0], [0, 2, 2, 2, 2], [0, 0, 0, 1, 1], 
   *      [0, 0, 0, 0, 0], [0, 0, 0, 1, 1], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
   *  State(Ongoing(P1), [[0, 0, 0, 0, 0], [0, 0, 1, 1, 1], [0, 0, 0, 2, 2], 
   *      [0, 0, 0, 0, 0], [0, 0, 0, 2, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
   *  State(Ongoing(P2), [[0, 0, 0, 0, 0], [0, 0, 1, 1, 1], [0, 0, 0, 2, 2], 
   *      [0, 0, 0, 0, 0], [0, 0, 0, 2, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
   *  State(Draw, [[1, 1, 2, 2, 1], [1, 1, 2, 2, 1], [1, 1, 2, 2, 1], 
   *      [2, 2, 1, 1, 2], [2, 2, 1, 1, 2], [2, 2, 1, 1, 2], [1, 1, 2, 2, 1]])*/
  type state = State(status, list(list(int))); 

  /* Data Definition: a move is the integer number of the column (starting at 1   
   * from left to right) a player can drop his/her piece in 
   * Example data: 1, 2, 3, 4, 5, 6, 7 */
  type move = int;
  
  /* Input: a whichPlayer, p, indicating P1 or P2 
   * Output: the string result of translating P1 as "Player 1" and P2 as 
   *  "Player 2" */
  let stringOfPlayer: whichPlayer => string = p =>
  switch(p)
  {
    | P1 => "Player 1"
    | P2 => "Player 2"
  };

  /* Input: a game state, s
   * Output: the string output of the state, in the form of board and the game 
   *  status (who is winning, draw, ongoing and whose turn). The board will be 
   *  printed as a matrix of numbers, with 0 being empty, 1 being P1's piece
   *  2 being P2's piece, with a message following the board indicating the 
   *  current game status as described. */
  let stringOfState: state => string = s =>
  {
    /* transpose: helper function of stringOfState that transposes the 
     *   orientation of the list of lists by matrix rules of transposing */
    let rec transpose: board => board = mat =>
    switch (mat) 
    {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
      | [[_], ..._] => [List.map(x => List.hd(x), mat)]
      | [[_, ..._], ..._] => List.append([List.map(x => List.hd(x), mat)], 
            transpose(List.map(y => List.tl(y), mat)))
    };
    /* printRow: helper function of stringOfState that turns a list of ints  
     *   into a string of the int elements inside the input list */
    let rec printRow: list(int) => string = l =>
    switch(l)
    {
      | [] => ""
      | [hd] => string_of_int(hd)
      | [hd, ... tl] => string_of_int(hd) ++ "  " ++ printRow(tl)
    }
    /* printBoard: helper function of stringOfState that turns a board into a   
     *   tring of the int elements inside the input board. Calls printRow on
     *   each element list inside input board and organizes those lists into
     *   grid-like form for readability. */
    let rec printBoard: board => string = lst =>
    switch(lst)
    {
      | [] => ""
      | [hd, ...tl] => printRow(hd) ++ "\n" ++ printBoard(tl)
    }
    switch(s)
    {
      | State(Ongoing(p), b) => 
          stringOfPlayer(p) ++ "'s current board: \n" ++ 
            printBoard(transpose(b))
      | State(Draw, b) => 
         "Game has ended in Draw" ++ "\n" ++ printBoard(transpose(b))
      | State(Win(p), b) => 
          stringOfPlayer(p) ++ " has won!" ++ "\n" ++ 
            printBoard(transpose(b))
    }
  };

  /* Input: a move, m 
   * Output: the string translation of input move, m*/
  let stringOfMove: move => string = m =>
    string_of_int(m);



/* Game Logic */

  /* Input: an (int, int) tuple, (row, col), representing the dimensions of the 
   *   game board with row = number of rows and col = number of columns 
   * Output: An empty Connect 4 board with the input dimensions (row, col). All
   *  positions will be filled with 0's at the beginning to represent an empty
   *  Connect 4 board.
   */
  let makeBoard: (int, int) => board = (row, col) =>
  {
    /* addRows: helper function of makeBoard -- creates one column (list(int))
     *   of the appropriate length of the empty board */
    let rec addRows: int => list(int) = row =>
    if(row == 0) {[]}
    else
    {
      [0, ...addRows(row - 1)]
    };
    /* addCols: helper function of makeBoard -- creates the empty board 
     *   (list(list(int))) of the appropriate number of columns by calling 
     *   addRows the input int amount of times */
    let rec addCols: (int, list(int)) => board = (c, temp) =>
    if (c == 0) {[]}
    else
    {
      [temp, ... addCols(c - 1, temp)]
    };
    addCols(col, addRows(row));
  };


  /* Definition of an initial state for the game: 
   *  The initial state is Ongoing(P1) status (Player 1 goes first), with an 
   *  empty board of the initial dimensions set above in initialRows and 
   *  initialCols */
  let initialState: state = 
    State(Ongoing(P1), makeBoard(initialRows, initialCols)); 


  /* Input: a state, s
   * Output: a list of moves that are allowed for input state s. A move is 
   *  defined as legal if the column in which the piece is to be placed still 
   *  has empty spaces (0 at the beginning of the column list = still open) */
  let legalMoves: state => list(move) = s => 
  {
    /* lHelper: helper function of legalMoves -- takes in a board and an index 
     *   counter value that increments through the columns; this counter thus
     *   represents which columns are open and if open, the value of the counter 
     *   is added onto the output list */
    let rec lHelper: (board, int) => list(move) = (lst, index) =>
    {
      switch (lst, index)
      {
        | ([], _) => []
        | ([hd, ... tl], i) => 
        if (List.hd(hd) == 0) 
        {[i, ... lHelper(tl, i + 1)]}
        else
        {lHelper(tl, i + 1)}
      }
    }
    switch(s)
    {
      | State(Ongoing(_), b) => lHelper(b, 1)
      | State(Win(_), _) => []
      | State(Draw, _) => []
    }
  };


  /* Input: a game state, s
   * Output: the status component of input game state s (Win(whichPlayer), 
   *  Draw, or Ongoing(whichPlayer) */
  let gameStatus: state => status = s =>
  {
    let State(stat, _) = s;
    stat;
  };


  /* Input: a state, s, and a valid move, m, for Connect 4
   * Output: the new state after m is incorporated into the board of input 
   *  state s and the turn of the player is switched from P1 to P2 or P2 to P1
   *  if a player has not won yet. If there is a winner after updating the game 
   *  board, the game status of the output state is changed to the appropriate 
   *  status (Win(P1), Win(P2), or Draw) */
  let nextState: (state, move) => state = (s, m) =>
  {
    /* changeCol: helper function of nextState - takes the int representing the
     *   player (1 or 2), an int counter that counts which element of the  
     *   column was modified, and the column list(int) that is to be changed.
     *   Function outputs the changed column and the element of the column that
     *   the new piece was placed in. */
    let rec changeCol: (int, int, list(int)) => (list(int), int) = 
      (i, r, col) =>
    switch(col)
    {
      | [0] => ([i], r)
      | [hd1, hd2, ... tl] => 
        if(hd1 == 0 && hd2 != 0)
        {([i, hd2, ... tl], r)}
        else
        {
          let (rst, row) = changeCol(i, r + 1, [hd2, ... tl]);
          ([hd1, ...rst], row)
        }
      | _ => failwith("Exception to make match cases exhaustive")
    };
    /* newBoard: helper function of nextState - takes the int representing which
     *   player's piece is being added to the board (1 or 2), an int counter   
     *   that counts which element of the column was modified, and the board 
     *   that is to be changed.
     *   Function outputs the changed board and the row number that
     *   the new piece was placed in. */
    let rec newBoard: (int, int, board) => (board, int) = 
      (p, r, b) =>
    if (List.length(List.tl(b)) == initialCols - m)
    {
      let (newCol, row) = changeCol(p, r, List.hd(b));
      ([newCol, ... List.tl(b)], row);
    }
    else
    {
      let (b1, row) = newBoard(p, r, List.tl(b));
      ([List.hd(b), ... b1], row)
    };
    /* colCheck: helper function of nextState - takes the column to be checked,
     *   a counter int, and the int representation of player (1 or 2) for which   
     *   it is checking pieces of. 
     *   Function outputs a boolean: true if it found 4 consecutive 1's or 2's
     *   in a list(int) column and false otherwise. */
    let rec colCheck: (list(int), int, int) => bool = (l, count, p) =>
    switch(l, count)
    {
      | ([], _) => false
      | ([hd, ... tl], r) => 
        if(hd != p)
        {
          if (List.length(tl) < 4) {false}
          else {colCheck(tl, 0, p)}
        }
        else
        {r == 3 || colCheck(tl, r + 1, p)}
    };
    /* diagCheckLeft: helper function of nextState - takes in the board, the
     *   int representation of player (1 or 2) for which it is checking pieces 
     *   of, an int (counter), and the row number of the new piece that was  
     *   just placed in the board.
     *   Function outputs a boolean: true if it found 4 consecutive 1's or 2's
     *   in a diagonal of top right to bottom left direction and false 
     *   otherwise. */  
    let diagCheckLeft: (board, int, int, int) => bool = 
      (l, p, count, r) => 
    {
      let rec diagLHelper: (board, int, int, int) => bool = 
      (b, counter, c1, r1) =>
      switch(c1, r1)
      {
        | (0, y) => List.nth(List.nth(b, 0), y) == p && counter == 3
        | (x, y) when y == initialRows - 1 => List.nth(List.nth(b, x), y) == p  
            && counter == 3
        | (x, y) => 
          if (List.nth(List.nth(b, x), y) == p)
          {counter == 3 || diagLHelper(b, counter + 1, x - 1, y + 1)}
          else
          {diagLHelper(b, 0, x - 1, y + 1)}
      };
      if ((r + m >= 4) && (r + m <= initialRows + initialCols - 4)) 
      {
        if (r + m - 1 < initialCols) 
        {diagLHelper(l, count, r + m - 1, 0)}
        else
        {diagLHelper(l, count, initialCols - 1, r + m - initialCols)}
      } 
      else
      {false};
    };
    /* diagCheckRight: helper function of nextState - takes in the board, the
     *   int representation of player (1 or 2) for which it is checking pieces 
     *   of, an int (counter), and the row number of the new piece that was  
     *   just placed in the board.
     *   Function outputs a boolean: true if it found 4 consecutive 1's or 2's
     *   in a diagonal of top left to bottom right direction and false 
     *   otherwise. */
    let diagCheckRight: (board, int, int, int) => bool = 
      (l, p, count, r) => 
    {
      let rec diagRHelper: (board, int, int, int) => bool = 
        (b, counter, c1, r1) =>
      switch(c1, r1)
      {
        | (x, y) when x == initialCols - 1 => List.nth(List.nth(b, x), y) == p 
          && counter == 3
        | (x, y) when y == initialRows - 1 => List.nth(List.nth(b, x), y) == p 
          && counter == 3
        | (x, y) => 
          if (List.nth(List.nth(b, x), y) == p)
          {counter == 3 || diagRHelper(b, counter + 1, x + 1, y + 1)}
          else
          {diagRHelper(b, 0, x + 1, y + 1)}
      };
      if ((r > m - 1 && r - m + 1 > initialRows - 4) || 
          (r < m - 1 && m - 1 - r > initialCols - 4))
      {false}
      else
      {
        if (r <= m - 1) 
        {diagRHelper(l, count, m - 1 - r, 0)}
        else
        {diagRHelper(l, count, 0, r - m + 1)}  
      };
    };
    switch(s)
    {
    | State(Win(_), _) => failwith("The game has already ended in a win")
    | State(Draw, _) => failwith("The game has already ended in a Draw")
    | State(Ongoing(P1), b) => 
        {
          let (newB1, row1) = newBoard(1, 0, b);
          if(colCheck(List.nth(newB1, m - 1), 0, 1) ||
             colCheck(List.map(x => List.nth(x, row1), newB1), 0, 1) ||
             diagCheckLeft(newB1, 1, 0, row1) ||
             diagCheckRight(newB1, 1, 0, row1))
          {State(Win(P1), newB1);}
          else if (legalMoves(State(Ongoing(P2), newB1)) == [])
          {State(Draw, newB1)}
          else
          {State(Ongoing(P2), newB1)}
        }
    | State(Ongoing(P2), b) =>
        {
          let (newB2, row2) = newBoard(2, 0, b);
          if(colCheck(List.nth(newB2, m - 1), 0, 2) ||
             colCheck(List.map(x => List.nth(x, row2), newB2), 0, 2) ||
             diagCheckLeft(newB2, 2, 0, row2) ||
             diagCheckRight(newB2, 2, 0, row2))
          {State(Win(P2), newB2);}
          else if (legalMoves(State(Ongoing(P1), newB2)) == [])
          {State(Draw, newB2)}
          else
          {State(Ongoing(P1), newB2)}
        }
    } 
  };
 



  /* Input: a string, s, representing a human input move
   * Output: a move translation of the input s. If the move is not an integer
   *  between 1 and initialCols, inclusive, then an invalid move of 0 is 
   *  returned that will be caught and a new move from the user will be 
   *  prompted. */
  let moveOfString: string => move = s =>
    try(int_of_string(s))
    {
      | _ => -1
    };
  



  /* Input: a state, s
   * Output: a float that represents the value of the board of the state. The
   *  value is positive for Player 1 and negative for Player 2. The value comes 
   *  from a calculation of how advantageous a certain board is to the current 
   *  player (very positive if advantageous for Player 1 to win and very
   *  negative if advantageous for Player 2 to win; 0 if a draw), which comes  
   *  from checking the number of 1's or 2's and open spaces adjacent to the 
   *  position of possible legal moves of the board. */
  let estimateValue: state => float = s => 
  { 
    /* nextOpen: helper function of estimateValue - takes in a state.
     *   Function outputs a list of (int, int) tuples that represent the 
     *   coordinates of the legal move positions of the current board. The
     *   coordinates are int (col, row) form. */
    let nextOpen: state => list((int, int)) = st =>
    {
      let rec openHelper: (list(int), int) => int = (col, row) =>
      switch(col)
      {
        | [0] => row
        | [hd1, hd2, ... tl] => 
        if (hd1 == 0 && hd2 != 0) {row}
        else {openHelper([hd2, ... tl], row + 1)};
        | _ => failwith("Column is full")
      };
      let State(_, b) = st; 
      List.map(x => (x - 1, openHelper(List.nth(b, x - 1), 0)), legalMoves(st));
    }
    /* vertCount: helper function of estimateValue - takes in a board, an 
     *   (int, int) tuple representing a position on the board, and a int 
     *   representation of a player (1, 2) that it is checking pieces of.
     *   Function outputs the number of adjacent pieces that are the input 
     *   player's vertically next to the input position. */
    let vertCount: (board, (int, int), int) => int = 
      (b, (col, row), player) => 
    {
      let rec countDown: (int, int) => int = (c, r) =>
        if (r == initialRows - 1) {0} 
        else 
        {
          if (List.nth(List.nth(b, c), r + 1) == player)
          {1 + countDown(c, r + 1)}
          else 
          {0}
        };
      let v = countDown(col, row);
      if (v + row >= 3){v}
      else {0}
    };
    /* horzCount: helper function of estimateValue - takes in a board, an 
     *   (int, int) tuple representing a position on the board, and a int 
     *   representation of a player (1, 2) that it is checking pieces of.
     *   Function outputs the number of adjacent pieces that are the input 
     *   player's horizontally next to the input position. */
    let horzCount: (board, (int, int), int) => int = (b, (col, row), player) =>
    {
      /*Checks horizontally to the left of current piece 
        Returns how many pieces to the left of current coordinate */
      let rec hCheckL: (int, int) => int = (c, r) =>
      if (c == 0) {0}
      else 
      {
        if (List.nth(List.nth(b, c - 1), r) == player)
        {1 + hCheckL(c - 1, r)}
        else {0}
      };
      /*Checks horizontally to the right of current piece 
        Returns how many pieces to the right of current coordinate */
      let rec hCheckR: (int, int) => int = (c, r) =>
      if (c == initialCols - 1) {0}
      else 
      {
        if (List.nth(List.nth(b, c + 1), r) == player)
        {1 + hCheckR(c + 1, r)}
        else {0}
      };
      
      let left = hCheckL(col, row);
      let right = hCheckR(col, row);

      /* Number of open spaces past last consecutive element to the right 
       *  of the "group of 4" of interest */ 
      let rec openR: (int, int) => int = (cR, rR) => 
      if (cR == initialCols - 1) {0}
      else
      {
        if (cR + 1 <= initialCols - 1)
        {
          if (List.nth(List.nth(b, cR + 1), rR) == 0 ||
              List.nth(List.nth(b, cR + 1), rR) == player)
          {1 + openR(cR + 1, rR)}
          else {0}
        }
        else {0}
      };
      /* Number of open spaces past last consecutive element to the left 
       *  of the "group of 4" of interest */     
      let rec openL: (int, int) => int = (cL, rL) => 
      if (cL == 0) {0}
      else
      {
        if (cL - 1 >= 0)
        {
          if (List.nth(List.nth(b, cL - 1), rL) == 0 || 
              List.nth(List.nth(b, cL - 1), rL) == player)
          {1 + openL(cL - 1, rL)}
          else {0}
        }
        else {0}
      };
      if (left + right >= 3)
      {left + right}
      else
      {
        if (left + right + openR(col + right, row) + 
            openL(col - left, row) >= 3)
        {left + right}
        else
        {0}
      }
    };
    /* diagCountLeft: helper function of estimateValue - takes in a board, an 
     *   (int, int) tuple representing a position on the board, and a int 
     *   representation of a player (1, 2) that it is checking pieces of.
     *   Function outputs the number of adjacent pieces that are the input 
     *   player's diagonally (top right to bottom left) next to the input 
     *   position. */
    let diagCountLeft: (board, (int, int), int) => int = 
      (b, (col, row), player) =>
    {
     /* Returns how many adjacent pieces the player has to the top right of 
      * current coordinate */
      let rec checkUpR: (int, int) => int = (c, r) =>
      if (c == initialCols - 1 || r == 0) {0}
      else 
      {
        if (List.nth(List.nth(b, c + 1), r - 1) == player)
        {1 + checkUpR(c + 1, r - 1)}
        else {0}
      };
     /* Returns how many adjacent pieces the player has to the bottom left of 
      * current coordinate */
      let rec checkDownL: (int, int) => int = (c, r) =>
      if (c == 0 || r == initialRows - 1) {0}
      else 
      {
        if (List.nth(List.nth(b, c - 1), r + 1) == player)
        {1 + checkDownL(c - 1, r + 1)}
        else {0}
      };
      let upRight = checkUpR(col, row);
      let downLeft = checkDownL(col, row);
      /* Number of open spaces past last consecutive element diagonally to the  
       *  top right of the "group of 4" of interest */
      let rec openUpR: (int, int) => int = (cR, rR) => 
      if (cR == initialCols - 1 || rR == 0) {0}
      else
      {
        if (cR + 1 <= initialCols - 1 && rR - 1 >= 0)
        {
          if (List.nth(List.nth(b, cR + 1), rR - 1) == 0 ||
              List.nth(List.nth(b, cR + 1), rR - 1) == player)
          {1 + openUpR(cR + 1, rR - 1)}
          else {0}
        }
        else {0}
      };
      /* Number of open spaces past last consecutive element diagonally to the  
       *  bottom left of the "group of 4" of interest */
      let rec openDownL: (int, int) => int = (cL, rL) => 
      if (cL == 0 || rL == initialRows - 1) {0}
      else
      {
        if (cL - 1 >= 0 && rL + 1 <= initialRows - 1)
        {
          if (List.nth(List.nth(b, cL - 1), rL + 1) == 0 ||
              List.nth(List.nth(b, cL - 1), rL + 1) == player)
          {1 + openDownL(cL - 1, rL + 1)}
          else {0}
        }
        else {0}
      };
      if (upRight + downLeft >= 3)
      {upRight + downLeft}
      else
      {
        if (upRight + downLeft + openUpR(col + upRight, row - upRight) + 
            openDownL(col - downLeft, row + downLeft) >= 3)
        {upRight + downLeft}
        else
        {0}
      }
    };
    /* diagCountRight: helper function of estimateValue - takes in a board, an 
     *   (int, int) tuple representing a position on the board, and a int 
     *   representation of a player (1, 2) that it is checking pieces of.
     *   Function outputs the number of adjacent pieces that are the input 
     *   player's diagonally (top left to bottom right) next to the input 
     *   position. */
    let diagCountRight: (board, (int, int), int) => int = 
      (b, (col, row), player) =>
    {
     /* Returns how many adjacent pieces the player has to the bottom right of 
      * current coordinate */
      let rec checkDownR: (int, int) => int = (c, r) =>
      if (c == initialCols - 1 || r == initialRows - 1) {0}
      else 
      {
        if (List.nth(List.nth(b, c + 1), r + 1) == player)
        {1 + checkDownR(c + 1, r + 1)}
        else {0}
      };
     /* Returns how many adjacent pieces the player has to the bottom left of 
      * current coordinate */
      let rec checkUpL: (int, int) => int = (c, r) =>
      if (c == 0 || r == 0) {0}
      else 
      {
        if (List.nth(List.nth(b, c - 1), r - 1) == player)
        {1 + checkUpL(c - 1, r - 1)}
        else {0}
      };
      let downRight = checkDownR(col, row);
      let upLeft = checkUpL(col, row);
      /* Number of open spaces past last consecutive element diagonally to the  
       *  bottom right of the "group of 4" of interest */
      let rec openDownR: (int, int) => int = (cR, rR) => 
      if (cR == initialCols - 1 || rR == initialRows - 1) {0}
      else
      {
        if (cR + 1 <= initialCols - 1 && rR + 1 <= initialRows - 1)
        {
          if (List.nth(List.nth(b, cR + 1), rR + 1) == 0 ||
              List.nth(List.nth(b, cR + 1), rR + 1) == player)
          {1 + openDownR(cR + 1, rR + 1)}
          else {0}
        }
        else {0}
      };
      /* Number of open spaces past last consecutive element diagonally to the  
       *  top left of the "group of 4" of interest */
      let rec openUpL: (int, int) => int = (cL, rL) => 
      if (cL == 0 || rL == 0) {0}
      else
      {
        if (cL - 1 >= 0 && rL - 1 >= 0)
        {
          if (List.nth(List.nth(b, cL - 1), rL - 1) == 0 ||
              List.nth(List.nth(b, cL - 1), rL - 1) == player)
          {1 + openUpL(cL - 1, rL - 1)}
          else {0}
        }
        else {0}
      };
      if (upLeft + downRight >= 3)
      {upLeft + downRight}
      else
      {
        if (upLeft + downRight + openUpL(col - upLeft, row - upLeft) + 
            openDownR(col + downRight, row + downRight) >= 3)
        {upLeft + downRight}
        else
        {0}
      }
    };
    /* calculateMoves: helper function of estimateValue - takes in a board, a 
     *   list((int, int)) representing positions of next moves of board, and  
     *   a int representation of a player (1, 2) that it is checking pieces of.
     *   Function outputs a float value of the board after checking the 
     *   adjacent pieces to each potential legal move using above helpers. 
     *   Values for adjacency are assigned in the following ranking: 
     *   3 in a row > 2 > 1 > 0 via an exponential system of power 5. */
    let rec calculateMoves: (board, list((int, int)), int) => float = 
      (gameB, moveList, p) =>
    switch(moveList)
    {
      | [] => 0.0
      | [hd, ... tl] => 
      {
        let v = float_of_int(vertCount(gameB, hd, p));
        let h = float_of_int(horzCount(gameB, hd, p));
        let d1 = float_of_int(diagCountLeft(gameB, hd, p));
        let d2 = float_of_int(diagCountRight(gameB, hd, p));
        (5.0**v +. 5.0**h +. 5.0**d1 +. 5.0**d2) +. 
        calculateMoves(gameB, tl, p);
      }
    }
    switch(s)
    {
      | State(Win(P1), _) => 100000.0
      | State(Win(P2), _) => -100000.0
      | State(Draw, _) => 0.0
      | State(Ongoing(P1), b) => 
          calculateMoves(b, nextOpen(State(Ongoing(P1), b)), 1)
      | State(Ongoing(P2), b) => 
          -1.0 *. calculateMoves(b, nextOpen(State(Ongoing(P2), b)), 2)
    }
  }; 

};

module MyGame : Game = Connect4;
open Connect4;

/*
/* Test cases: stringOfPlayer */
checkExpect(stringOfPlayer(P1), "Player 1", "stringOfPlayer: P1");
checkExpect(stringOfPlayer(P2), "Player 2", "stringOfPlayer: P2");


/* Test cases: stringOfState */
checkError(() => stringOfState(State(Ongoing(P1), [[]])), 
  "A matrix cannot be 0-dimensional.");

let b1: board = 
[[0, 0, 0, 0, 0],
 [0, 1, 1, 2, 1],
 [0, 0, 1, 1, 1],
 [0, 0, 1, 1, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

let b2: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 2, 2, 2],
 [0, 0, 1, 1, 1],
 [0, 0, 1, 2, 2],
 [0, 0, 0, 1, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

let b3: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 2, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

let b4: board = 
[[1, 1, 2, 2, 1],
 [2, 1, 2, 2, 1],
 [1, 1, 2, 2, 1],
 [2, 2, 1, 1, 2],
 [1, 2, 1, 1, 2],
 [2, 2, 1, 1, 2],
 [1, 1, 2, 2, 1]];

let b5: board = 
[[1, 1, 2, 2, 1],
 [0, 0, 2, 2, 1],
 [1, 1, 2, 2, 1],
 [2, 2, 1, 1, 2],
 [0, 2, 1, 1, 2],
 [2, 2, 1, 1, 2],
 [0, 1, 2, 2, 1]];
 
checkExpect(stringOfState(State(Ongoing(P1), b3)), 
  "Player 1's current board: \n0  0  0  0  0  0  0\n0  2  0  0  0  0  0\n" ++ 
  "0  1  1  2  0  0  0\n0  2  2  1  0  0  0\n0  1  1  2  2  0  0\n", 
  "stringOfState: Ongoing(P1)");

checkExpect(stringOfState(State(Ongoing(P2), b3)), 
  "Player 2's current board: \n0  0  0  0  0  0  0\n0  2  0  0  0  0  0\n" ++ 
  "0  1  1  2  0  0  0\n0  2  2  1  0  0  0\n0  1  1  2  2  0  0\n", 
  "stringOfState: Ongoing(P2)");

checkExpect(stringOfState(State(Win(P1), b1)), 
  "Player 1 has won!\n0  0  0  0  0  0  0\n0  1  0  0  0  0  0\n" ++ 
  "0  1  1  1  0  0  0\n0  2  1  1  0  0  0\n0  1  1  2  1  0  0\n", 
  "stringOfState: Win(P1)"); 

checkExpect(stringOfState(State(Win(P2), b2)), 
  "Player 2 has won!\n0  0  0  0  0  0  0\n0  2  0  0  0  0  0\n" ++ 
  "0  2  1  1  0  0  0\n0  2  1  2  1  0  0\n0  2  1  2  1  0  0\n", 
  "stringOfState: Win(P2)");

checkExpect(stringOfState(State(Draw, b4)), 
  "Game has ended in Draw\n1  2  1  2  1  2  1\n1  1  1  2  2  2  1\n" ++ 
  "2  2  2  1  1  1  2\n2  2  2  1  1  1  2\n1  1  1  2  2  2  1\n", 
  "stringOfState: Draw");

checkExpect(stringOfState(initialState), 
  "Player 1's current board: \n0  0  0  0  0  0  0\n0  0  0  0  0  0  0\n" ++ 
  "0  0  0  0  0  0  0\n0  0  0  0  0  0  0\n0  0  0  0  0  0  0\n", 
  "stringOfState: initialState");


/* Test cases: stringOfMove */
checkExpect(stringOfMove(1), "Column 1", "stringOfMove: 1");
checkExpect(stringOfMove(7), "Column 7", "stringOfMove: 7");
checkExpect(stringOfMove(10), "Column 10", "stringOfMove: 10");


/* Test cases: makeBoard */
checkExpect(makeBoard(5, 7), 
[[0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0]], 
"makeBoard: 5 x 7");

checkExpect(makeBoard(7, 5), 
[[0, 0, 0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0, 0, 0], 
 [0, 0, 0, 0, 0, 0, 0]], 
"makeBoard: 7 x 5");

checkExpect(makeBoard(1, 1), [[0]], "makeBoard: 1 x 1");

checkExpect(makeBoard(0, 0), [], "makeBoard: no dimensions");


/* Test cases: legalMoves */
checkExpect(legalMoves(State(Ongoing(P2), b3)), 
[1, 2, 3, 4, 5, 6, 7], "legalMoves: P2 all columns open");

checkExpect(legalMoves(initialState), 
[1, 2, 3, 4, 5, 6, 7], "legalMoves: P1 all columns open at initialState");

checkExpect(legalMoves(State(Draw, b4)), 
[], "legalMoves: Draw");

checkExpect(legalMoves(State(Win(P1), b1)), [], "legalMoves: P1 win");

checkExpect(legalMoves(State(Win(P2), b2)), [], "legalMoves: P2 win");

checkExpect(legalMoves(State(Ongoing(P1), b5)), [2, 5, 7], 
"legalMoves: some columns full");


/* Test cases: gameStatus */
checkExpect(gameStatus(initialState), Ongoing(P1), 
  "gameStatus: initialState => Ongoing(P1)");
checkExpect(gameStatus(State(Ongoing(P2), b3)), Ongoing(P2), 
  "gameStatus: ongoing game, P2's turn => Ongoing(P2)");
checkExpect(gameStatus(State(Win(P1), b1)), Win(P1), 
  "gameStatus: Win by P1 => Win(P1)");
checkExpect(gameStatus(State(Win(P2), b2)), Win(P2), 
  "gameStatus: Win by P2 => Win(P2)");
checkExpect(gameStatus(State(Draw, b4)), Draw, 
  "gameStatus: Draw game => Draw");


/* Test cases: nextState */
checkError(() => nextState(State(Ongoing(P1), [[], []]), 1), 
  "tl");
checkError(() => nextState(State(Win(P1), b1), 5), 
  "The game has already ended in a win");
checkError(() => nextState(State(Win(P2), b2), 6), 
  "The game has already ended in a win");
checkError(() => nextState(State(Draw, b4), 2), 
  "The game has already ended in a Draw");
let b6: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 0, 1, 2, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];
 
let b7: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

let b8: board = 
[[0, 0, 0, 0, 0],
 [0, 1, 1, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 2]];

 let b9: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 1, 2, 1, 2],
 [0, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

checkExpect(nextState(State(Ongoing(P1), b6), 3), State(Win(P1), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 2, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 0, 1, 2, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P1 plays winning move - horizontal");

checkExpect(nextState(State(Ongoing(P1), b7), 2), State(Win(P1), 
[[0, 0, 0, 0, 0],
 [0, 1, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P1 plays winning move - vertical");

 checkExpect(nextState(State(Ongoing(P1), b8), 3), State(Win(P1), 
[[0, 0, 0, 0, 0],
 [0, 1, 1, 2, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 2]]), "nextState: P1 plays winning move - diagonal 1");

 checkExpect(nextState(State(Ongoing(P1), b9), 5), State(Win(P1), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 1, 2, 1, 2],
 [1, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P1 plays winning move - diagonal 2");

 checkExpect(nextState(State(Ongoing(P2), b6), 3), State(Ongoing(P1), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 2, 1],
 [0, 0, 2, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 0, 1, 2, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P2 blocks P1 - horizontal");

checkExpect(nextState(State(Ongoing(P2), b7), 2), State(Ongoing(P1), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P2 blocks P1 - vertical");

 checkExpect(nextState(State(Ongoing(P2), b8), 3), State(Ongoing(P1), 
[[0, 0, 0, 0, 0],
 [0, 1, 1, 2, 1],
 [0, 0, 2, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 2]]), "nextState: P2 blocks P1 - diagonal 1");

 checkExpect(nextState(State(Ongoing(P2), b9), 5), State(Ongoing(P1), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 0, 1, 2, 1],
 [0, 1, 2, 1, 2],
 [2, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: P2 blocks P1 - diagonal 2");
 
 checkExpect(nextState(initialState, 4), State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]]), "nextState: changing the initial state");

let b10: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 2, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]];
 
let b11: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 2, 1, 1],
 [0, 0, 2, 2, 1],
 [0, 0, 0, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]];

let b12: board = 
[[0, 0, 0, 0, 2],
 [0, 1, 1, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 1, 2],
 [0, 0, 0, 0, 2]];

 let b13: board = 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 2, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

checkExpect(nextState(State(Ongoing(P2), b10), 2), State(Win(P2), 
[[0, 0, 0, 0, 0],
 [2, 2, 2, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]]), "nextState: P2 plays winning move - vertical");

 checkExpect(nextState(State(Ongoing(P2), b11), 4), State(Win(P2), 
[[0, 0, 0, 0, 0],
 [0, 0, 2, 1, 1],
 [0, 0, 2, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]]), "nextState: P2 plays winning move - horizontal");

 checkExpect(nextState(State(Ongoing(P2), b12), 3), State(Win(P2), 
[[0, 0, 0, 0, 2],
 [0, 1, 1, 2, 1],
 [0, 0, 2, 2, 1],
 [0, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 1, 2],
 [0, 0, 0, 0, 2]]), "nextState: P2 plays winning move - Diagonal 1");

 checkExpect(nextState(State(Ongoing(P2), b13), 6), State(Win(P2), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 2, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0]]), "nextState: P2 plays winning move - Diagonal 2");

 checkExpect(nextState(State(Ongoing(P1), b10), 2), State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [1, 2, 2, 2, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]]), "nextState: P1 blocks P2 - vertical");

 checkExpect(nextState(State(Ongoing(P1), b11), 4), State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [0, 0, 2, 1, 1],
 [0, 0, 2, 2, 1],
 [0, 0, 1, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]]), "nextState: P1 blocks P2 - horizontal");

 checkExpect(nextState(State(Ongoing(P1), b12), 3), State(Ongoing(P2), 
[[0, 0, 0, 0, 2],
 [0, 1, 1, 2, 1],
 [0, 0, 1, 2, 1],
 [0, 2, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 1, 2],
 [0, 0, 0, 0, 2]]), "nextState: P1 blocks P2 - Diagonal 1");

 checkExpect(nextState(State(Ongoing(P1), b13), 6), State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [0, 2, 1, 1, 1],
 [0, 2, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 1, 2, 2, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]]), "nextState: P1 blocks P2 - Diagonal 2");

let b14: board = 
[[1, 1, 2, 2, 1],
 [2, 1, 2, 2, 1],
 [0, 1, 2, 2, 1],
 [2, 2, 1, 1, 2],
 [1, 2, 1, 1, 2],
 [2, 2, 1, 1, 2],
 [1, 1, 2, 2, 1]];

 checkExpect(nextState(State(Ongoing(P1), b14), 3), State(Draw, 
[[1, 1, 2, 2, 1],
 [2, 1, 2, 2, 1],
 [1, 1, 2, 2, 1],
 [2, 2, 1, 1, 2],
 [1, 2, 1, 1, 2],
 [2, 2, 1, 1, 2],
 [1, 1, 2, 2, 1]]), "nextState: P1 plays to draw");

 checkExpect(nextState(State(Ongoing(P2), b14), 3), State(Draw, 
[[1, 1, 2, 2, 1],
 [2, 1, 2, 2, 1],
 [2, 1, 2, 2, 1],
 [2, 2, 1, 1, 2],
 [1, 2, 1, 1, 2],
 [2, 2, 1, 1, 2],
 [1, 1, 2, 2, 1]]), "nextState: P2 plays to draw");


/* Test cases: moveOfString */
checkExpect(moveOfString("henry"), -1, 
  "moveOfString: Invalid move henry => -1");
checkExpect(moveOfString("blahblah"), -1, 
  "moveOfString: Invalid move blahblah => -1");
checkExpect(moveOfString("-500"), -500, 
  "moveOfString: Invalid move -500 => -500");
checkExpect(moveOfString("12345"), 12345, 
  "moveOfString: Invalid move 12345 => 12345");
checkExpect(moveOfString("2"), 2, "moveOfString: Valid move 2 => 2");


/* Test cases: estimateValue */
checkExpect(estimateValue(State(Win(P1), b1)), 100000.0, 
  "estimateValue: P1 Win");
checkExpect(estimateValue(State(Win(P2), b2)), -100000.0, 
  "estimateValue: P2 Win");
checkExpect(estimateValue(State(Draw, b4)), 0.0, "estimateValue: Draw");

checkExpect(estimateValue(initialState), 28.0, "estimateValue: initialState");

checkExpect(estimateValue(State(Ongoing(P1), b3)), 36.0, 
  "estimateValue: early and average board for P1");
checkExpect(estimateValue(State(Ongoing(P2), b3)), -72.0, 
  "estimateValue: early and average board for P2");

checkExpect(estimateValue(State(Ongoing(P1), b14)), 4.0, 
  "estimateValue: P1 final move => Draw");
checkExpect(estimateValue(State(Ongoing(P2), b14)), -4.0, 
  "estimateValue: P2 final move => Draw");

checkExpect(estimateValue(State(Ongoing(P1), b6)), 180.0, 
  "estimateValue: P1 3-in-a-row => about to win horizontal");
checkExpect(estimateValue(State(Ongoing(P1), b7)), 228.0, 
  "estimateValue: P1 3-in-a-row => about to win vertical");
checkExpect(estimateValue(State(Ongoing(P1), b8)), 160.0, 
  "estimateValue: P1 3-in-a-row => about to win Diagonal 1");
checkExpect(estimateValue(State(Ongoing(P1), b9)), 280.0, 
  "estimateValue: P1 3-in-a-row => about to win Diagonal 2");

checkExpect(estimateValue(State(Ongoing(P2), b10)), -172.0, 
  "estimateValue: P2 3-in-a-row => about to win vertical");
checkExpect(estimateValue(State(Ongoing(P2), b11)), -188.0, 
  "estimateValue: P2 3-in-a-row => about to win horizontal");
checkExpect(estimateValue(State(Ongoing(P2), b12)), -292.0, 
  "estimateValue: P2 3-in-a-row => about to win Diagonal 1");
checkExpect(estimateValue(State(Ongoing(P2), b13)), -328.0, 
  "estimateValue: P2 3-in-a-row => about to win Diagonal 2");

checkExpect(estimateValue(State(Ongoing(P2), b6)), -64.0, 
  "estimateValue: P1 3-in-a-row => P2 not advantageous");
checkExpect(estimateValue(State(Ongoing(P2), b7)), -80.0, 
  "estimateValue: P1 3-in-a-row => P2 not advantageous");
checkExpect(estimateValue(State(Ongoing(P2), b8)), -84.0, 
  "estimateValue: P1 3-in-a-row => P2 not advantageous");
checkExpect(estimateValue(State(Ongoing(P2), b9)), -56.0, 
  "estimateValue: P1 3-in-a-row => P2 not advantageous");

checkExpect(estimateValue(State(Ongoing(P1), b10)), 60.0, 
  "estimateValue: P2 3-in-a-row => P1 not advantageous");
checkExpect(estimateValue(State(Ongoing(P1), b11)), 36.0, 
  "estimateValue: P2 3-in-a-row => P1 not advantageous");
checkExpect(estimateValue(State(Ongoing(P1), b12)), 68.0, 
  "estimateValue: P2 3-in-a-row => P1 not advantageous");
checkExpect(estimateValue(State(Ongoing(P1), b13)), 80.0, 
  "estimateValue: P2 3-in-a-row => P1 not advantageous");

  let b15: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 1, 1],
 [0, 2, 1, 2, 1],
 [0, 0, 2, 1, 2],
 [0, 1, 2, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

 let b16: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 2, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0]];

 let b17: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 2, 2],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0]];

 let b18: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 2, 1],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 1, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 2]];

 let b19: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]];

 let b20: board = 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 2]];


checkExpect(estimateValue(State(Ongoing(P1), b15)), 104.0,
  "estimateValue: P1 multiple open 2-in-a-row, P1 2-in-a-row");
checkExpect(estimateValue(State(Ongoing(P1), b18)), 72.0,
  "estimateValue: P1 multiple open 2-in-a-row");
checkExpect(estimateValue(State(Ongoing(P1), b16)), 84.0,
  "estimateValue: P1 2-in-a-row open");
checkExpect(estimateValue(State(Ongoing(P1), b17)), 48.0,
  "estimateValue: P1 2-in-a-row blocked - wall");
checkExpect(estimateValue(State(Ongoing(P1), b19)), 48.0,
  "estimateValue: P1 2-in-a-row blocked - pieces");
checkExpect(estimateValue(State(Ongoing(P1), b20)), 72.0,
  "estimateValue: P1 2-in-a-row open");  


checkExpect(estimateValue(State(Ongoing(P2), b15)), -88.0,
  "estimateValue: P1 2-in-a-row, P1 2-in-a-row");
checkExpect(estimateValue(State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]])), -48.0,
  "estimateValue: P2 blocked 2-in-a-row - pieces");
checkExpect(estimateValue(State(Ongoing(P2), 
[[0, 0, 0, 0, 0],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]])), -44.0,
  "estimateValue: P2 blocked 2-in-a-row - wall");
checkExpect(estimateValue(State(Ongoing(P2), b20)), -40.0,
  "estimateValue: P2 open 2-in-a-row");

  let b21: board = 
  [[0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 1],
   [0, 0, 0, 0, 2],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0]];

  let b22: board = 
  [[0, 0, 0, 0, 1],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 2],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0]];

  checkExpect(estimateValue(State(Ongoing(P1), b21)), 40.0,
    "estimateValue: P1 open wee singleton connected");
  checkExpect(estimateValue(State(Ongoing(P2), b21)), -36.0,
    "estimateValue: P2 open wee singleton connected");
  checkExpect(estimateValue(State(Ongoing(P1), b22)), 36.0,
    "estimateValue: P1 open wee singleton apart");
  checkExpect(estimateValue(State(Ongoing(P2), b22)), -40.0,
    "estimateValue: P2 open wee singleton part");
  checkExpect(estimateValue(State(Ongoing(P1), 
[[0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]])), 36.0,
  "estimateValue: P1 single - wall block");
checkExpect(estimateValue(State(Ongoing(P2), 
[[0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]])), -36.0,
  "estimateValue: P2 single - wall block");
checkExpect(estimateValue(State(Ongoing(P2), 
[[0, 0, 0, 0, 1],
 [0, 0, 0, 0, 2],
 [0, 0, 0, 0, 1],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0]])), -36.0,
  "estimateValue: P2 single - piece block");
*/
