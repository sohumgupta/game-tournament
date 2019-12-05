 
open SigGame; 

module Connect4 = {

    /* board dimensions */
    let boardWidth: int = 7;
    let boardHeight: int = 5;

    let names = "master master";

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

    /* a slot of the board that can be empty or filled with either
    player's piece */
    type slot =
      | Piece(whichPlayer)
      | Empty;

    /* a board is the collection of all slots; in this case, the board
    is a list of list of slot lists that each represent columns  */
    type board = list(list(slot));

    /* a state is made up of a position (board) and a status */
    type state = (board, status);

    /* a move is simply the integer representing the column the player
    drops their piece into */
    type move = Column(int);

    /* Printing functions */

    /*
        string_of_player
        input: a player, p
        output: a string representing the player
    */
    let stringOfPlayer = (p: whichPlayer): string => {
        switch (p) {
        | P1 => "Player 1"
        | P2 => "Player 2"
        };
    };

    /*
        string_of_slot
        input: a slot, s
        output: a string representing the slot
    */
    let stringOfSlot = (s: slot): string => {
        switch (s) {
        | Piece P1 => "1 "
        | Piece P2 => "2 "
        | Empty => "0 "
        };
    };

    /*
        my_flatten
        input: a list of strings, l
        output: the string result of concatenating all the strings together
    */
    let rec myFlatten = (l: list(string)) => {
        switch (l) {
        | [] => ""
        | [hd, ... tl] => hd ++ myFlatten(tl)
        };
    };

    /*
        string_of_state_helper
        input: a state made up of a board, status tuple, (b, s)
        output: a string representing what the game board looks like
    */
    let rec stringOfStateHelper = ((b, s): state): string => {
        switch (b) {
        | [[], ... _] => ""
        | [[_hd, ... _], ... _] => (myFlatten(List.map(stringOfSlot, (List.map(List.hd, b))))
            ++ "\n" ++ stringOfStateHelper((List.map(List.tl, b), s)))
        | _ => failwith("Invalid input.")
        };
    };

    /*
        count_up
        input: an integer len
        output: the string result of counting up from 1 to len with a space
                between each number
    */
    let rec countUp = (len: int): string => {
        switch (len) {
        | 0 => failwith("Number of columns cannot be 0.")
        | 1 => "1"
        | _num => countUp(len - 1) ++ " " ++ string_of_int(len)
        };
    };

    /*
      string_of_state
      input: a state, s
      output: the string representing what a game board looks like
    */
    let stringOfState = ((b, s): state): string => {
      //stringOfStateHelper((b, s)) ++ (countUp(List.length(b)));
      stringOfStateHelper((b, s));
    };

    /*
      string_of_move
      input: a move
      output: a string representing the move
    */
    let stringOfMove = (column: move): string => {
      let (Column(num)) = column;
      "column " ++ (string_of_int(num));
    };

    /*
      create_item_list
      input: an item of any type and an integer n
      output: a list with n 'item's in it
    */
    let rec createItemList = (item: 'a, n: int): list('a) => {
        switch(n) {
        | 0 => []
        | x when x > 0 => [item, ... createItemList(item, n - 1)]
        | _ => failwith("Negative number of items.")
        };
    };

    /* set the initial state by initializing the board using preset dimensions */
    let initialState: state =
      (createItemList(createItemList(Empty, boardHeight), boardWidth), Ongoing(P1));

    /*
        legal_moves_helper
        input: a board, b and an integer representing column number, col
        output: the list of columns (potential moves) with at least one open spot
    */
    let rec legalMovesHelper = (b: board, col: int): list(move) => {
        switch(b) {
        | [] => []
        | [[Empty, ... _], ... tail] =>
            [Column(col), ... legalMovesHelper(tail, col + 1)]
        | [[Piece(_p), ... _], ... tail] => legalMovesHelper(tail, col + 1)
        | _ => failwith("legalMovesHelper: invalid input")
        };
    };

    /*
        legal_moves
        input: a state, s
        output: the list of legal moves at s
    */
    let legalMoves = ((b, s): state): list(move) => {
        switch(s) {
        | Ongoing(_p) => legalMovesHelper(b, 1)
        | _ => failwith("Game already over.")
        };
    };

    

    /*
        vert_winp_helper
        input: a slot, p1; a list of slots, l; an integer, num
        output: if there are "four in a row" in l, where p1 is the
                type of piece we're looking for: Some(p1), else None
    */
    let rec vertWinpHelper = (p1: slot, l: list(slot), num: int): option(whichPlayer) => {
        switch(l) {
        | [] => None
        | [Empty, ... []] => None
        | [Empty, a, ... tl] => vertWinpHelper(a, tl, 1)
        | [Piece(p), ... tl] when Piece(p) == p1 =>
            if (num + 1 >= 4) Some(p) else vertWinpHelper(p1, tl, num + 1)
        | [p, ... tl] => vertWinpHelper(p, tl, 1)
        };
    };

    /*
      vert_winp
      input: a board, b
      output: a which_player option determining if there are four in a row vertically
    */
    let rec vertWinp = (b: board): option(whichPlayer) => {
      switch(b) {
      | [] => None
      | [[s, ... tail], ... rest] =>
          switch(vertWinpHelper(s, tail, 0)) {
          | Some(x) => Some(x)
          | None => vertWinp(rest)
          };
      | _ => failwith("vertWinp: invalid input")
      };
    };

    /*
      horz_winp
      input:a board, b
      output: a which_player option determining if there are four in a row horizontally
    */
    let rec horzWinp = (b: board): option(whichPlayer) => {
      switch(b) {
      | [[], ... _] => None
      | [head, ... _tail] =>
          switch(vertWinpHelper(List.hd(head), List.map(List.hd, b), 0)) {
          | Some(x) => Some(x)
          | None => horzWinp(List.map(List.tl, b))
          };
      | _ => failwith("horzWinp: invalid input")
      };
    };

    /*
      get_diag
      input: a board, b
      output: a slot list representing the down-right diagonal beginning
              with the top left slot
    */
    let rec getDiag = (b: board): list(slot) => {
      switch(b) {
      | [[], ... _] => []
      | [[head, ... _], ... []] => [head]
      | [[head, ... []], ... _] => [head]
      | [[head, ... _], ... tail] => [head, ... getDiag(List.map(List.tl, tail))]
      | _ => failwith("get_diag: your board input was too small.")
      };
    };

    /*
      diag_winp_top
      input: a board, b
      output: a which_player option determining if there is four in a row down
              right diagonally, where the first in each diagonal is only the top row in b
    */
    let rec diagWinpTop = (b: board): option(whichPlayer) => {
      switch(b) {
      | [] => None
      | [[a, ... _], ... tail] =>
          switch(vertWinpHelper(a, getDiag(b), 0)) {
          | Some(x) => Some(x)
          | None => diagWinpTop(tail)
          };
      | _ => failwith("diagWinpTop: invalid input")
      };
    };

    /*
      diag_winp_side
      input: a board, b
      output: a which_player option determining if there is four in a row down
              right diagonally, where the first in each diagonal is only the
              first column in b
    */
    let rec diagWinpSide = (b: board): option(whichPlayer) => {
      switch(b) {
      | [[], ... _] => None
      | [[a, ... _], ... _] =>
          switch(vertWinpHelper(a, getDiag(b), 0)) {
          | Some(x) => Some(x)
          | None => diagWinpSide(List.map(List.tl, b))
          };
      | _ => failwith("diagWinpSide: invalid input")
      };
    };

    /*
      is_somep
      input: a which_player option list, alod
      output: a which_player option, the first Some(x) occurring in alod
              if alod has no Some, then None
    */
    let rec isSomep = (alod: list(option(whichPlayer))): option(whichPlayer) => {
      switch(alod) {
      | [] => None
      | [None, ... tail] => isSomep(tail)
      | [Some(x), ... _tail] => Some(x)
      };
    };

    /*
      diag_down
      input: a board, b
      output: a which_player option that determines if there is four in a row
              diagonally down right
    */
    let diagDown = (b: board): option(whichPlayer) => {
      isSomep([diagWinpTop(b), diagWinpSide(b)]);
    };

    /*
      diag_up
      input: a board, b
      output: a which_player option that determines if there is a four in a row
              diagonally up to the right
    */
    let diagUp = (b: board): option(whichPlayer) => {
      diagDown(List.rev(b));
    };

    /*
      winp
      input: a board, b
      output: if there are any four in a row, then Some of which_player, else None
    */
    let winp = (b: board): option(whichPlayer) => {
        isSomep([vertWinp(b), horzWinp(b), diagDown(b), diagUp(b)]);
    };
    /*
        tiep
        input: a board, b
        output: a boolean for whether or not the board is completely full of pieces
    */
    let rec tiep = (b: board): bool => {
        switch(b) {
        | [] => true
        | [[Empty, ... _tail], ... _rest] => false
        | [[Piece(_p), ... tail], ... rest] => tiep([tail, ... rest])
        | [[], ... rest] => tiep(rest)
        };
    };

    /*
        game_status
        input: a state, s
        output: the status of the game at s
    */
    let gameStatus = ((b, s): state): status => {
        switch(winp(b)) {
        | Some(P1) => Win(P1)
        | Some(P2) => Win(P2)
        | _  => if (tiep(b)) Draw else s;
        };
    };

    /*
      dropper
      input: a slot list col and a which_player p
      output: a slot list with the last empty replaced with Piece(p)
    */
    let rec dropper = (col: list(slot), p: whichPlayer): list(slot) => {
      switch(col) {
      | [Empty, ... []] => [Piece(p), ... []]
      | [Empty, Piece(b), ... rest] => [Piece(p), Piece(b), ... rest] /* TODO WHY IS THIS BOTH PIECE OF P */
      | [Empty, ... tail] => [Empty, ... dropper(tail, p)]
      | _ => failwith("dropper: invalid move.")
      };
    };

    /*
      next_state_helper
      input: c, an int representing the current list we're on; the move made Column(m), b, the current board; p, the current player
      output: the board after the piece has been put in the correct position
    */
    let rec nextStateHelper = (c: int, Column(m): move, b: board, p: whichPlayer): board => {
      switch(c, m, b) {
      | (x, y, [head, ... tail]) when x == y => [dropper(head, p), ... tail]
      | (x, y, [head, ... tail]) when x < y => [head, ... nextStateHelper(c+1, Column(m), tail, p)]
      | (_, _, _) => failwith("next_state_helper: invalid move.")
      };
    };

    /*
      next_state
      input: a state, s and a legal move, made
      output: the state after move m has been made
    */
    let nextState = ((b, s): state, m: move): state => {
      switch(s) {
      | Ongoing(P1) => {
          let x = nextStateHelper(1, m, b, P1);
          (x, (gameStatus((x, Ongoing(P2)))));
        }
      | Ongoing(P2) => {
          let x = nextStateHelper(1, m, b, P2);
          (x, (gameStatus((x, Ongoing(P1)))));
        }
      | _ => failwith("Game is already over")
      };
    };

    /*
      move_of_string
      input: player input for move
      output: the internal representation of a move
    */
    let moveOfString = (s: string): move => {
      try (Column(int_of_string(s))) {
      | _ => failwith("Not a valid move.")
      };
    };

    /* SPECIFIC TO AI PLAYERS */

    /* hueristics for what defines a good position: look for
    these patterns in rows/columns/diagonals */
    let patterns: list((list(slot), float)) =
      [([Piece(P1) , Piece(P1) , Piece(P1) , Piece(P1)], infinity) ,
        ([Piece(P2) , Piece(P2) , Piece(P2) , Piece(P2)], neg_infinity) ,
        ([Empty , Piece(P1) , Piece(P1) , Piece(P1) , Empty], 5000.) ,
        ([Empty , Piece(P2) , Piece(P2) , Piece(P2) , Empty], (-5000.)) ,
        ([Empty , Piece(P1) , Piece(P1) , Piece(P1)], 1000.) ,
        ([Empty , Piece(P2) , Piece(P2) , Piece(P2)], (-1000.)) ,
        ([Piece(P1) , Piece(P1) , Piece(P1) , Empty], 1000.) ,
        ([Piece(P2) , Piece(P2) , Piece(P2) , Empty], (-1000.)) ,
        ([Piece(P1) , Piece(P1) , Empty , Piece(P1)], 1000.) ,
        ([Piece(P2) , Piece(P2) , Empty , Piece(P2)], (-1000.)) ,
        ([Piece(P1) , Empty , Piece(P1) , Piece(P1)], 1000.) ,
        ([Piece(P2) , Empty , Piece(P2) , Piece(P2)], (-1000.)) ,
        ([Piece(P1) , Piece(P1) , Empty , Empty], 100.) ,
        ([Piece(P2) , Piece(P2) , Empty , Empty], (-100.)) ,
        ([Empty , Empty , Piece(P1) , Piece(P1)], 100.) ,
        ([Empty , Empty , Piece(P2) , Piece(P2)], (-100.)) ,
        ([Piece(P1) , Empty , Piece(P1) , Empty], 100.) ,
        ([Piece(P2) , Empty , Piece(P2) , Empty], (-100.)) ,
        ([Empty , Piece(P1) , Empty , Piece(P1)], 100.) ,
        ([Empty , Piece(P2) , Empty , Piece(P2)], (-100.)) ,
        ([Empty , Piece(P1) , Piece(P1) , Empty], 100.) ,
        ([Empty , Piece(P2) , Piece(P2) , Empty], (-100.)) ,
        ([Piece(P1) , Empty , Empty , Piece(P1)], 100.) ,
        ([Piece(P2) , Empty , Empty , Piece(P2)], (-100.)) ,
        ([Piece(P1) , Empty , Empty , Empty], 10.) ,
        ([Piece(P2) , Empty , Empty , Empty], (-10.)) ,
        ([Empty , Piece(P1) , Empty , Empty], 10.) ,
        ([Empty , Piece(P2) , Empty , Empty], (-10.)) ,
        ([Empty , Empty , Piece(P1) , Empty], 10.) ,
        ([Empty , Empty , Piece(P2) , Empty], (-10.)) ,
        ([Empty , Empty , Empty , Piece(P1)], 10.) ,
        ([Empty , Empty , Empty , Piece(P2)], (-10.))];

    /*
      eval_list
      input: a slot list, input; two pattern lists (I don't understand why)
      output:
    */
    let rec eval_list = (input: list(slot), pattern_list: list((list(slot), float)), pattern_list_full: list((list(slot), float))): float => {
      switch(input, pattern_list) {
      | ([], _) => 0.0
      | ([_head, ... tail], []) => eval_list(tail, pattern_list_full, pattern_list_full)
      | ([a, b, c, d, ... tail1], [(p, v), ... _tail2]) when [a, b, c, d, ... []] == p =>
          v +. eval_list([b, c, d, ... tail1], pattern_list_full, pattern_list_full)
      | ([_a, _b, _c, _d, ... _tail1], [_m, ... tail2]) => eval_list(input, tail2, pattern_list_full)
      | (_, _) => 0.0
      };
    };

    /*
      row_get
      input: a board, b
      output: a slot list list of all rows in b
    */
    let rec row_get = (b: board): list(list(slot)) => {
      switch(b) {
      | [[], ... _] => []
      | [[_, ... _], ... _tail] => [List.map(List.hd, b), ... row_get(List.map(List.tl, b))]
      | _ => failwith("row_get: invalid input")
      };
    };
    /*
      get_diag_top
      input: a board b
      output: a slot list list of all down-right diagonals in b with first element
      in top row of b
    */
    let rec get_diag_top = (b: board): list(list(slot)) => {
      switch(b) {
        | [] => []
        | [[_a, ..._], ...tail] => [getDiag(b), ...get_diag_top(tail)]
        | _ => failwith("get_diag_top: invalid input.")
      };
    };
    /*
      get_diag_side
      input: a board, b
      output: a slot list list of all down-right diagonals in b with first element
      in left column of b
    */
    let rec get_diag_side = (b: board): list(list(slot)) => {
      switch(b) {
        | [[], ..._] => []
        | [[_a, ..._], ..._] => [getDiag(b), ...(get_diag_side(List.map(List.tl, b)))]
        | _ => failwith("get_diag_side: invalid input.")
      };
    };

    /*
      get_all
      input: a nonempty board b
      output: a list of all the columns, rows, and diagnals in b as a slot list list
    */
    let get_all = (b: board): list(list(slot)) =>
      if (b == []) {
        [];
      } else {
        b
        @ row_get(b)
        @ get_diag_top(List.tl(b))
        @ get_diag_side(b)
        @ get_diag_top(List.tl(List.rev(b)))
        @ get_diag_side(List.rev(b));
      };

    let rec eval_leaf_helper = (rows: list(list(slot))): float => {
      switch(rows) {
        | [] => 0.0
        | [head, ...tail] => eval_list(head, patterns, patterns) +. (eval_leaf_helper(tail))
      };
    };

    /*
      eval_leaf
      input: a state, (b, s)
      output: a float representing the value of the state
    */
    let evalLeaf = ((b, _s): state): float => {
      eval_leaf_helper(get_all(b));
    };

    let testThis = (s : int) => s + 1;

};

module MyGame : Game = Connect4;
open Connect4;
