open SigGame;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame

  let rec max_list (flt_lst: list(float)): float =
    switch(flt_lst) {
      | [] => failwith("No possible move")
      | [a] => a
      | [flt1, flt2, ...tl] => if (flt1 > flt2) {
        max_list([flt1, ...tl])
      } else {
        max_list([flt2, ...tl])
      };
    };

  let rec min_list (flt_lst: list(float)): float =
    switch(flt_lst) {
      | [] => failwith("No possible move")
      | [a] => a
      | [flt1, flt2, ...tl] => if (flt1 < flt2) {
        min_list ([flt1, ...tl])
      } else {
        min_list([flt2, ...tl])
      };
    };

  let rec move_finder(mv_lst: list(PlayerGame.move), count: int, flt_lst: list(float), flt: float): PlayerGame.move =
    switch(flt_lst) {
      | [] => failwith("No possible move")
      | [hd, ...tl] => if (hd == flt) {
        List.nth(mv_lst, count)
      } else {
        move_finder(mv_lst, (count + 1), tl, flt)
      };
    };

  let rec minimax (a_state: PlayerGame.state, n: int) : float =
    switch(n) {
      | 0 => PlayerGame.evalLeaf(a_state)
      | _c when (PlayerGame.gameStatus(a_state)) == PlayerGame.Ongoing(PlayerGame.P2) =>
          min_list(List.map(((f) => minimax(f, n-1)),
                      List.map(PlayerGame.nextState(a_state), PlayerGame.legalMoves(a_state))))
      | _c when (PlayerGame.gameStatus(a_state)) == PlayerGame.Ongoing(PlayerGame.P1) =>
          max_list(List.map(((f) => minimax(f, n-1)),
                      List.map(PlayerGame.nextState(a_state), PlayerGame.legalMoves(a_state))))
      | _c when (PlayerGame.gameStatus(a_state)) == PlayerGame.Win(PlayerGame.P1) => infinity
      | _c when (PlayerGame.gameStatus(a_state)) == PlayerGame.Win(PlayerGame.P2) => neg_infinity
      | _c => 0.
     }

  let nextMove: PlayerGame.state => PlayerGame.move = s => {
    let moves = PlayerGame.legalMoves(s);
    let flt_lst = List.map(((f) => minimax(f, 4)), (List.map(PlayerGame.nextState(s), moves)));
    switch(PlayerGame.gameStatus(s)) {
      | PlayerGame.Ongoing(PlayerGame.P1) => move_finder(moves, 0, flt_lst, max_list(flt_lst))
      | PlayerGame.Ongoing(PlayerGame.P2) => move_finder(moves, 0, flt_lst, min_list(flt_lst))
      | _ => failwith("Game Over")
    };
    }
};
