open! CS17SetupGame;
open SigGame; 
open Connect4;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame

  /* nextMove: state => move
   * input: s, the current game state
   * output: the move that should be taken next based on the game state */
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {

    /* minimax: (state, int) => float
     * input: a (state, depth) tuple where state is the current game state and
     *        depth is an integer representing the number of moves ahead that we
     *        want to look
     * output: a float that represents the best possible value of a state that 
     *         can be produced within the specified depth
    
    recursion diagram:
    OI: (State(Ongoing(P1), [[0, 1, 1, 1], [0, 2, 2, 2]]), 4)
      RI: (State(Win(P1), [[0, 1, 1, 1], [0, 2, 2, 2]]), 3)
          (State(Ongoing(P2), [[0, 1, 1, 1], [1, 2, 2, 2]]), 3)
      RO: infinity
          0.0
    checks that the initial player is P1 and chooses the largest recursive
    output value
    OO: infinity */
    let rec minimax: (PlayerGame.state, int) => float = (state, depth) => 
    switch(PlayerGame.gameStatus(state), (depth == 0)) {
        | (_, true) => PlayerGame.estimateValue(state)
        | (Win(P1), _) => infinity
        | (Win(P2), _) => neg_infinity
        | (Draw, _) => 0.0
        | (Ongoing(P1), _) => {
            let rec maxValue: (float, list(float)) => float = (m, values) => 
            switch(values) {
                | [] => m
                | [hd, ... tl] => if (m > hd) {maxValue(m, tl)} else {
                  maxValue(hd, tl)}
            }; 
            maxValue(neg_infinity, List.map(i =>
              minimax(PlayerGame.nextState(state, i), depth - 1), 
              PlayerGame.legalMoves(state)))}
        | (Ongoing(P2), _) => {
            let rec minValue: (float, list(float)) => float = (m, values) => 
            switch(values) {
                | [] => m
                | [hd, ... tl] => if (m < hd) {minValue(m, tl)} else {
                  minValue(hd, tl)}
            };
            minValue(infinity, List.map(i =>
              minimax(PlayerGame.nextState(state, i), depth - 1), 
              PlayerGame.legalMoves(state)))} 
    };

    /* maxMove: list((float, move)) => move
     * input: moves, a list of (float, move) tuples where each tuple consists of
     *        a move that can be made and the estimated value of the state that 
     *        will result from that move
     * output: the move that results in the highest estimated value
    
    recursion diagram for maxMoveHelper:
    OI:((neg_infinity, Move(1)), [(neg_infinity, Move(1)), (infinity, Move(2))])
      RI: ((neg_infinity, Move(1)), [(infinity, Move(2))])
      RO: Move(2)
    OO: Move(2) */
    let maxMove: list((float, PlayerGame.move)) => PlayerGame.move = moves => {
      let rec maxMoveHelper: ((float, PlayerGame.move),
      list((float, PlayerGame.move))) => PlayerGame.move = (max, pairs) => 
        switch(pairs, max) {
          | ([], (value, move)) => {string_of_float(value);move;}
          | ([(hd, move), ... tl], (maxVal, _)) when hd > maxVal =>
          maxMoveHelper((hd, move), tl)
          | ([_, ... tl], (_, _)) => maxMoveHelper(max, tl)};
      maxMoveHelper(List.hd(moves), List.tl(moves))};
    
    /* minMove: list((float, move)) => move
     * input: moves, a list of (float, move) tuples where each tuple consists of
     *        a move that can be made and the estimated value of the state that 
     *        will result from that move
     * output: the move that results in the lowest estimated value
    
    recursion diagram for minMoveHelper:
    OI:((neg_infinity, Move(1)), [(neg_infinity, Move(1)), (infinity, Move(2))])
      RI: ((neg_infinity, Move(1)), [(infinity, Move(2))])
      RO: Move(1)
    OO: Move(1)  */
    let minMove: list((float, PlayerGame.move)) => PlayerGame.move = moves => {
      let rec minMoveHelper: ((float, PlayerGame.move),
      list((float, PlayerGame.move))) => PlayerGame.move = (min, pairs) => 
        switch(pairs, min) {
          | ([], (value, move)) => {string_of_float(value);move;}
          | ([(hd, move), ... tl], (minVal, _)) when hd < minVal =>
          minMoveHelper((hd, move), tl)
          | ([_, ... tl], (_, _)) => minMoveHelper(min, tl)};
      minMoveHelper(List.hd(moves), List.tl(moves))};
    
    /* nextStatePaths is a list of (state, move) tuples where each tuple 
       consists of a legal move and the state that that move would result in */
    let nextStatePaths =
      List.map(i => (PlayerGame.nextState(s, i), i), PlayerGame.legalMoves(s));
      
    /* nextValuePaths: (list((state, move)), list((float, move))) => 
       list((float, move)) 
     * input: moves, a list of (state, move) tuples and acc, a list of 
     *        (float, move) tuples that acts as an accumulator keeping track of
     *        the value of moves that don't result in a win
     * output: if one of the elements of moves results in a win, then the output
     *         list will contain the tuple corresponding to the value of the win
     *         and its corresponding move
     *         if all the elements of moves don't result in a win, then the
     *         output list contains the tuples of a possible next move and its
     *         corresponding estimated value
    
    recursion diagram:
    OI: ([(State(Ongoing(P1), [[2, 1, 1, 1], [0, 2, 2, 2]]), Move(1)),
          (State(Win(P2), [[0, 1, 1, 1], [2, 2, 2, 2]]), Move(2))], [])
      RI: ([(State(Win(P2), [[0, 1, 1, 1], [2, 2, 2, 2]]), Move(2))], 
           [(0.0, Move(1))])
      RO: [(infinity, Move(2))]
    OO: [(infinity, Move(2))]
    */
    let rec nextValuePaths: (list((PlayerGame.state, PlayerGame.move)),
    list((float, PlayerGame.move))) => list((float, PlayerGame.move)) =
    (moves, acc) => switch(moves) {
      | [] => acc
      | [(s, m), ...tl] => if (PlayerGame.gameStatus(s) == Win(P1)) {
        [(infinity, m)] } else if (PlayerGame.gameStatus(s) == Win(P2)) {
        [(neg_infinity, m)] } else {
        nextValuePaths(tl, [(minimax(s, 4), m), ... acc])}};

    switch(PlayerGame.gameStatus(s)) {
        | Win(_) => failwith("game is over, win")
        | Draw => failwith("game is over, draw")
        | Ongoing(P1) => maxMove(nextValuePaths(nextStatePaths, []))
        | Ongoing(P2) => minMove(nextValuePaths(nextStatePaths, []))}
    }; 
}

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */
