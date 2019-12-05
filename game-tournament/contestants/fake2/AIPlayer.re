open CS17SetupGame;
open SigGame; 
open Connect4;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame

  let nextMove: PlayerGame.state => PlayerGame.move = s => 
  {
    /* finds minimum of a list of floats. Used in Level 1 */
    let minHelper: list(float) => float = fltLst => 
    {
      let rec minAcc: (list(float), float) => float = (input, locMin) =>
      switch (input) {
      | [] => locMin
      | [hd, ...tl] => if (hd < locMin) minAcc(tl, hd) else minAcc(tl, locMin)
      };
      minAcc(fltLst, List.hd(fltLst))
    };

    /* finds maximum of a list of floats. Used in Level 1 */
    let maxHelper: list(float) => float = fltLst => 
    {
      let rec maxAcc: (list(float), float) => float = (input, locMax) =>
      switch (input) {
      | [] => locMax
      | [hd, ...tl] => if (hd > locMax) maxAcc(tl, hd) else maxAcc(tl, locMax)
      };
      maxAcc(fltLst, List.hd(fltLst))
    };

    /* minimax when it is P1's turn. starts with min, then max, then estVal */
    let rec miniMaxP1: (PlayerGame.state, int) => float = (stat, depth) =>
    switch (depth) {
    | 5 => PlayerGame.estimateValue(stat)
    | 2 | 4 => maxHelper(List.map(x => miniMaxP1(x, depth + 1), List.map(x => PlayerGame.nextState(stat, x), PlayerGame.legalMoves(stat)))) 
    | 1 | 3 => minHelper(List.map(x => miniMaxP1(x, depth + 1), List.map(x => PlayerGame.nextState(stat, x), PlayerGame.legalMoves(stat)))) 
    | _ => failwith("miniMaxP1 outside range of estimation")
    };

    /* minimax when it is P2's turn. starts with max, then min, then estVal */
    let rec miniMaxP2: (PlayerGame.state, int) => float = (stat, depth) =>
    switch (depth) {
    | 5 => PlayerGame.estimateValue(stat)
    | 2 | 4=> minHelper(List.map(x => miniMaxP2(x, depth + 1), List.map(x => PlayerGame.nextState(stat, x), PlayerGame.legalMoves(stat)))) 
    | 1 | 3=> maxHelper(List.map(x => miniMaxP2(x, depth + 1), List.map(x => PlayerGame.nextState(stat, x), PlayerGame.legalMoves(stat)))) 
    | _ => failwith("miniMaxP2 outside range of estimation")
    };

    /* finds the best move for P1 */
    let maxMove: list((float, PlayerGame.move)) => PlayerGame.move = fmPair => 
    {
      let rec extractMaxMove: (list((float, PlayerGame.move)), (float, PlayerGame.move)) => PlayerGame.move = (input, acc) =>
      switch (input, acc) {
      | ([], (_, mvAcc)) => mvAcc
      | ([(num, mv), ...tl], (fltAcc, _)) => if (num > fltAcc) extractMaxMove(tl, (num, mv)) else extractMaxMove(tl, acc)
      };
      extractMaxMove(fmPair, List.hd(fmPair));
    };

    /* finds the best move for P2 */
    let minMove: list((float, PlayerGame.move)) => PlayerGame.move = fmPair => 
    {
      let rec extractMinMove: (list((float, PlayerGame.move)), (float, PlayerGame.move)) => PlayerGame.move = (input, acc) =>
      switch (input, acc) {
      | ([], (_, mvAcc)) => mvAcc
      | ([(num, mv), ...tl], (fltAcc, _)) => if (num < fltAcc) extractMinMove(tl, (num, mv)) else extractMinMove(tl, acc)
      };
      extractMinMove(fmPair, List.hd(fmPair));
    };

    let possibleMoves = PlayerGame.legalMoves(s);

    /* rest of program for nextMove, switch based on whose turn it is */
    switch(PlayerGame.gameStatus(s)) {
    | Ongoing(P1) => 
        maxMove(List.map(x => (miniMaxP1(PlayerGame.nextState(s, x), 1), x), 
                        possibleMoves))
    | Ongoing(P2) => 
        minMove(List.map(x => (miniMaxP2(PlayerGame.nextState(s, x), 1), x), 
                        possibleMoves))
    | Win(_) | Draw => failwith("nextMove, game ended, no moves to make")
    }
  };
}

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

