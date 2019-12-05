open! CS17SetupGame;
open SigGame; 
open Connect4;

module AIPlayer = (MyGame: Game) => 
{
  module PlayerGame = MyGame
 /* Minimax implementation - without alpha-beta pruning - functional*/
/* Input: a state, s
 * Output: the next move for an AI player, generated via the minimax algorithm 
 */
 /*
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => 
  {
    /* Finds the maximum value of the input list of floats */
    let rec findMaxValue: list(float) => float = lst =>
    switch(lst)
    {
      | [] => failwith("[]: Exception to make match cases exhaustive")
      | [x] => x
      | [hd, ... tl] => 
        {
          let max = findMaxValue(tl);
          if (hd > max) {hd}
          else {max}
        }
    };
    /* Finds the minimum value of the input list of floats */
    let rec findMinValue: list(float) => float = lst =>
    switch(lst)
    {
      | [] => failwith("[]: Exception to make match cases exhaustive")
      | [x] => x
      | [hd, ... tl] => 
        {
          let min = findMinValue(tl);
          if (hd < min) {hd}
          else {min}
        }
    };
    /* Builds the game tree and traverses the depths in alternating min and 
     * max per the minimax algorithm. */
    let rec minimax: (PlayerGame.state, int, bool) => float = 
      (current, depth, isMax) =>  
    if (depth == 0 || PlayerGame.legalMoves(current) == []) 
    {PlayerGame.estimateValue(current)}
    else
    {
      let nextList = List.map(x => PlayerGame.nextState(current, x), 
                          PlayerGame.legalMoves(current));
      if(isMax)
      {
        findMaxValue(List.map(x => minimax(x, depth - 1, !isMax), nextList))
      }
      else
      {
        findMinValue(List.map(x => minimax(x, depth - 1, !isMax), nextList))
      }
    };
    /* Takes in a list of floats and returns the index of the float of 
     * maximum value in the list. */
    let maxMoveIndex: list(float) => int = vList =>
    {
      let rec maxHelper: list(float) => (int, float) = lst =>
      switch(lst)
      {
        | [] => failwith("No legal moves available")
        | [x] => (0, x)
        | [hd, ... tl] => 
          {
            let (index, temp) = maxHelper(tl);
            if (hd > temp) {(0, hd)}
            else {(index + 1, temp)}
          }
      }
      let (x, _) = maxHelper(vList);
      x;
    };
    /* Takes in a list of floats and returns the index of the float of 
     * minimum value in the list. */
    let minMoveIndex: list(float) => int = vList =>
    {
      let rec minHelper: list(float) => (int, float) = lst =>
      switch(lst)
      {
        | [] => failwith("No legal moves available")
        | [x] => (0, x)
        | [hd, ... tl] => 
          {
            let (index, temp) = minHelper(tl);
            if (hd < temp) {(0, hd)}
            else {(index + 1, temp)}
          }
      }
      let (x, _) = minHelper(vList);
      x;
    };
    switch(PlayerGame.gameStatus(s))
    {
      | Win(_) => failwith("Game has ended")
      | Draw => failwith("Game has ended in a Draw")
      | Ongoing(P1) => 
        {
          let possibleMoves1 = PlayerGame.legalMoves(s);
          let nextLst1 = List.map(x => PlayerGame.nextState(s, x), 
                          possibleMoves1);
          let valList1 = List.map(x => minimax(x, 5, false), nextLst1);
          List.nth(possibleMoves1, maxMoveIndex(valList1));
        }
      | Ongoing(P2) => 
        {
          let possibleMoves2 = PlayerGame.legalMoves(s);
          let nextLst2 = List.map(x => PlayerGame.nextState(s, x), 
                          possibleMoves2);
          let valList2 = List.map(x => minimax(x, 5, true), nextLst2);
          List.nth(possibleMoves2, minMoveIndex(valList2));
        } 
    }
  };
  */


  /* Minimax implementation - alpha-beta pruning */
  /* Input: a state, s
   * Output: the next move for an AI player, generated via the minimax 
   *   algorithm but with short-circuiting by alpha beta pruning 
   */
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => 
  {
    /* Builds the game tree; function calls maximize if the current node
     * observed is a maximizing node and minimize is the current node is a 
     * minimizing node. Will call maximize or minimize until depth = 0 or
     * the current state is a terminal state - in both cases simply returns
     * the value of the current state. */
    let rec alphaBeta: (PlayerGame.state, int, bool, float, float) => float =  
      (current, depth, isMax, alpha, beta) =>
    {
      if (depth == 0 || PlayerGame.legalMoves(current) == [])
      {PlayerGame.estimateValue(current)}
      else
      {
        let nextList = List.map(x => PlayerGame.nextState(current, x), 
                          PlayerGame.legalMoves(current)); 
        if (isMax)
        {maximize(nextList, depth - 1, !isMax, alpha, beta, -1.0 *. max_float)}
        else
        {minimize(nextList, depth - 1, !isMax, alpha, beta, max_float)}
      }
    }
    /* Takes in a list of states and finds the value of the highest valued 
     * state. Short-circuits searching based on the alpha beta pruning rule of
     * if alpha >= beta, then can stop searching and return best value obtained
     * up to that point. */
    and maximize: (list(PlayerGame.state), int, bool, float, float, float)  
      => float = (childLstA, depthA, isMaxA, alpha, beta, bestA) =>
    switch(childLstA)
    {
      | [] => bestA
      | [hd, ... tl] => 
        {
          let u1 = alphaBeta(hd, depthA, isMaxA, alpha, beta);
          let v1 = max(u1, bestA);
          let a = max(alpha, v1)
          if (a >= beta)
          {v1}
          else
          {maximize(tl, depthA, isMaxA, a, beta, v1)}
        }
    }
    /* Takes in a list of states and finds the value of the lowest valued 
     * state. Short-circuits searching based on the alpha beta pruning rule of
     * if alpha >= beta, then can stop searching and return best value obtained
     * up to that point. */
    and minimize: (list(PlayerGame.state), int, bool, float, float, float) 
      => float = (childLstB, depthB, isMaxB, alpha, beta, bestB) =>
    switch(childLstB)
    {
      | [] => bestB
      | [hd, ... tl] => 
        {
          let u2 = alphaBeta(hd, depthB, isMaxB, alpha, beta);
          let v2 = min(u2, bestB);
          let b = min(beta, v2);
          if (alpha >= b)
          {v2}
          else
          {minimize(tl, depthB, isMaxB, alpha, b, v2)}
        }
    };
    /* Takes in a list of floats and returns the index of the float of 
     * maximum value in the list. */
    let maxMoveIndex: list(float) => int = vList =>
    {
      let rec maxHelper: list(float) => (int, float) = lst =>
      switch(lst)
      {
        | [] => failwith("No legal moves available")
        | [x] => (0, x)
        | [hd, ... tl] => 
          {
            let (index, temp) = maxHelper(tl);
            if (hd > temp) {(0, hd)}
            else {(index + 1, temp)}
          }
      }
      let (x, _) = maxHelper(vList);
      x;
    };
    /* Takes in a list of floats and returns the index of the float of 
     * minimum value in the list. */
    let minMoveIndex: list(float) => int = vList =>
    {
      let rec minHelper: list(float) => (int, float) = lst =>
      switch(lst)
      {
        | [] => failwith("No legal moves available")
        | [x] => (0, x)
        | [hd, ... tl] => 
          {
            let (index, temp) = minHelper(tl);
            if (hd < temp) {(0, hd)}
            else {(index + 1, temp)}
          }
      }
      let (x, _) = minHelper(vList);
      x;
    };
    switch(PlayerGame.gameStatus(s))
    {
      | Win(_) => failwith("Game has ended")
      | Draw => failwith("Game has ended in a Draw")
      | Ongoing(P1) => 
        {
          let possibleMoves1 = PlayerGame.legalMoves(s);
          let nextLst1 = List.map(x => PlayerGame.nextState(s, x), 
                          possibleMoves1);
          let valList1 = List.map(x => alphaBeta(x, 6, false, 
                          -1.0 *. max_float, max_float), nextLst1);
          List.nth(possibleMoves1, maxMoveIndex(valList1)); 
        }
      | Ongoing(P2) => 
        {
          let possibleMoves2 = PlayerGame.legalMoves(s);
          let nextLst2 = List.map(x => PlayerGame.nextState(s, x), 
                          possibleMoves2);
          let valList2 = List.map(x => alphaBeta(x, 6, true, 
                          -1.0 *. max_float, max_float), nextLst2);
          List.nth(possibleMoves2, minMoveIndex(valList2));
        } 
    }
  }
  
};

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

