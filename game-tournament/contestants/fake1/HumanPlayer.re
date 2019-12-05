open SigGame;
open Connect4;

module HumanPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  type getInput = {
    mutable m: PlayerGame.move
  }

  let getInputJSLine : unit => string = {
    [%bs.raw{|
      function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('What move do you want to make? ');
        return ans;
    }|}]
  }

/* Input: a state, s
 * Output: the next move for a human player, determined by taking in human 
 *  input and parsing it to check if it is valid. If it is not, prompts the
 *  user to enter a new move. If the move is valid, then sets the move and the
 *  cycle repeats until the game is over. */
let rec nextMove = s => {
    let myMove : getInput = {m: List.nth(PlayerGame.legalMoves(s), 0)};
    let input : string = getInputJSLine();
    switch (input) {
    | "exit" => failwith("Exiting Game REPL"); 
    | _ => {
      let mov = try(PlayerGame.moveOfString(input)) {
        | _ =>
          print_endline("not a valid move");
          nextMove(s); 
        }
        /* Modified 11/19*/
      if (List.mem(PlayerGame.moveOfString(input), PlayerGame.legalMoves(s))) 
      {
        myMove.m = mov;
      } else {
        print_endline("Illegal move.");
        myMove.m = nextMove(s);
      }
      }
    }
  myMove.m;
  }
};


module TestGame = Connect4;
module TestHumanPlayer = HumanPlayer(TestGame); 
open TestHumanPlayer; 

/* Because next_move involves read input, you don't need to test it here.
 * If you use any helper procedures in TestHumanPlayer that don't take in 
 * or return a state, test them here. */
