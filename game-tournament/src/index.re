open Reprocessing;

module Master = Connect4Master.Connect4;

open AC4;
open APlayer;
module G1 = Connect4;
module P1 = AIPlayer(G1);

open BC4;
open BPlayer;
module G2 = Connect4;
module P2 = AIPlayer(G2);

let numRows = 6;
let numCols = 7;

let p1Name = G1.names;
let p2Name = G2.names;

type states = {
	mutable master: Master.state,
	mutable p1: G1.state,
	mutable p2: G2.state,
	mutable player: int
}

type win = {
	mutable winP: string,
	mutable count: int
}

let winP = { winP: "O", count: 0}

let curStates = {
	master: Master.initialState,
	p1: G1.initialState,
	p2: G2.initialState,
	player: 1
};

let rec contains = (list, value) => switch (list) {
	| [] => false
	| [hd, ...tl] => (hd == value) || contains(tl, value)
}

let setup = (env) => {
	Display.drawBG(env);
	Display.drawNames(p1Name, p2Name, env);
}

let draw = (state, env) => {

	if (winP.winP == "O") {
		let state = curStates.master;
		let p1s = curStates.p1;
		let p2s = curStates.p2;
		let player = curStates.player;

		let currentStatus = Master.gameStatus(state);
		winP.winP = switch(currentStatus) {
			| Win(P1) => "1W"
			| Win(P2) => "2W"
			| Draw => "D"
			| _  => "O"
		};

		let game = switch (winP.winP) {
			| "1W" => "Player 1 Wins!"
			| "2W" => "Player 2 Wins!"
			| "D" => "It is a Draw!"
			| "O" => ""
			| _ => failwith("huh?")
		}

		if (game != "") {
			print_endline(game);
			Display.drawScore(game, env);
		} else {
			if (player == 1) {
				let move = G1.stringOfMove(P1.nextMove(p1s));
				if (!contains(Master.legalMoves(state), Master.moveOfString(move))) {
					winP.winP = "2W";
					print_endline("Player 2 Wins!");
					Display.drawScore("Player 2 Wins!", env);
				} else {
					let newstate = Master.nextState(state, Master.moveOfString(move));
					let newp1s = G1.nextState(p1s, G1.moveOfString(move));
					let newp2s = G2.nextState(p2s, G2.moveOfString(move));
					curStates.player = 2;
					curStates.master = newstate;
					curStates.p1 = newp1s;
					curStates.p2 = newp2s;
					Display.drawBoard(numRows, numCols, Master.stringOfState(curStates.master), env);
					Printf.printf("");
				}
			} else {
				let move = G2.stringOfMove(P2.nextMove(p2s));
				if (!contains(Master.legalMoves(state), Master.moveOfString(move))) {
					winP.winP = "1W";
					print_endline("Player 1 Wins!");
					Display.drawScore("Player 1 Wins!", env);
				} else {
					let newstate = Master.nextState(state, Master.moveOfString(move));
					let newp1s = G1.nextState(p1s, G1.moveOfString(move));
					let newp2s = G2.nextState(p2s, G2.moveOfString(move));
					curStates.player = 1;
					curStates.master = newstate;
					curStates.p1 = newp1s;
					curStates.p2 = newp2s;
					Display.drawBoard(numRows, numCols, Master.stringOfState(curStates.master), env);
					Printf.printf("");
				}
			}; 
		};
	} else {
		winP.count = winP.count + 1;
		if (winP.count >= 200) {
			exit(0);
		}
	};
};

run(~setup, ~draw, ());
