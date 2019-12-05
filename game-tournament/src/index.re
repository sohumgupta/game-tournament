open Reprocessing;
open Display;
open Connect4Master;
//open APlayer;
open BPlayer;

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

let p1Name = "sgupta45 sgupta46";
let p2Name = "sgupta47 sgupta48";

type states = {
	mutable master: Master.state,
	mutable p1: G1.state,
	mutable p2: G2.state,
	mutable player: int
}

let curStates = {
	master: Master.initialState,
	p1: G1.initialState,
	p2: G2.initialState,
	player: 1
};

let rec playGame = (state, p1s, p2s, player, env) => {
	Display.drawBoard(numRows, numCols, Master.stringOfState(state), env);
	let currentStatus = Master.gameStatus(state);
	let winP = switch(currentStatus) {
		| Win(P1) => "1W"
        | Win(P2) => "2W"
		| Draw => "D"
        | _  => "O"
	};
	if (winP == "O") {
		if (player == 1) {
			let move = G1.stringOfMove(P1.nextMove(p1s));
			print_endline("move: " ++ move);
			let newstate = Master.nextState(state, Master.moveOfString(move));
			let newp1s = G1.nextState(p1s, G1.moveOfString(move));
			let newp2s = G2.nextState(p2s, G2.moveOfString(move));
			playGame(newstate, p1s, p2s, 2, env);
		} else {
			let move = G2.stringOfMove(P2.nextMove(p2s));
			print_endline("move: " ++ move);
			let newstate = Master.nextState(state, Master.moveOfString(move));
			let newp1s = G1.nextState(p1s, G1.moveOfString(move));
			let newp2s = G2.nextState(p2s, G2.moveOfString(move));
			playGame(newstate, p1s, p2s, 1, env);
		}
	} else {
		switch (winP) {
			| "1W" => "Player 1 Wins!"
			| "2W" => "Player 2 Wins!"
			| "D" => "Draw!"
			| _ => failwith("huh?")
		}
	}
}

let setup = (env) => {
	Display.drawBG(env);
	Display.drawNames(p1Name, p2Name, env);
}

let draw = (_state, env) => {
	
	let state = curStates.master;
	let p1s = curStates.p1;
	let p2s = curStates.p2;
	let player = curStates.player;

	Display.drawBoard(numRows, numCols, Master.stringOfState(state), env);
	let currentStatus = Master.gameStatus(state);
	let winP = switch(currentStatus) {
		| Win(P1) => "1W"
        | Win(P2) => "2W"
		| Draw => "D"
        | _  => "O"
	};
	let game1 = 
	if (winP == "O") {
		if (player == 1) {
			let move = G1.stringOfMove(P1.nextMove(p1s));
			print_endline("move: " ++ move);
			let newstate = Master.nextState(state, Master.moveOfString(move));
			let newp1s = G1.nextState(p1s, G1.moveOfString(move));
			let newp2s = G2.nextState(p2s, G2.moveOfString(move));
			curStates.player = 2;
			curStates.master = newstate;
			curStates.p1 = newp1s;
			curStates.p2 = newp2s;
		} else {
			let move = G2.stringOfMove(P2.nextMove(p2s));
			print_endline("move: " ++ move);
			let newstate = Master.nextState(state, Master.moveOfString(move));
			let newp1s = G1.nextState(p1s, G1.moveOfString(move));
			let newp2s = G2.nextState(p2s, G2.moveOfString(move));
			curStates.player = 1;
			curStates.master = newstate;
			curStates.p1 = newp1s;
			curStates.p2 = newp2s;
		}; 
		"";
	} else {
		switch (winP) {
			| "1W" => "Player 1 Wins!"
			| "2W" => "Player 2 Wins!"
			| "D" => "Draw!"
			| _ => failwith("huh?")
		}
	};
	if (game1 != "") {
		Display.drawScore(game1, env);
	}
};

run(~setup, ~draw, ());
