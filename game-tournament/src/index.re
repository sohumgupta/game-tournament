open Reprocessing;

let numRows = 6;
let numCols = 7;
let radiusX = 400/numCols;
let radiusY = 400/numRows;

let allPlayers = ["a", "b", "c", "d", "e", "f", "g", "h"];

let rec initColumn = (rows) => switch (rows) {
	| 0 => []
	| r => ["empty", ...initColumn(r - 1)]
}

let rec initBoard = (cols) => switch (cols) {
	| 0 => []
	| c => [initColumn(numRows), ...initBoard(c - 1)]
}

let rec drawColumn = (column, startX, startY, env) => switch (column) {
	| [] => Printf.printf("");
	| [hd, ...tl] => {
		Draw.ellipse(~center=(startX, startY + radiusX), ~radx=radiusX, ~rady=radiusY, env); 
		drawColumn(tl, startX, startY + radiusY*2 + 20, env)
	}
}

let rec drawBoard = (board, startX, env) => switch (board) {
	| [] => Printf.printf("");
	| [hd, ...tl] => {
		drawColumn(hd, startX, radiusY/2 + 20, env);
		drawBoard(tl, startX + radiusX * 2 + 20, env)
	}
}

let shuffle = d => {
	let nd = List.map(c => (Random.bits(), c), d);
	let sond = List.sort(compare, nd);
	List.map(snd, sond);
};

let playGame = (p1, p2) => { if (p1 > p2) {p1} else {p2} }

let rec playRound = (players) => switch(players) {
	| [] => players
	| [p] => players 
	| [p1, p2, ...tl] => {
		if (playGame(p1, p2) == playGame(p2, p1)) {
			print_endline(playGame(p1, p2) ++ " won when " ++ p1 ++ " played " ++ p2);
			[playGame(p1, p2), ...playRound(tl)]
		} else {
			print_endline("p1 and p2 both won when " ++ p1 ++ " played " ++ p2);
			[playGame(p1, p2), playGame(p2, p1), ...playRound(tl)]
		}
	}
}

let rec playTournament = (players) => {
	let newplayers = shuffle(players);
	switch (newplayers) {
	| [] => print_endline("wait what...")
	| [p] => print_endline(p ++ " is the winner!")
	| [p1, p2] => print_endline(p1 ++ " and " ++ p2 ++ " are both winners!")
	| alop => print_endline("playing tournament!"); playTournament(playRound(alop))
	}
}

let setup = (env) => {
	Env.size(~width=1400, ~height=1000, env);
	Draw.background(Utils.color(~r=200, ~g=200, ~b=200, ~a=255), env);
	
	let board = initBoard(numCols);
	
	Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
	drawBoard(board, 310, env);

	playTournament(allPlayers);
}

let draw = (_state, env) => {
	Printf.printf("");
};

run(~setup, ~draw, ());
