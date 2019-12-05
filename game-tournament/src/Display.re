open Reprocessing;
open Connect4Master;

let numRows = 6;
let numCols = 7;
let radiusX = 400/numCols;
let radiusY = 400/numRows;

let drawPiece = (player, x, y, env) => { 
    switch (player) {
        | "0" => Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env); 
        | "1" => Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env); 
        | "2" => Draw.fill(Utils.color(~r=0, ~g=0, ~b=255, ~a=255), env); 
        | _ => failwith("bad player value")
    };
    Draw.ellipse(~center=(x, y), ~radx=radiusX, ~rady=radiusY, env);
}

let drawScore = (s, env) => {
    Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=150), env); 
    Draw.rect(~pos=(450,500), ~width=500, ~height=200, env);
    Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
    Draw.text(~body=s, ~pos=(600, 590), env);
    Unix.sleep(2);
}

let drawNames = (p1, p2, env) => {
    drawPiece("1", 80, 80, env);
    drawPiece("2", 1320, 80, env);

    Draw.text(~body=p1, ~pos=(150, 60), env);
    Draw.text(~body=p2, ~pos=(950, 60), env);
}

let drawBG = (env) => {
    Env.size(~width=1400, ~height=1000, env);
    Draw.background(Utils.color(~r=200, ~g=200, ~b=200, ~a=255), env);
}

let drawRow = (r, c, s, env) => {
    let pieces = Str.split(Str.regexp(" "), s);
    List.mapi((i, piece) => drawPiece(piece, int_of_float(float_of_int(i) *. 2.5 *. float_of_int(radiusX) +. 270.), int_of_float(float_of_int(r) *. 2.5 *. float_of_int(radiusY) +. 80.), env), pieces); 
}

let drawBoard = (r, c, s, env) => {
    let rows = Str.split(Str.regexp("\n"), s);
    List.mapi((i, row) => drawRow(i+1, c, row, env), rows); 
}

let drawNothing = (env) => {
    Env.size(~width=1400, ~height=1000, env);
    Draw.background(Utils.color(~r=200, ~g=200, ~b=200, ~a=0), env);
}