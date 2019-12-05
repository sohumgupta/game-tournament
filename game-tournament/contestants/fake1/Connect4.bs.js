// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var CS17SetupGame$Game1 = require("./CS17SetupGame.bs.js");

function stringOfPlayer(p) {
  if (p) {
    return "Player 2";
  } else {
    return "Player 1";
  }
}

function stringOfState(s) {
  var transpose = function (mat) {
    if (mat) {
      var match = mat[0];
      if (match) {
        if (match[1]) {
          return List.append(/* :: */[
                      List.map(List.hd, mat),
                      /* [] */0
                    ], transpose(List.map(List.tl, mat)));
        } else {
          return /* :: */[
                  List.map(List.hd, mat),
                  /* [] */0
                ];
        }
      } else {
        return Pervasives.failwith("A matrix cannot be 0-dimensional.");
      }
    } else {
      return Pervasives.failwith("A matrix cannot be 0-dimensional.");
    }
  };
  var printRow = function (l) {
    if (l) {
      var tl = l[1];
      var hd = l[0];
      if (tl) {
        return String(hd) + ("  " + printRow(tl));
      } else {
        return String(hd);
      }
    } else {
      return "";
    }
  };
  var printBoard = function (lst) {
    if (lst) {
      return printRow(lst[0]) + ("\n" + printBoard(lst[1]));
    } else {
      return "";
    }
  };
  var match = s[0];
  if (typeof match === "number") {
    return "Game has ended in Draw\n" + printBoard(transpose(s[1]));
  } else if (match.tag) {
    return (
            match[0] ? "Player 2" : "Player 1"
          ) + ("'s current board: \n" + printBoard(transpose(s[1])));
  } else {
    return (
            match[0] ? "Player 2" : "Player 1"
          ) + (" has won!\n" + printBoard(transpose(s[1])));
  }
}

function stringOfMove(m) {
  return String(m);
}

function makeBoard(row, col) {
  var addRows = function (row) {
    if (row === 0) {
      return /* [] */0;
    } else {
      return /* :: */[
              0,
              addRows(CS17SetupGame$Game1.$neg(row, 1))
            ];
    }
  };
  var addCols = function (c, temp) {
    if (c === 0) {
      return /* [] */0;
    } else {
      return /* :: */[
              temp,
              addCols(CS17SetupGame$Game1.$neg(c, 1), temp)
            ];
    }
  };
  return addCols(col, addRows(row));
}

var initialState_000 = /* Ongoing */Block.__(1, [/* P1 */0]);

var initialState_001 = makeBoard(5, 7);

var initialState = /* State */[
  initialState_000,
  initialState_001
];

function legalMoves(s) {
  var lHelper = function (_lst, _index) {
    while(true) {
      var index = _index;
      var lst = _lst;
      if (lst) {
        var tl = lst[1];
        if (List.hd(lst[0]) === 0) {
          return /* :: */[
                  index,
                  lHelper(tl, CS17SetupGame$Game1.$plus(index, 1))
                ];
        } else {
          _index = CS17SetupGame$Game1.$plus(index, 1);
          _lst = tl;
          continue ;
        }
      } else {
        return /* [] */0;
      }
    };
  };
  var tmp = s[0];
  if (typeof tmp === "number" || !tmp.tag) {
    return /* [] */0;
  } else {
    return lHelper(s[1], 1);
  }
}

function gameStatus(s) {
  return s[0];
}

function nextState(s, m) {
  var changeCol = function (i, r, col) {
    if (col) {
      var hd1 = col[0];
      if (hd1 === 0 && !col[1]) {
        return /* tuple */[
                /* :: */[
                  i,
                  /* [] */0
                ],
                r
              ];
      }
      var match = col[1];
      if (match) {
        var tl = match[1];
        var hd2 = match[0];
        if (hd1 === 0 && hd2 !== 0) {
          return /* tuple */[
                  /* :: */[
                    i,
                    /* :: */[
                      hd2,
                      tl
                    ]
                  ],
                  r
                ];
        } else {
          var match$1 = changeCol(i, CS17SetupGame$Game1.$plus(r, 1), /* :: */[
                hd2,
                tl
              ]);
          return /* tuple */[
                  /* :: */[
                    hd1,
                    match$1[0]
                  ],
                  match$1[1]
                ];
        }
      } else {
        return Pervasives.failwith("Exception to make match cases exhaustive");
      }
    } else {
      return Pervasives.failwith("Exception to make match cases exhaustive");
    }
  };
  var newBoard = function (p, r, b) {
    if (List.length(List.tl(b)) === CS17SetupGame$Game1.$neg(7, m)) {
      var match = changeCol(p, r, List.hd(b));
      return /* tuple */[
              /* :: */[
                match[0],
                List.tl(b)
              ],
              match[1]
            ];
    } else {
      var match$1 = newBoard(p, r, List.tl(b));
      return /* tuple */[
              /* :: */[
                List.hd(b),
                match$1[0]
              ],
              match$1[1]
            ];
    }
  };
  var colCheck = function (_l, _count, p) {
    while(true) {
      var count = _count;
      var l = _l;
      if (l) {
        var tl = l[1];
        if (l[0] !== p) {
          if (List.length(tl) < 4) {
            return false;
          } else {
            _count = 0;
            _l = tl;
            continue ;
          }
        } else if (count === 3) {
          return true;
        } else {
          _count = CS17SetupGame$Game1.$plus(count, 1);
          _l = tl;
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var diagCheckLeft = function (l, p, count, r) {
    var diagLHelper = function (b, _counter, _c1, _r1) {
      while(true) {
        var r1 = _r1;
        var c1 = _c1;
        var counter = _counter;
        if (c1 !== 0) {
          if (r1 === CS17SetupGame$Game1.$neg(5, 1)) {
            if (List.nth(List.nth(b, c1), r1) === p) {
              return counter === 3;
            } else {
              return false;
            }
          } else if (List.nth(List.nth(b, c1), r1) === p) {
            if (counter === 3) {
              return true;
            } else {
              _r1 = CS17SetupGame$Game1.$plus(r1, 1);
              _c1 = CS17SetupGame$Game1.$neg(c1, 1);
              _counter = CS17SetupGame$Game1.$plus(counter, 1);
              continue ;
            }
          } else {
            _r1 = CS17SetupGame$Game1.$plus(r1, 1);
            _c1 = CS17SetupGame$Game1.$neg(c1, 1);
            _counter = 0;
            continue ;
          }
        } else if (List.nth(List.nth(b, 0), r1) === p) {
          return counter === 3;
        } else {
          return false;
        }
      };
    };
    if (CS17SetupGame$Game1.$plus(r, m) >= 4 && CS17SetupGame$Game1.$plus(r, m) <= CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$plus(5, 7), 4)) {
      if (CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$plus(r, m), 1) < 7) {
        return diagLHelper(l, count, CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$plus(r, m), 1), 0);
      } else {
        return diagLHelper(l, count, CS17SetupGame$Game1.$neg(7, 1), CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$plus(r, m), 7));
      }
    } else {
      return false;
    }
  };
  var diagCheckRight = function (l, p, count, r) {
    var diagRHelper = function (b, _counter, _c1, _r1) {
      while(true) {
        var r1 = _r1;
        var c1 = _c1;
        var counter = _counter;
        if (c1 === CS17SetupGame$Game1.$neg(7, 1)) {
          if (List.nth(List.nth(b, c1), r1) === p) {
            return counter === 3;
          } else {
            return false;
          }
        } else if (r1 === CS17SetupGame$Game1.$neg(5, 1)) {
          if (List.nth(List.nth(b, c1), r1) === p) {
            return counter === 3;
          } else {
            return false;
          }
        } else if (List.nth(List.nth(b, c1), r1) === p) {
          if (counter === 3) {
            return true;
          } else {
            _r1 = CS17SetupGame$Game1.$plus(r1, 1);
            _c1 = CS17SetupGame$Game1.$plus(c1, 1);
            _counter = CS17SetupGame$Game1.$plus(counter, 1);
            continue ;
          }
        } else {
          _r1 = CS17SetupGame$Game1.$plus(r1, 1);
          _c1 = CS17SetupGame$Game1.$plus(c1, 1);
          _counter = 0;
          continue ;
        }
      };
    };
    if (r > CS17SetupGame$Game1.$neg(m, 1) && CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$neg(r, m), 1) > CS17SetupGame$Game1.$neg(5, 4) || r < CS17SetupGame$Game1.$neg(m, 1) && CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$neg(m, 1), r) > CS17SetupGame$Game1.$neg(7, 4)) {
      return false;
    } else if (r <= CS17SetupGame$Game1.$neg(m, 1)) {
      return diagRHelper(l, count, CS17SetupGame$Game1.$neg(CS17SetupGame$Game1.$neg(m, 1), r), 0);
    } else {
      return diagRHelper(l, count, 0, CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$neg(r, m), 1));
    }
  };
  var match = s[0];
  if (typeof match === "number") {
    return Pervasives.failwith("The game has already ended in a Draw");
  } else if (match.tag) {
    if (match[0]) {
      var match$1 = newBoard(2, 0, s[1]);
      var row2 = match$1[1];
      var newB2 = match$1[0];
      if (colCheck(List.nth(newB2, CS17SetupGame$Game1.$neg(m, 1)), 0, 2) || colCheck(List.map((function (x) {
                    return List.nth(x, row2);
                  }), newB2), 0, 2) || diagCheckLeft(newB2, 2, 0, row2) || diagCheckRight(newB2, 2, 0, row2)) {
        return /* State */[
                /* Win */Block.__(0, [/* P2 */1]),
                newB2
              ];
      } else if (legalMoves(/* State */[
              /* Ongoing */Block.__(1, [/* P1 */0]),
              newB2
            ]) === /* [] */0) {
        return /* State */[
                /* Draw */0,
                newB2
              ];
      } else {
        return /* State */[
                /* Ongoing */Block.__(1, [/* P1 */0]),
                newB2
              ];
      }
    } else {
      var match$2 = newBoard(1, 0, s[1]);
      var row1 = match$2[1];
      var newB1 = match$2[0];
      if (colCheck(List.nth(newB1, CS17SetupGame$Game1.$neg(m, 1)), 0, 1) || colCheck(List.map((function (x) {
                    return List.nth(x, row1);
                  }), newB1), 0, 1) || diagCheckLeft(newB1, 1, 0, row1) || diagCheckRight(newB1, 1, 0, row1)) {
        return /* State */[
                /* Win */Block.__(0, [/* P1 */0]),
                newB1
              ];
      } else if (legalMoves(/* State */[
              /* Ongoing */Block.__(1, [/* P2 */1]),
              newB1
            ]) === /* [] */0) {
        return /* State */[
                /* Draw */0,
                newB1
              ];
      } else {
        return /* State */[
                /* Ongoing */Block.__(1, [/* P2 */1]),
                newB1
              ];
      }
    }
  } else {
    return Pervasives.failwith("The game has already ended in a win");
  }
}

function moveOfString(s) {
  try {
    return Caml_format.caml_int_of_string(s);
  }
  catch (exn){
    return -1;
  }
}

function estimateValue(s) {
  var nextOpen = function (st) {
    var openHelper = function (_col, _row) {
      while(true) {
        var row = _row;
        var col = _col;
        if (col) {
          var hd1 = col[0];
          if (hd1 === 0 && !col[1]) {
            return row;
          }
          var match = col[1];
          if (match) {
            var hd2 = match[0];
            if (hd1 === 0 && hd2 !== 0) {
              return row;
            } else {
              _row = CS17SetupGame$Game1.$plus(row, 1);
              _col = /* :: */[
                hd2,
                match[1]
              ];
              continue ;
            }
          } else {
            return Pervasives.failwith("Column is full");
          }
        } else {
          return Pervasives.failwith("Column is full");
        }
      };
    };
    var b = st[1];
    return List.map((function (x) {
                  return /* tuple */[
                          CS17SetupGame$Game1.$neg(x, 1),
                          openHelper(List.nth(b, CS17SetupGame$Game1.$neg(x, 1)), 0)
                        ];
                }), legalMoves(st));
  };
  var vertCount = function (b, param, player) {
    var row = param[1];
    var countDown = function (c, r) {
      if (r === CS17SetupGame$Game1.$neg(5, 1) || List.nth(List.nth(b, c), CS17SetupGame$Game1.$plus(r, 1)) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, countDown(c, CS17SetupGame$Game1.$plus(r, 1)));
      }
    };
    var v = countDown(param[0], row);
    if (CS17SetupGame$Game1.$plus(v, row) >= 3) {
      return v;
    } else {
      return 0;
    }
  };
  var horzCount = function (b, param, player) {
    var row = param[1];
    var col = param[0];
    var hCheckL = function (c, r) {
      if (c === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(c, 1)), r) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, hCheckL(CS17SetupGame$Game1.$neg(c, 1), r));
      }
    };
    var hCheckR = function (c, r) {
      if (c === CS17SetupGame$Game1.$neg(7, 1) || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(c, 1)), r) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, hCheckR(CS17SetupGame$Game1.$plus(c, 1), r));
      }
    };
    var left = hCheckL(col, row);
    var right = hCheckR(col, row);
    var openR = function (cR, rR) {
      if (cR === CS17SetupGame$Game1.$neg(7, 1) || !(CS17SetupGame$Game1.$plus(cR, 1) <= CS17SetupGame$Game1.$neg(7, 1) && (List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), rR) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), rR) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openR(CS17SetupGame$Game1.$plus(cR, 1), rR));
      }
    };
    var openL = function (cL, rL) {
      if (cL === 0 || !(CS17SetupGame$Game1.$neg(cL, 1) >= 0 && (List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), rL) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), rL) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openL(CS17SetupGame$Game1.$neg(cL, 1), rL));
      }
    };
    if (CS17SetupGame$Game1.$plus(left, right) >= 3 || CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(left, right), openR(CS17SetupGame$Game1.$plus(col, right), row)), openL(CS17SetupGame$Game1.$neg(col, left), row)) >= 3) {
      return CS17SetupGame$Game1.$plus(left, right);
    } else {
      return 0;
    }
  };
  var diagCountLeft = function (b, param, player) {
    var row = param[1];
    var col = param[0];
    var checkUpR = function (c, r) {
      if (c === CS17SetupGame$Game1.$neg(7, 1) || r === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(c, 1)), CS17SetupGame$Game1.$neg(r, 1)) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, checkUpR(CS17SetupGame$Game1.$plus(c, 1), CS17SetupGame$Game1.$neg(r, 1)));
      }
    };
    var checkDownL = function (c, r) {
      if (c === 0 || r === CS17SetupGame$Game1.$neg(5, 1) || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(c, 1)), CS17SetupGame$Game1.$plus(r, 1)) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, checkDownL(CS17SetupGame$Game1.$neg(c, 1), CS17SetupGame$Game1.$plus(r, 1)));
      }
    };
    var upRight = checkUpR(col, row);
    var downLeft = checkDownL(col, row);
    var openUpR = function (cR, rR) {
      if (cR === CS17SetupGame$Game1.$neg(7, 1) || rR === 0 || !(CS17SetupGame$Game1.$plus(cR, 1) <= CS17SetupGame$Game1.$neg(7, 1) && CS17SetupGame$Game1.$neg(rR, 1) >= 0 && (List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), CS17SetupGame$Game1.$neg(rR, 1)) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), CS17SetupGame$Game1.$neg(rR, 1)) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openUpR(CS17SetupGame$Game1.$plus(cR, 1), CS17SetupGame$Game1.$neg(rR, 1)));
      }
    };
    var openDownL = function (cL, rL) {
      if (cL === 0 || rL === CS17SetupGame$Game1.$neg(5, 1) || !(CS17SetupGame$Game1.$neg(cL, 1) >= 0 && CS17SetupGame$Game1.$plus(rL, 1) <= CS17SetupGame$Game1.$neg(5, 1) && (List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), CS17SetupGame$Game1.$plus(rL, 1)) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), CS17SetupGame$Game1.$plus(rL, 1)) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openDownL(CS17SetupGame$Game1.$neg(cL, 1), CS17SetupGame$Game1.$plus(rL, 1)));
      }
    };
    if (CS17SetupGame$Game1.$plus(upRight, downLeft) >= 3 || CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(upRight, downLeft), openUpR(CS17SetupGame$Game1.$plus(col, upRight), CS17SetupGame$Game1.$neg(row, upRight))), openDownL(CS17SetupGame$Game1.$neg(col, downLeft), CS17SetupGame$Game1.$plus(row, downLeft))) >= 3) {
      return CS17SetupGame$Game1.$plus(upRight, downLeft);
    } else {
      return 0;
    }
  };
  var diagCountRight = function (b, param, player) {
    var row = param[1];
    var col = param[0];
    var checkDownR = function (c, r) {
      if (c === CS17SetupGame$Game1.$neg(7, 1) || r === CS17SetupGame$Game1.$neg(5, 1) || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(c, 1)), CS17SetupGame$Game1.$plus(r, 1)) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, checkDownR(CS17SetupGame$Game1.$plus(c, 1), CS17SetupGame$Game1.$plus(r, 1)));
      }
    };
    var checkUpL = function (c, r) {
      if (c === 0 || r === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(c, 1)), CS17SetupGame$Game1.$neg(r, 1)) !== player) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, checkUpL(CS17SetupGame$Game1.$neg(c, 1), CS17SetupGame$Game1.$neg(r, 1)));
      }
    };
    var downRight = checkDownR(col, row);
    var upLeft = checkUpL(col, row);
    var openDownR = function (cR, rR) {
      if (cR === CS17SetupGame$Game1.$neg(7, 1) || rR === CS17SetupGame$Game1.$neg(5, 1) || !(CS17SetupGame$Game1.$plus(cR, 1) <= CS17SetupGame$Game1.$neg(7, 1) && CS17SetupGame$Game1.$plus(rR, 1) <= CS17SetupGame$Game1.$neg(5, 1) && (List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), CS17SetupGame$Game1.$plus(rR, 1)) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$plus(cR, 1)), CS17SetupGame$Game1.$plus(rR, 1)) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openDownR(CS17SetupGame$Game1.$plus(cR, 1), CS17SetupGame$Game1.$plus(rR, 1)));
      }
    };
    var openUpL = function (cL, rL) {
      if (cL === 0 || rL === 0 || !(CS17SetupGame$Game1.$neg(cL, 1) >= 0 && CS17SetupGame$Game1.$neg(rL, 1) >= 0 && (List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), CS17SetupGame$Game1.$neg(rL, 1)) === 0 || List.nth(List.nth(b, CS17SetupGame$Game1.$neg(cL, 1)), CS17SetupGame$Game1.$neg(rL, 1)) === player))) {
        return 0;
      } else {
        return CS17SetupGame$Game1.$plus(1, openUpL(CS17SetupGame$Game1.$neg(cL, 1), CS17SetupGame$Game1.$neg(rL, 1)));
      }
    };
    if (CS17SetupGame$Game1.$plus(upLeft, downRight) >= 3 || CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(CS17SetupGame$Game1.$plus(upLeft, downRight), openUpL(CS17SetupGame$Game1.$neg(col, upLeft), CS17SetupGame$Game1.$neg(row, upLeft))), openDownR(CS17SetupGame$Game1.$plus(col, downRight), CS17SetupGame$Game1.$plus(row, downRight))) >= 3) {
      return CS17SetupGame$Game1.$plus(upLeft, downRight);
    } else {
      return 0;
    }
  };
  var calculateMoves = function (gameB, moveList, p) {
    if (moveList) {
      var hd = moveList[0];
      var v = vertCount(gameB, hd, p);
      var h = horzCount(gameB, hd, p);
      var d1 = diagCountLeft(gameB, hd, p);
      var d2 = diagCountRight(gameB, hd, p);
      return Math.pow(5.0, v) + Math.pow(5.0, h) + Math.pow(5.0, d1) + Math.pow(5.0, d2) + calculateMoves(gameB, moveList[1], p);
    } else {
      return 0.0;
    }
  };
  var match = s[0];
  if (typeof match === "number") {
    return 0.0;
  } else if (match.tag) {
    if (match[0]) {
      var b = s[1];
      return -1.0 * calculateMoves(b, nextOpen(/* State */[
                      /* Ongoing */Block.__(1, [/* P2 */1]),
                      b
                    ]), 2);
    } else {
      var b$1 = s[1];
      return calculateMoves(b$1, nextOpen(/* State */[
                      /* Ongoing */Block.__(1, [/* P1 */0]),
                      b$1
                    ]), 1);
    }
  } else if (match[0]) {
    return -100000.0;
  } else {
    return 100000.0;
  }
}

var Connect4 = {
  initialRows: 5,
  initialCols: 7,
  names: "aswanso2 mlou2",
  stringOfPlayer: stringOfPlayer,
  stringOfState: stringOfState,
  stringOfMove: stringOfMove,
  makeBoard: makeBoard,
  initialState: initialState,
  legalMoves: legalMoves,
  gameStatus: gameStatus,
  nextState: nextState,
  moveOfString: moveOfString,
  estimateValue: estimateValue
};

var MyGame = {
  stringOfPlayer: stringOfPlayer,
  stringOfState: stringOfState,
  stringOfMove: stringOfMove,
  initialState: initialState,
  legalMoves: legalMoves,
  gameStatus: gameStatus,
  nextState: nextState,
  moveOfString: moveOfString,
  estimateValue: estimateValue
};

exports.Connect4 = Connect4;
exports.MyGame = MyGame;
/* initialState Not a pure module */
