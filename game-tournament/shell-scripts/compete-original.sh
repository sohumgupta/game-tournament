#!/bin/bash 

P1=$1
P2=$2

echo ""
sed -e "s/NAME1/$P1/" -e "s/NAME2/$P2/" referee3.ml > ref.ml
ocaml ref.ml
u1=$?
sleep 3

sed -e "s/NAME2/$P1/" -e "s/NAME1/$P2/" referee3.ml > ref.ml
ocaml ref.ml
u2=$?
if [[ u1 = 0 && u2 = 0  ]]; then
  echo $1 " advances!"
  echo $2 " advances!"
fi

sleep 3

