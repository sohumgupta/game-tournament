#!/bin/bash -x

# UNNEEDED? ./swap1
/bin/rm -rf namelist
cd shell-scripts
ls ../contestants > namelist
./run_round.sh namelist n2
read -n1 -r -p 'Press space to start round 2' key
./run_round.sh n2 n3
read -n1 -r -p 'Press space to start round 3' key
./run_round.sh n3 n4
read -n1 -r -p 'Press space to start round 4' key
./run_round.sh n4 n5
read -n1 -r -p 'Press space to start round 5' key
./run_round.sh n5 n6
read -n1 -r -p 'Press space to start round 6' key
./run_round.sh n6 n7
read -n1 -r -p 'Press space to start round 7' key
./run_round.sh n7 n8
read -n1 -r -p 'Press space to start round 8' key
./run_round.sh n8 n9
read -n1 -r -p 'Press space to start round 9' key
./run_round.sh n9 n10

