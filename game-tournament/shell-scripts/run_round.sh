#!/bin/bash -x

if [ $# -ne 2 ];
  then
    echo "Usage: run_round namelist output-file"
    exit 1
fi

NAMES=$1
OUTPUT=$2
/bin/rm -rf playerlist round outfile $OUTPUT
./shuffle.sh $NAMES  playerlist
./make_round.sh playerlist  > round.sh
grep "compete.sh" round.sh | sed -e  's/^.*compete.sh//'
chmod +x ./round.sh
./round.sh > outfile
grep "advances!" outfile | sed -e 's/ advances!.*//' | sort | uniq > $OUTPUT

