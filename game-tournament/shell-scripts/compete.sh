#!/bin/bash 

# PLAN: need to link the 
# two players' files to the source directory, rebuild, then run.
# must check that the results get into the results file (stdout)
#

P1=$1
P2=$2

cd ..
rm src/AC4.re src/APlayer.re src/BC4.re src/BPlayer.re

ln -s ../contestants/$P1/Connect4.re src/AC4.re
ln -s ../contestants/$P1/AIPlayer.re src/APlayer.re
ln -s ../contestants/$P2/Connect4.re src/BC4.re
ln -s ../contestants/$P2/AIPlayer.re src/BPlayer.re

echo ""
npm run build:native
npm run start:native
u1=$?
sleep 3

npm run build:native
npm start:native
u2=$?
if [[ u1 = 0 && u2 = 0  ]]; then
  echo $1 " advances!"
  echo $2 " advances!"
fi

sleep 3

cd shell-scripts
