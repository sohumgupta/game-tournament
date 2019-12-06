#!/bin/bash -x

#Expects a single argument

PLAYERS=$1
awk ' BEGIN {print "#!/bin/bash "} NR % 2 == 1 { o=$0 ; next } { print "./compete.sh "  o " " $0 ; print "#read -n1 -r -p \"Press space to continue...\" key"} END { if ( NR % 2 == 1 ) { print "echo \"" o " advances! --Bye in this round--\""  } }' $PLAYERS

