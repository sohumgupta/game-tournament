#!/bin/bash -x

perl -MList::Util=shuffle -e 'print shuffle(<>);' < $1 > $2
