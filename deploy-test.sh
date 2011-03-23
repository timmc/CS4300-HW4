#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

TRI_BASE="/course/cs4300/.www/HW5/tri"

./mccormack_t_HW5/run.sh bary2 "$TRI_BASE/pinwheel.tri"
./mccormack_t_HW5/run.sh wire "$TRI_BASE/dodecahedron.tri"
./mccormack_t_HW5/run.sh shade "$TRI_BASE/bunny-69451.tri"
