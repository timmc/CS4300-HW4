#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

WEB_BASE="/course/cs4300/.www"

./mccormack_t_HW5/run.sh simple "$WEB_BASE/HW4/tri/dodecahedron.tri"
./mccormack_t_HW5/run.sh painter "$WEB_BASE/HW5/tri/overlapping.tri"
./mccormack_t_HW5/run.sh zbuffer "$WEB_BASE/HW5/tri/intersecting.tri"
