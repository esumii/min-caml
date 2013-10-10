#!/bin/sh
exec sed -e 's/^(\*NOMINCAML \(.*\)\*)$/\1/' -e 's/^(\*MINCAML\*)\(.*\)$//' "$@"
