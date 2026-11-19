#!/bin/ksh
# Thin shim: the filmstrip vectors live at libs/filmstrip/test/vectors.ksh
# (card 2c440c53 acceptance criterion 2). Kept so either path runs.
exec /bin/ksh "$(dirname "$0")/test/vectors.ksh" "$@"
