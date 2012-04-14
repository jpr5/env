#!/usr/bin/env bash

EMACSSERVER="/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs"
EMACSCLIENT="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient"

# Premise: Almost always want to invoke emacsclient.  Thus we catch the special 
# cases of running emacs itself, otherwise pass through to emacsclient.

# if no args, invoke background emacs
# if (-nw, -batch, --batch) in args, invoke emacs
# else invoke emacsclient

if test $# -eq 0; then
    # TODO: detect that the server socket already exists and isn't stale
    $EMACSSERVER -D --iconic &
    exit
fi

for opt in "-nw" "-batch" "--batch"; do 
    for arg in $*; do 
        if test "$opt" == "$arg"; then 
            $EMACSSERVER $*
            exit
        fi
    done
done

$EMACSCLIENT $*
