#! /bin/bash

set -e

prog_NAME=$(basename "${0}")

warn()
{
    echo "${prog_NAME}: WARN: $*"
}

info()
{
    echo "${prog_NAME}: INFO: $*"
}

err()
{
    echo "${prog_NAME}: ERROR: $*" 1>&2
}

die()
{
    echo "${prog_NAME}: ERROR: $*" 1>&2
    exit 1
}

PERFSCRIPT_DIR="$1"
PERFDATA_DIR="$2"
BIN="$3"
SERVER="$4"
SERVER_DIR="$5"

SERVER_W_DIR="$SERVER:$SERVER_DIR"
SSH="ssh $SERVER"

scp "$BIN" "$SERVER_W_DIR"
scp "$PERFSCRIPT_DIR"/init.sh "$SERVER_W_DIR"
scp "$PERFSCRIPT_DIR"/run-unikernel.sh "$SERVER_W_DIR"
scp "$PERFSCRIPT_DIR"/run-test.sh "$SERVER_W_DIR"
scp "$PERFSCRIPT_DIR"/cleanup.sh "$SERVER_W_DIR"

info initializing context for unikernel
"$SSH" "$PERFSCRIPT_DIR"/init.sh

info running unikernel in background
"$SSH" "$PERFSCRIPT_DIR"/run-unikernel.sh &
UNIKERNEL_PID=$!

info sleeping a bit before test
sleep 5

info running test
"$SSH" "$PERFSCRIPT_DIR"/run-test.sh

info killing unikernel
kill "$UNIKERNEL_PID"

info copying results to "$PERFDATA_DIR"
scp "${SERVER_W_DIR}/results/*" "$PERFDATA_DIR"

info running cleanup
"$SSH" "$PERFSCRIPT_DIR"/cleanup.sh

info done


