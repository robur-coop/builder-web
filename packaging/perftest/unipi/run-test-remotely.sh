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
scp -r "$PERFSCRIPT_DIR"/* "$SERVER_W_DIR"

#> goto problem: backgrounding a task make it not fail this script :/
info initializing context for unikernel
$SSH "cd $SERVER_DIR; ./init.sh" &

info running unikernel in background
$SSH "cd $SERVER_DIR; ./run-unikernel.sh" &

info sleeping a bit before test
sleep 5

info running test
$SSH "cd $SERVER_DIR; ./run-test.sh"

info killing unikernel
$SSH "cd $SERVER_DIR; kill "'$(cat run-unikernel.sh.PID)' || echo "couldn't kill: unikernel not running"

info killing init-daemon
$SSH "cd $SERVER_DIR; kill "'$(cat init.sh.PID)' || echo "couldn't kill: git daemon not running"

info copying results to "$PERFDATA_DIR"
if [ ! -e "$PERFDATA_DIR" ]; then
    mkdir -p "$PERFDATA_DIR"
fi
scp "${SERVER_W_DIR}/output/*" "$PERFDATA_DIR"/

info running cleanup
$SSH "cd $SERVER_DIR; ./cleanup.sh"

info done


