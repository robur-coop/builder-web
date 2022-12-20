#!/bin/sh

set -e
#set -x

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

usage()
{
    cat <<EOM 1>&2
usage: ${prog_NAME} [ OPTIONS ]
Starts up a performance-test if job is supported and the binary has not been tested before.
In the end a new histogram-plot is generated containing all the results over time.
Options:
    --uuid=STRING
        UUID of build.
    --job=STRING
        The job name of the build.
    --sha256=STRING
        The SHA256 hash of the main binary.
    --data-dir=STRING
        Path to the data directory.
    --cache-dir=STRING
        Path to the cache directory.
EOM
    exit 1
}

UUID=
JOB=
BIN_SHA256=
CACHE_DIR=
DATA_DIR=

while [ $# -gt 0 ]; do
    OPT="$1"

    case "${OPT}" in
        --uuid=*)
            UUID="${OPT##*=}"
            ;;
        --job=*)
            JOB="${OPT##*=}"
            ;;
        --sha256=*)
            BIN_SHA256="${OPT##*=}"
            ;;
        --cache-dir=*)
            CACHE_DIR="${OPT##*=}"
            ;;
        --data-dir=*)
            DATA_DIR="${OPT##*=}"
            ;;
        *)
            warn "Ignoring unknown option: '${OPT}' (Note that this script reads DB)"
            ;;
    esac
    shift
done

[ -z "${UUID}" ] && die "The --uuid option must be specified"
[ -z "${JOB}" ] && die "The --job option must be specified"
[ -z "${BIN_SHA256}" ] && die "The --sha256 option must be specified"
[ -z "${CACHE_DIR}" ] && die "The --cache-dir option must be specified"
[ -z "${DATA_DIR}" ] && die "The --data-dir option must be specified"

BIN_REL="$1"
[ -z "${BIN_REL}" ] && \
    die "The main binarys localpath must be given as the first positional argument"

info "processing UUID '$UUID'"

BIN="${DATA_DIR}/$BIN_REL" 
BIN_EXT=$(echo "$BIN_REL" | sed 's/.*\.\(.*\)/\1/')

DB="${DATA_DIR}/builder.sqlite3"

PERFDATA_DIR="$DATA_DIR/_performance/$JOB/$BIN_SHA256"
PERFSCRIPT_DIR="$DATA_DIR/_performance/$JOB"
#< goto think if this dir makes the most sense

case "${JOB},${BIN_EXT}" in
    unipi,hvt)
        if [ -d "$PERFDATA_DIR" ]; then
            info "$PERFDATA_DIR already exists, exiting"
            exit 0
        fi;
        #> goto pass [ server-ip; ]
        "$PERFSCRIPT_DIR"/run-test.sh "$PERFDATA_DIR"
        #> goto pass [ cache-dir; ]
        "$PERFSCRIPT_DIR"/plot.sh "$PERFDATA_DIR"
        ;;
    *)
        info "Job '${JOB}' compiled to the '${BIN_EXT}'-target doesn't support performance-testing"
        ;;
esac











