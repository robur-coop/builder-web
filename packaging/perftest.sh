#!/bin/sh

set -e
#set -x

#> Note: this script lies within <conf-dir>/upload_hooks
CONF_DIR="$(dirname "${0}")"/..

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

while [ $# -gt 1 ]; do
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

# >>> CONFIGURE PER SYSTEM
SERVER="starand"
SERVER_DIR="0tmp/robur_perftest"
# <<< --------------------

info "processing UUID '$UUID'"

BIN="${DATA_DIR}/$BIN_REL" 
BIN_EXT=$(echo "$BIN_REL" | sed 's/.*\.\(.*\)/\1/')

DB="${DATA_DIR}/builder.sqlite3"

DB="${DATA_DIR}/builder.sqlite3"
[ ! -e "$DB" ] && die "The database doesn't exist: '$DB'"

DB_VERSION="$(sqlite3 "$DB" "PRAGMA user_version;")"
[ -z "$DB_VERSION" ] && die "Couldn't read database version from '$DB'"
[ "$DB_VERSION" -lt 16 ] && die "The database version should be >= 16. It is '$DB_VERSION'"

APP_ID="$(sqlite3 "$DB" "PRAGMA application_id;")"
[ -z "$APP_ID" ] && die "Couldn't read application-id from '$DB'"
[ "$APP_ID" -ne 1234839235 ] && die "The application-id should be = 1234839235. It is '$APP_ID'"

PERFJOB_DIR="$DATA_DIR/_perftest/$JOB"
PERFSCRIPT_DIR="$CONF_DIR/perftest/$JOB"
PERFDATA_DIR="$PERFJOB_DIR/$BIN_SHA256"

if [ -d "$PERFDATA_DIR" ]; then
    info "$PERFDATA_DIR already exists, exiting"
    exit 0
fi;
#< goto maybe add a 'force' param to rerun test + regenerate plot

case "${JOB},${BIN_EXT}" in
    unipi,hvt)
        "$PERFSCRIPT_DIR"/run-test-remotely.sh "$PERFSCRIPT_DIR"/remote "$PERFDATA_DIR" "$BIN" "$SERVER" "$SERVER_DIR"
        "$PERFSCRIPT_DIR"/plot.sh "$PERFJOB_DIR" "$CACHE_DIR" "$DB" "$JOB" "$UUID"
        ;;
    *)
        info "Job '${JOB}' compiled to the '${BIN_EXT}'-target doesn't support performance-testing"
        ;;
esac











