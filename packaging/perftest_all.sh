#!/bin/sh

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

usage()
{
    cat <<EOM 1>&2
usage: ${prog_NAME} [ OPTIONS ] 
Starts up a performance-test if job is supported and the binary has not been tested before.
In the end a new histogram-plot is generated containing all the results over time.
Options:
    --job=STRING
        The job name of the build.
    --data-dir=STRING
        Path to the data directory.
    --cache-dir=STRING
        Path to the cache directory.
    --conf-dir=STRING
        Path to the configuration directory.
EOM
    exit 1
}

JOB=
DATA_DIR=
CACHE_DIR=
CONF_DIR=

while [ $# -gt 1 ]; do
    OPT="$1"

    case "${OPT}" in
        --job=*)
            JOB="${OPT##*=}"
            ;;
        --data-dir=*)
            DATA_DIR="${OPT##*=}"
            ;;
        --cache-dir=*)
            CACHE_DIR="${OPT##*=}"
            ;;
        --conf-dir=*)
            CONF_DIR="${OPT##*=}"
            ;;
        *)
            warn "Ignoring unknown option: '${OPT}' (Note that this script reads DB)"
            ;;
    esac
    shift
done

[ -z "${JOB}" ] && die "The --job option must be specified"
[ -z "${CACHE_DIR}" ] && die "The --cache-dir option must be specified"
[ -z "${DATA_DIR}" ] && die "The --data-dir option must be specified"
[ -z "${CONF_DIR}" ] && die "The --conf-dir option must be specified"

DB="${DATA_DIR}/builder.sqlite3"
[ ! -e "$DB" ] && die "The database doesn't exist: '$DB'"

get_jobs_build-uuids () {
    sqlite3 "$DB" "select b.uuid from build as b \
        join job as j on j.id = b.job\
        where j.name = '$JOB' \
        order by b.id desc"
}

get_bin_hash () {
    UUID="$1"
    sqlite3 "$DB" "SELECT lower(hex(ba.sha256)) FROM build AS b
        JOIN build_artifact AS ba ON ba.id = b.main_binary
        WHERE uuid = '$UUID'"
}

get_bin_localpath () {
    UUID="$1"
    sqlite3 "$DB" "SELECT ba.localpath FROM build AS b
        JOIN build_artifact AS ba ON ba.id = b.main_binary
        WHERE uuid = '$UUID'"
}

while read -r UUID; do
    BIN="$DATA_DIR"/$(get_bin_localpath "$UUID")
    BIN_SHA256=$(get_bin_hash "$UUID")

    "$CONF_DIR"/upload_hooks/perftest.sh \
               --uuid="$UUID" \
               --job="$JOB" \
               --sha256="$BIN_SHA256" \
               --data-dir="$DATA_DIR" \
               --cache-dir="$CACHE_DIR" \
               "$BIN"
done < <(get_jobs_build-uuids)


