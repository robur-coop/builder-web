#!/bin/sh

set -ex

prog_NAME=$(basename "${0}")

warn()
{
    echo "${prog_NAME}: WARN: $*"
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
usage: ${prog_NAME} [ OPTIONS ] FILE
Generates visualizations
Options:
    --debug-binary=STRING
        Path to debug binary.
    --opam-switch=STRING
        Path to opam switch.
    --uuid=STRING
        UUID of build.
    --cache-dir=STRING
        Path to the cache directory.
EOM
    exit 1
}

DEBUG=
OPAM=
UUID=
CACHE=

while [ $# -gt 1 ]; do
    OPT="$1"

    case "${OPT}" in
        --debug-binary=*)
            DEBUG="${OPT##*=}"
            ;;
        --opam-switch=*)
            OPAM="${OPT##*=}"
            ;;
        --uuid=*)
            UUID="${OPT##*=}"
            ;;
        --cache-dir=*)
            CACHE="${OPT##*=}"
            ;;
        --*)
            warn "Ignoring unknown option: '${OPT}'"
            ;;
        *)
            err "Unknown option: '${OPT}'"
            usage
            ;;
    esac
    shift
done

[ -z "${UUID}" ] && die "The --uuid option must be specified"
[ -z "${CACHE}" ] && die "The --cache-dir option must be specified"
[ -z "${OPAM}" ] && die "The --opam-switch option must be specified"

FILENAME="${1}"
CACHE_DIR="${CACHE}/${UUID}"
BUILDER_VIZ="builder-viz"

mktemp_aux () {
    if [ "$(uname)" = "Linux" ]; then
        mktemp -t "$1.XXX"
    elif [ "$(uname)" = "FreeBSD" ]; then
        mktemp -t "$1"
    else
        mktemp -t "$1.XXX" #< Defaulting to same as Linux
    fi
}
TMPTREE=$(mktemp_aux treeviz)
TMPOPAM=$(mktemp_aux opamviz)
cleanup () {
  rm -rf "${TMPTREE}" "${TMPOPAM}"
}

trap cleanup EXIT

if [ -e "${CACHE_DIR}.dependencies.html" ]; then
    echo "Dependency visualization already exists ${CACHE_DIR}.dependencies.html"
else
    if ${BUILDER_VIZ} dependencies "${OPAM}" > "${TMPOPAM}"; then
        mv "${TMPOPAM}" "${CACHE_DIR}.dependencies.html"
    fi
fi

stat_aux () {
    if [ "$(uname)" = "Linux" ]; then
        stat -c "%s" "$1"
    elif [ "$(uname)" = "FreeBSD" ]; then
        stat -f "%z" "$1"
    else
        stat -c "%s" "$1" #< Defaulting to same as Linux
    fi
}

SIZE="$(stat_aux ${FILENAME})"

if [ ! -z "${DEBUG}" ]; then
    if [ -e "${CACHE_DIR}.treemap.html" ]; then
        echo "Treemap visualization already exists ${CACHE_DIR}.treemap.html"
    else
        if ${BUILDER_VIZ} treemap "${DEBUG}" "${SIZE}" > "${TMPTREE}"; then
            mv "${TMPTREE}" "${CACHE_DIR}.treemap.html"
        fi
    fi
else
    echo "No --debug-binary provided, not producing any treemap"
fi
