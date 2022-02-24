#!/bin/sh

set -e

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
usage: ${prog_NAME} [ OPTIONS ] DATADIR
Generates visualizations of all things
EOM
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

DIR="${1}"

CACHE="${DIR}/_cache"

for i in $(find "${DIR}" -type f -path \*output/bin\*); do
    UUID=$(echo "${i}" | rev | cut -d '/' -f 4 | rev)
    ARGS="--cache-dir="${CACHE}" --uuid="${UUID}""
    FILE=$(basename "${i}")
    DIR=$(dirname "${i}")
    PDIR="${DIR}/.."
    ARGS2=
    if [ -f "${PDIR}/${FILE}.debug" ]; then
        ARGS2="${ARGS2} --debug-binary="${PDIR}/${FILE}.debug""
    fi
    if [ -f "${PDIR}/opam-switch" ]; then
        ARGS2="${ARGS2} --opam-switch="${PDIR}/opam-switch""
    fi
    if [ -z "${ARGS2}" ]; then
        echo "neither debug nor opam switch found for ${UUID}"
    else
        ARGS="${ARGS}${ARGS2} ${i}"
        ./visualizations.sh ${ARGS}
    fi
done
