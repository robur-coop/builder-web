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
Updates a FreeBSD package repository
Options:
    --build-time=STRING
        Build timestamp (used for the version of the package).
    --sha256=STRING
        Hex encoded SHA256 digest of the main binary.
    --job=STRING
        Job name that was built.
EOM
    exit 1
}

BUILD_TIME=
SHA=
JOB=

while [ $# -gt 1 ]; do
    OPT="$1"

    case "${OPT}" in
        --build-time=*)
            BUILD_TIME="${OPT##*=}"
            ;;
        --sha256=*)
            SHA="${OPT##*=}"
            ;;
        --job=*)
            JOB="${OPT##*=}"
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

[ -z "${BUILD_TIME}" ] && die "The --build-time option must be specified"
[ -z "${SHA}" ] && die "The --sha256 option must be specified"
[ -z "${JOB}" ] && die "The --job option must be specified"

FILENAME="${1}"

: "${REPO:="/usr/local/www/pkg"}"
: "${REPO_KEY:="/usr/local/etc/builder-web/repo.key"}"

if [ "$(basename "${FILENAME}" .pkg)" = "$(basename "${FILENAME}")" ]; then
    echo "Not a FreeBSD package"
    exit 0
fi

if ls "${REPO}"/*/All/"${JOB}"-*."${SHA}".pkg > /dev/null; then
    echo "Same hash already present, nothing to do"
    exit 0
fi

TMP=$(mktemp -d -t repak)
MANIFEST="${TMP}/+MANIFEST"
TMPMANIFEST="${MANIFEST}.tmp"

cleanup () {
    rm -rf "${TMP}"
}

trap cleanup EXIT

PKG_ROOT="${TMP}/pkg"

tar x -C "${TMP}" -f "${FILENAME}"
mkdir "${PKG_ROOT}"
mv "${TMP}/usr" "${PKG_ROOT}"

VERSION=$(jq -r '.version' "${MANIFEST}")
# if we've a tagged version (1.5.0), append the number of commits and a dummy hash
VERSION_GOOD=$(echo $VERSION | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+$') || true
VERSION_WITH_COMMIT=$(echo $VERSION | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\.g[0-9a-fA-f]\+$') || true
if [ $VERSION_GOOD -eq 0 -a $VERSION_WITH_COMMIT -eq 0 ]; then
    die "version does not conform to (MAJOR.MINOR.PATCH[.#NUM_COMMITS.g<HASH>])"
fi
if [ $VERSION_WITH_COMMIT -eq 0 ]; then
    VERSION="${VERSION}.0.g0000000"
fi

NAME=$(jq -r '.name' "${MANIFEST}")
FULL_VERSION="${VERSION}.${BUILD_TIME}.${SHA}"

jq -ca ".version=\"$FULL_VERSION\"" "${MANIFEST}" > "${TMPMANIFEST}"
mv "${TMPMANIFEST}" "${MANIFEST}"

ABI=$(jq -r '.abi' "${MANIFEST}")
REPO_DIR="${REPO}/${ABI}"
PKG_DIR="${REPO_DIR}/All"

# to avoid races, first create the package in temporary directory
# and then move it before recreating the index
pkg create -r "${PKG_ROOT}" -m "${MANIFEST}" -o "${TMP}"
mkdir -p "${PKG_DIR}"
mv "${TMP}/${NAME}-${FULL_VERSION}.pkg" "${PKG_DIR}"

pkg repo "${REPO_DIR}" "${REPO_KEY}"
