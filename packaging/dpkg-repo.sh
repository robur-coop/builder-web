#!/bin/sh

set -ex

export HOME="/home/builder"

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
Updates an aptly package repository
Options:
    --build-time=STRING
        Build timestamp (used for the version of the package).
    --sha256=STRING
        Hex encoded SHA256 digest of the main binary.
    --job=STRING
        Job name that was built.
    --platform=STRING
        Platform name on which the build was performed.
EOM
    exit 1
}

BUILD_TIME=
SHA=
JOB=
PLATFORM=

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
        --platform=*)
            PLATFORM="${OPT##*=}"
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
[ -z "${PLATFORM}" ] && die "The --platform option must be specified"

FILENAME="${1}"

if [ $(basename "${FILENAME}" .deb) = $(basename "${FILENAME}") ]; then
  echo "Not a Debian package"
  exit 0
fi

if aptly repo show -with-packages "${PLATFORM}" | grep "${SHA}" > /dev/null; then
  echo "Package with same SHA256 already in repository"
  exit 0
fi

TMP=$(mktemp -d -t debrep)

cleanup () {
  rm -rf "${TMP}"
}

trap cleanup EXIT

PKG_ROOT="${TMP}/pkg"

mkdir "${PKG_ROOT}"

dpkg-deb -R "${FILENAME}" "${PKG_ROOT}"

VERSION=$(dpkg-deb -f "${FILENAME}" Version)
# if we've a tagged version (1.5.0), append the number of commits and a dummy hash
VERSION_GOOD=$(echo $VERSION | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+$') || true
VERSION_WITH_COMMIT=$(echo $VERSION | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+\-[0-9]\+\-g[0-9a-fA-f]\+$') || true
if [ $VERSION_GOOD -eq 0 -a $VERSION_WITH_COMMIT -eq 0 ]; then
    die "version does not conform to (MAJOR.MINOR.PATCH[-#NUM_COMMITS-g<HASH>])"
fi
if [ $VERSION_WITH_COMMIT -eq 0 ]; then
    VERSION="${VERSION}-0-g0000000"
fi

NEW_VERSION="${VERSION}"-"${BUILD_TIME}"-"${SHA}"

sed -i "" -e "s/Version:.*/Version: ${NEW_VERSION}/g" "${PKG_ROOT}/DEBIAN/control"

dpkg-deb --build "${PKG_ROOT}" "${TMP}"

if ! aptly repo show "${PLATFORM}" > /dev/null 2>&1; then
  aptly repo create --distribution="${PLATFORM}" "${PLATFORM}"
fi

PACKAGE=$(dpkg-deb -f "${FILENAME}" Package)
aptly repo remove "${PLATFORM}" "${PACKAGE}"
aptly repo add "${PLATFORM}" "${TMP}"

: "${REPO_KEYID:="D5E2DC92617877EDF7D4FD4345EA05FB7E26053D"}"

if ! aptly publish show "${PLATFORM}" > /dev/null 2>&1; then
  aptly publish repo -gpg-key="${REPO_KEYID}" "${PLATFORM}"
else
  aptly publish update -gpg-key="${REPO_KEYID}" "${PLATFORM}"
fi
