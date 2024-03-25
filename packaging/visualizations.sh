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
Generates visualizations
Options:
    --uuid=STRING
        UUID of build.
    --data-dir=STRING
        Path to the data directory.
    --cache-dir=STRING
        Path to the cache directory.
EOM
    exit 1
}

UUID=
CACHE_DIR=
DATA_DIR=

while [ $# -gt 0 ]; do
    OPT="$1"

    case "${OPT}" in
        --uuid=*)
            UUID="${OPT##*=}"
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
[ -z "${CACHE_DIR}" ] && die "The --cache-dir option must be specified"
[ -z "${DATA_DIR}" ] && die "The --data-dir option must be specified"

info "processing UUID '${UUID}'"

DB="${DATA_DIR}/builder.sqlite3"

# A new visualizations.sh script may be installed during an upgrade while the
# old builder-web binary is running. In that case things can get out of sync.
DB_VERSION="$(sqlite3 "$DB" "PRAGMA user_version;")"
[ -z "$DB_VERSION" ] && die "Couldn't read database version from '$DB'"
[ "$DB_VERSION" -ne 18 ] && die "The database version should be 18. It is '$DB_VERSION'"

APP_ID="$(sqlite3 "$DB" "PRAGMA application_id;")"
[ -z "$APP_ID" ] && die "Couldn't read application-id from '$DB'"
[ "$APP_ID" -ne 1234839235 ] && die "The application-id should be 1234839235. It is '$APP_ID'"

get_main_binary () {
    sqlite3 "${DB}" "SELECT '_artifacts/' || substr(lower(hex(ba.sha256)), 1, 2) || '/' || lower(hex(ba.sha256))
        FROM build AS b
        JOIN build_artifact AS ba ON ba.build = b.id AND b.main_binary = ba.id
        WHERE uuid = '${UUID}';"
}

BIN="${DATA_DIR}/$(get_main_binary)" || die "Failed to get main binary from database"
[ -z "${BIN}" ] && die "No main-binary found in db '${DB}' for build '${UUID}'"

get_debug_binary () {
    sqlite3 "${DB}" "SELECT '_artifacts/' || substr(lower(hex(ba.sha256)), 1, 2) || '/' || lower(hex(ba.sha256))
        FROM build AS b
        JOIN build_artifact AS ba ON ba.build = b.id
        WHERE
          uuid = '${UUID}'
          AND ba.filepath LIKE '%.debug';"
}

DEBUG_BIN_RELATIVE="$(get_debug_binary)" || die "Failed to get debug binary from database"

get_opam_switch () {
    sqlite3 "${DB}" "SELECT '_artifacts/' || substr(lower(hex(ba.sha256)), 1, 2) || '/' || lower(hex(ba.sha256))
        FROM build AS b
        JOIN build_artifact AS ba ON ba.build = b.id
        WHERE
          uuid = '${UUID}'
          AND ba.filepath = 'opam-switch';"
}

OPAM_SWITCH="$(get_opam_switch)" || die "Failed to get opam switch from database"
[ -z "${OPAM_SWITCH}" ] && die "No 'opam-switch' found in db '${DB}' for build '${UUID}'"
OPAM_SWITCH="${DATA_DIR}/${OPAM_SWITCH}"

OPAM_GRAPH="opam-graph"
MODULECTOMY="modulectomy"

LATEST_TREEMAPVIZ_VERSION="$(${MODULECTOMY} --version)" || die "Failed to get modulectomy version"
LATEST_DEPENDENCIESVIZ_VERSION="$(${OPAM_GRAPH} --version)" || die "Failed to get opam-graph version"

TREEMAP_CACHE_DIR="${CACHE_DIR}/treemap_${LATEST_TREEMAPVIZ_VERSION}"
DEPENDENCIES_CACHE_DIR="${CACHE_DIR}/dependencies_${LATEST_DEPENDENCIESVIZ_VERSION}"

mktemp_aux () {
    if [ "$(uname)" = "Linux" ]; then
        mktemp -t "$1.XXX"
    elif [ "$(uname)" = "FreeBSD" ]; then
        mktemp -t "$1"
    else
        die 'Unsupported platform'
    fi
}

TMPTREE=$(mktemp_aux viz_treemap)
TMPDEPENDENCIES=$(mktemp_aux viz_dependencies)

cleanup () {
  rm -rf "${TMPTREE}" "${TMPDEPENDENCIES}"
}

trap cleanup EXIT

# /// Dependencies viz

if [ ! -d "${DEPENDENCIES_CACHE_DIR}" ]; then
    mkdir "${DEPENDENCIES_CACHE_DIR}" || die "Failed to create directory '${DEPENDENCIES_CACHE_DIR}'"
fi

OPAM_SWITCH_FILEPATH='opam-switch'

get_opam_switch_hash () {
    sqlite3 "${DB}" "SELECT lower(hex(ba.sha256)) FROM build AS b
        JOIN build_artifact AS ba ON ba.build = b.id
        WHERE uuid = '${UUID}'
        AND ba.filepath = '${OPAM_SWITCH_FILEPATH}';"
}

DEPENDENCIES_INPUT_HASH="$(get_opam_switch_hash)" || die "Failed to get opam-switch hash from database"
DEPENDENCIES_VIZ_FILENAME="${DEPENDENCIES_CACHE_DIR}/${DEPENDENCIES_INPUT_HASH}.html"

if [ -e "${DEPENDENCIES_VIZ_FILENAME}" ]; then
    info "Dependency visualization already exists: '${DEPENDENCIES_VIZ_FILENAME}'"
else
    if ${OPAM_GRAPH} --output-format=html "${OPAM_SWITCH}" > "${TMPDEPENDENCIES}"; then
        cp "${TMPDEPENDENCIES}" "${DEPENDENCIES_VIZ_FILENAME}"
        rm "${TMPDEPENDENCIES}"
    else
        die "opam-graph failed to generate visualization"
    fi
fi

# /// Treemap viz

stat_aux () {
    if [ "$(uname)" = "Linux" ]; then
        stat -c "%s" "$1"
    elif [ "$(uname)" = "FreeBSD" ]; then
        stat -f "%z" "$1"
    else
        die 'Unsupported platform'
    fi
}

SIZE="$(stat_aux "${BIN}")"

if [ ! -d "${TREEMAP_CACHE_DIR}" ]; then
    mkdir "${TREEMAP_CACHE_DIR}" || die "Failed to create directory '${TREEMAP_CACHE_DIR}'"
fi

get_debug_bin_hash () {
    sqlite3 "${DB}" "SELECT lower(hex(ba.sha256)) FROM build AS b
        JOIN build_artifact AS ba ON ba.build = b.id
        WHERE uuid = '${UUID}'
        AND ba.filepath LIKE '%.debug';"
}

TREEMAP_INPUT_HASH="$(get_debug_bin_hash)" || die "Failed to get treemap input hash from database"
TREEMAP_VIZ_FILENAME="${TREEMAP_CACHE_DIR}/${TREEMAP_INPUT_HASH}.html"

if [ -n "${DEBUG_BIN_RELATIVE}" ]; then
    DEBUG_BIN="${DATA_DIR}/$(get_debug_binary)"
    if [ -e "${TREEMAP_VIZ_FILENAME}" ]; then
        info "Treemap visualization already exists: '${TREEMAP_VIZ_FILENAME}'"
    else
        if
            ${MODULECTOMY} \
                --robur-defaults \
                --with-scale="${SIZE}" \
                "${DEBUG_BIN}" \
                > "${TMPTREE}"
        then
            cp "${TMPTREE}" "${TREEMAP_VIZ_FILENAME}"
            rm "${TMPTREE}"
        else
            die "modulectomy failed to generate visualization"
        fi
    fi
else
    info "No --debug-binary provided, not producing any treemap"
fi
