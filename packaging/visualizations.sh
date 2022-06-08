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

info "processing UUID '$UUID'"

DB="${DATA_DIR}/builder.sqlite3"

get_main_binary () {
    sqlite3 "${DB}" <<EOF
        select ba.localpath from build as b
        join build_artifact as ba on ba.build = b.id and b.main_binary = ba.id
        where uuid = '$UUID';
EOF
}

BIN="${DATA_DIR}/$(get_main_binary)"
[ -z "${BIN}" ] && die "No main-binary found in db '$DB' for build '$UUID'"

get_debug_binary () {
    sqlite3 "${DB}" <<EOF
        select ba.localpath from build as b
        join build_artifact as ba on ba.build = b.id 
        where 
          uuid = '$UUID'
          and ba.localpath like '%.debug';
EOF
}

DEBUG_BIN_RELATIVE="$(get_debug_binary)"

get_opam_switch () {
    sqlite3 "${DB}" <<EOF
        select ba.localpath from build as b
        join build_artifact as ba on ba.build = b.id 
        where 
          uuid = '$UUID'
          and ba.filepath = 'opam-switch';
EOF
}

OPAM_SWITCH="$(get_opam_switch)"
[ -z "${OPAM_SWITCH}" ] && die "No 'opam-switch' found in db '$DB' for build '$UUID'"
OPAM_SWITCH="${DATA_DIR}/${OPAM_SWITCH}"

#START debug print values
# echo "UUID = $UUID"
# echo "CACHE_DIR = $CACHE_DIR"
# echo "DATA_DIR = $DATA_DIR"
# echo "DB = $DB"
# echo "BIN = $BIN"
# echo "DEBUG_BIN = $DEBUG_BIN"
# echo "OPAM_SWITCH = $OPAM_SWITCH"
#END debug print values

OPAM_GRAPH="opam-graph"
MODULECTOMY="modulectomy"

LATEST_TREEMAPVIZ_VERSION="$($MODULECTOMY --version)"
LATEST_DEPENDENCIESVIZ_VERSION="$($OPAM_GRAPH --version)"

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
    mkdir "${DEPENDENCIES_CACHE_DIR}"
fi

OPAM_SWITCH_FILEPATH='opam-switch'

get_opam_switch_hash () {
    sqlite3 "${DB}" <<EOF
        select lower(hex(ba.sha256)) from build as b
        join build_artifact as ba on ba.build = b.id
        where uuid = '$UUID' 
        and ba.filepath = '$OPAM_SWITCH_FILEPATH';
EOF
}

DEPENDENCIES_INPUT_HASH="$(get_opam_switch_hash)" 
DEPENDENCIES_VIZ_FILENAME="${DEPENDENCIES_CACHE_DIR}/${DEPENDENCIES_INPUT_HASH}.html"

if [ -e "${DEPENDENCIES_VIZ_FILENAME}" ]; then
    info "Dependency visualization already exists: '${DEPENDENCIES_VIZ_FILENAME}'"
else
    if ${OPAM_GRAPH} --output-format=html "${OPAM_SWITCH}" > "${TMPDEPENDENCIES}"; then
        mv "${TMPDEPENDENCIES}" "${DEPENDENCIES_VIZ_FILENAME}"
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

SIZE="$(stat_aux "$BIN")"

if [ ! -d "${TREEMAP_CACHE_DIR}" ]; then
    mkdir "${TREEMAP_CACHE_DIR}"
fi

get_debug_bin_hash () {
    sqlite3 "${DB}" <<EOF
        select lower(hex(ba.sha256)) from build as b
        join build_artifact as ba on ba.build = b.id
        where uuid = '$UUID' 
        and ba.filepath like '%.debug';
EOF
}

TREEMAP_INPUT_HASH="$(get_debug_bin_hash)" 
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
            mv "${TMPTREE}" "${TREEMAP_VIZ_FILENAME}"
        else
            die "modulectomy failed to generate visualization"
        fi
    fi
else
    info "No --debug-binary provided, not producing any treemap"
fi
