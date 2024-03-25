#!/bin/sh

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
Generates visualizations of all things
    --data-dir=STRING
        Path to the data directory.
    --cache-dir=STRING
        Optional path to the cache directory. Defaults to DATA_DIR/_cache
    --viz-script=STRING
        Optional path to the visualizations.sh script. Defaults to ./visualizations.sh
    --ignore-done
        Optional flag to force script to ignore '.done' files
EOM
    exit 1
}

CACHE_DIR=
DATA_DIR=
VISUALIZATIONS_CMD="./visualizations.sh"
IGNORE_DONE="false"

while [ $# -gt 0 ]; do
    OPT="$1"

    case "${OPT}" in
        --cache-dir=*)
            CACHE_DIR="${OPT##*=}"
            ;;
        --data-dir=*)
            DATA_DIR="${OPT##*=}"
            ;;
        --viz-script=*)
            VISUALIZATIONS_CMD="${OPT##*=}"
            ;;
        --ignore-done)
            IGNORE_DONE="true"
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

[ -z "$DATA_DIR" ] && die "The --data-dir option must be specified"

DB="${DATA_DIR}/builder.sqlite3"
[ ! -e "$DB" ] && die "The database doesn't exist: '$DB'"

# Let's be somewhat lenient with the database version.
# In visualizations.sh we can be more strict.
DB_VERSION="$(sqlite3 "$DB" "PRAGMA user_version;")"
[ -z "$DB_VERSION" ] && die "Couldn't read database version from '$DB'"
[ "$DB_VERSION" -lt 16 ] && die "The database version should be >= 16. It is '$DB_VERSION'"

APP_ID="$(sqlite3 "$DB" "PRAGMA application_id;")"
[ -z "$APP_ID" ] && die "Couldn't read application-id from '$DB'"
[ "$APP_ID" -ne 1234839235 ] && die "The application-id should be = 1234839235. It is '$APP_ID'"

echo
echo "-----------------------------------------------------------------------------"
info "Starting batch creation of visualizations: $(date)"

if [ -z "$CACHE_DIR" ]; then
    CACHE_DIR="${DATA_DIR}/_cache"
    info "Defaulting --cache-dir to '$CACHE_DIR'"
fi
if [ ! -d "${CACHE_DIR}" ]; then
    info "Cache directory '$CACHE_DIR' doesn't exist, so it will be made"
    if ! mkdir "${CACHE_DIR}"; then
        die "Couldn't make cache directory: '$CACHE_DIR'"
    fi
fi

[ ! -e "${VISUALIZATIONS_CMD}" ] && die "'$VISUALIZATIONS_CMD' doesn't exist"
if [ -f "${VISUALIZATIONS_CMD}" ] && [ -x "${VISUALIZATIONS_CMD}" ]; then :; else
	die "'$VISUALIZATIONS_CMD' is not an executable"
fi

OPAM_GRAPH="opam-graph"
MODULECTOMY="modulectomy"

LATEST_TREEMAPVIZ_VERSION="$($MODULECTOMY --version)"
[ $? -ne 0 ] && die "Couldn't get modulectomy version"
LATEST_DEPENDENCIESVIZ_VERSION="$($OPAM_GRAPH --version)"
[ $? -ne 0 ] && die "Couldn't get opam-graph version"

TREEMAP_CACHE_DIR="${CACHE_DIR}/treemap_${LATEST_TREEMAPVIZ_VERSION}"
DEPENDENCIES_CACHE_DIR="${CACHE_DIR}/dependencies_${LATEST_DEPENDENCIESVIZ_VERSION}"

if
    [ "${IGNORE_DONE}" = "false" ] && \
    [ -f "${TREEMAP_CACHE_DIR}/.done" ] && \
    [ -f "${DEPENDENCIES_CACHE_DIR}/.done" ]; then
    info "Nothing to do"
    exit 0
fi

ATTEMPTED_VIZS=0
FAILED_VIZS=0

distinct-input () {
    {
        sqlite3 "${DATA_DIR}/builder.sqlite3" "SELECT b.uuid
            FROM build b
            JOIN build_artifact opam ON opam.build = b.id
            WHERE opam.filepath = 'opam-switch' AND b.main_binary NOT NULL
            GROUP BY opam.sha256;"
        sqlite3 "${DATA_DIR}/builder.sqlite3" "SELECT b.uuid
            FROM build b
            JOIN build_artifact debug ON debug.build = b.id
            WHERE debug.filepath LIKE '%.debug' AND b.main_binary NOT NULL
            GROUP BY debug.sha256;"
    } | sort -u
}

for UUID in $(distinct-input); do
    if ! "$VISUALIZATIONS_CMD" \
         --data-dir="${DATA_DIR}" \
         --cache-dir="${CACHE_DIR}" \
         --uuid="${UUID}"
    then
        FAILED_VIZS=$((FAILED_VIZS + 1))
    fi
    ATTEMPTED_VIZS=$((ATTEMPTED_VIZS + 1))
done

if [ -n "$(ls -A "${TREEMAP_CACHE_DIR}")" ]; then
    touch "${TREEMAP_CACHE_DIR}/.done"

    V=1
    while [ "$V" -lt "$LATEST_TREEMAPVIZ_VERSION" ]; do
        DIR_REMOVE="${CACHE_DIR}/treemap_${V}"
        if test -d "$DIR_REMOVE" && rm -r "$DIR_REMOVE"; then
            info "Removed old cache-directory: '$DIR_REMOVE'"
        fi
        V=$((V+1))
    done
else
    warn "Treemap-viz cache-directory is still empty - problem?"
fi

if [ -n "$(ls -A "${DEPENDENCIES_CACHE_DIR}")" ]; then
    touch "${DEPENDENCIES_CACHE_DIR}/.done"

    V=1
    while [ "$V" -lt "$LATEST_DEPENDENCIESVIZ_VERSION" ]; do
        DIR_REMOVE="${CACHE_DIR}/dependencies_${V}"
        if test -d "$DIR_REMOVE" && rm -r "$DIR_REMOVE"; then
            info "Removed old cache-directory: '$DIR_REMOVE'"
        fi
        V=$((V+1))
    done
else
    warn "Dependencies-viz cache-directory is still empty - problem?"
fi

info "Batch creation of visualizations for $ATTEMPTED_VIZS binaries, finished with $FAILED_VIZS failures: $(date)"

