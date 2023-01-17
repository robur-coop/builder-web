#! /bin/bash

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

PERFJOB_DIR="$1"
CACHE_DIR="$2"
DB="$3"
JOB="$4"
LATEST_UUID="$5"

DIMS=1920,1080
COLS=5,7,8 #resp-time, throughput, concurrent
#< Note: gnuplot can select the final columns

DAT="${PERFJOB_DIR}/tmp.dat"

#> Note: ordering desc for plot ordering left->right
get_jobs_build-uuids () {
    sqlite3 "$DB" "select b.uuid from build as b \
        join job as j on j.id = b.job\
        where j.name = '$JOB' \
        order by b.start_d asc, b.start_ps asc"
}

get_bin_hash () {
    UUID="$1"
    sqlite3 "$DB" "SELECT lower(hex(ba.sha256)) FROM build AS b
        JOIN build_artifact AS ba ON ba.id = b.main_binary
        WHERE uuid = '$UUID'"
}

N=0

while read -r UUID; do

    BIN_SHA256=$(get_bin_hash "$UUID")
    CSV="${PERFJOB_DIR}/${BIN_SHA256}/siege_test01.csv"

    if [ ! -f "$CSV" ]; then
        info "Skipping build with uuid '$UUID'. Test-data doesn't exist: '$CSV'"
        continue
    fi
    
    if [ $N = 0 ]; then
        echo -n "# Build UUID - " > "$DAT"
        cat "$CSV" | head -n1 | cut -d, -f"$COLS" \
            | sed 's/,//g' \
            | sed 's/ \+//' \
            | sed 's/\(  \+\)/ -\1/g' \
                  >> "$DAT"
    fi

    #goto validate csv file, and on invalid: append error-data instead

    info appending line from "$CSV" of uuid "$UUID"
    UUID_CUT=$(echo "$UUID" | cut -d- -f1)
    
    echo -n "$UUID_CUT      " >> "$DAT"
    cat "$CSV" | tail +2 | cut -d, -f"$COLS" \
        | sed 's/,//g' \
        | sed 's/ \+//' \
              >> "$DAT"
    
    N=$(($N + 1))

done < <(get_jobs_build-uuids)

PLOT_VERSION=1
PLOT_NAME="Throughput for 30 concurrent threads"
OUT_DIR="${CACHE_DIR}/perftest/${JOB}/${PLOT_NAME}_${PLOT_VERSION}"
if [ ! -e "$OUT_DIR" ]; then
    mkdir -p "$OUT_DIR"
fi
OUT_IMG="${OUT_DIR}/${LATEST_UUID}.png"

info generating plot: "$OUT_IMG"
gnuplot >"$OUT_IMG" <<EOF
set terminal png size $DIMS background rgb "gray40"
set output '$OUT_IMG'
set title '$PLOT_NAME'
set style data histograms
set style histogram clustered gap 2
set style fill solid 1.0 border lt -1
set ylabel "Throughput"
plot '$DAT' using 3:xtic(1)
EOF

#rm "$DAT"

info done
