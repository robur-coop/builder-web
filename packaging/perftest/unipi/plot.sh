#! /bin/bash

set -e

PERFJOB_DIR="$1"
CACHE_DIR="$2"
DB="$3"
JOB="$4"
LATEST_UUID="$5"

DIMS=1920,1080
COLS=5,7,8 #resp-time, throughput, concurrent
#< Note: gnuplot can select the final columns

DAT="${PERFJOB_DIR}/tmp.dat"

get_jobs_build-uuids () {
    sqlite3 "$DB" "select b.uuid from build as b \
        join job as j on j.id = b.job\
        where j.name = 'albatross' \
        order by b.id asc"
}

get_bin_hash () {
    UUID="$1"
    sqlite3 "$DB" "SELECT lower(hex(ba.sha256)) FROM build AS b
        JOIN build_artifact AS ba ON ba.id = b.main_binary
        WHERE uuid = '$UUID'"
}

N=0

while read UUID; do

    BIN_SHA256=$(get_bin_hash "$UUID")
    CSV="${PERFJOB_DIR}/${BIN_SHA256}/siege.csv"

    if [ $N = 1 ]; then
        echo -n "# Build UUID - "
        echo $(cat "$CSV" | head -n1 | cut -d, -f"$COLS" \
                   | sed 's/,//g' \
                   | sed 's/ \+//' \
                   | sed 's/\(  \+\)/ -\1/g' \
            ) > "$DAT"
    fi

    #goto validate csv file, and on invalid: append error-data instead 
    
    echo -n "$UUID      " >> "$DAT"
    cat "$CSV" | tail +2 | cut -d, -f"$COLS" \
        | sed 's/,//g' \
        | sed 's/ \+//' >> "$DAT"
    
    N=$(($N + 1))

done < <(get_jobs_build-uuids)

PLOT_VERSION=1
PLOT_NAME=throughput
OUT_IMG="${CACHE_DIR}/perf/${JOB}/${PLOT_NAME}_${PLOT_VERSION}/${LATEST_UUID}.png"

gnuplot <<EOF
set terminal png size $DIMS
set output '$OUT_IMG'
set title 'Concurrent'
set style data histograms
plot '$DAT' using 4:xtic(1)
EOF

rm "$DAT"
