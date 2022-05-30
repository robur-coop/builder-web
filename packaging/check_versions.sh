#!/bin/sh

# input in versions.txt is <v1> <v2> with v1 < v2.
# v1 and v2 are of the form <version>-<date>-<hash>, where <version> includes:
# - 2.0.0
# - 2.0.0-10-gabcdef

freebsd_sanitize_version () {
    post=$(echo $1 | rev | cut -d '-' -f 1-2 | rev | sed -e 's/-/./g')
    v=$(echo $1 | rev | cut -d '-' -f 3- | rev | sed -e 's/-/./g')
    version_good=$(echo $v | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+$')
    version_with_commit=$(echo $v | grep -c '^[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\.g[0-9a-fA-f]\+$')
    if [ $version_good -eq 0 -a $version_with_commit -eq 0 ]; then
        echo "invalid version $v";
        exit 1;
    fi
    if [ $version_with_commit -eq 0 ]; then
        v="${v}.0.g0000000"
    fi
    echo $v
}

while read version_a version_b; do
	version_a=$(freebsd_sanitize_version $version_a)
	version_b=$(freebsd_sanitize_version $version_b)
	result=$(pkg version -t "$version_a" "$version_b")
	printf "%s %s %s\n" "$version_a" "$result" "$version_b"
done < versions.txt

while read version_a version_b; do
	if dpkg --compare-versions "$version_a" lt "$version_b"; then
		echo "$version_a < $version_b"
	else
		echo "$version_a >= $version_b"
	fi
done < versions.txt
