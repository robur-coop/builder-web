#!/bin/sh

while read version_a version_b; do
	version_a=$(echo $version_a | sed -e 's/-/./g')
	version_b=$(echo $version_b | sed -e 's/-/./g')
	printf "%s %s %s\n" "$version_a" $(pkg version -t "$version_a" "$version_b") "$version_b"
done < versions.txt

while read version_a version_b; do
	if dpkg --compare-versions "$version_a" lt "$version_b"; then
		echo "$version_a < $version_b"
	else
		echo "$version_a >= $version_b"
	fi
done < versions.txt
