#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "builder-web" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/packaging/FreeBSD
bdir=$basedir/_build/install/default/bin
#tmptmpl=$(basename "$0")
#tmpd=$(mktemp -t "$tmptmpl")
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpd/rootdir
sbindir=$rootdir/usr/local/sbin
rcdir=$rootdir/usr/local/etc/rc.d
libexecdir=$rootdir/usr/local/libexec
sharedir=$rootdir/usr/local/share/builder-web
confdir=$rootdir/usr/local/etc/builder-web

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$sbindir" "$libexecdir" "$rcdir" "$sharedir" "$confdir/upload-hooks"

# stage service scripts
install -U "$pdir/rc.d/builder_web" "$rcdir/builder_web"

# stage app binaries
install -U "$bdir/builder-web" "$libexecdir/builder-web"

install -U "$bdir/builder-migrations" "$sbindir/builder-migrations"
install -U "$bdir/builder-db" "$sbindir/builder-db"

# stage visualization scripts
install -U "$basedir/packaging/batch-viz.sh" "$confdir/batch-viz.sh"
install -U "$basedir/packaging/visualizations.sh" "$confdir/upload-hooks/visualizations.sh"

# example repo scripts
install -U "$basedir/packaging/dpkg-repo.sh" "$sharedir/dpkg-repo.sh"
install -U "$basedir/packaging/FreeBSD-repo.sh" "$sharedir/FreeBSD-repo.sh"

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

sed -e "s:%%FLATSIZE%%:${flatsize}:" -e "/^[Vv]ersion:/s/-/./g" "$pdir/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + | sort |
        awk '{print "    " $2 ": \"" $1 "\"," }'
    find "$rootdir" -type l | sort |
        awk "{print \"    \"\$1 \": -,\"}"
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

export SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%ct)
pkg create -r "$rootdir" -M "$manifest" -o "$basedir/"
mv "$basedir/builder-web-*.pkg" "$basedir/builder-web.pkg"
echo 'bin: [ "builder-web.pkg" ]' > "$basedir/builder-web.install"
echo 'doc: [ "README.md" ]' >> "$basedir/builder-web.install"
