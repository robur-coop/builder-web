#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder-web
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "builder-web" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
systemddir=$rootdir/usr/lib/systemd/system
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir" "$debiandir" "$systemddir"

# stage app binaries
install $bdir/builder-web $bindir/builder-web
install $bdir/builder-migrations $bindir/builder-migrations
install $bdir/builder-db $bindir/builder-db
install $bdir/builder-viz $bindir/builder-viz

# service script
install -m 0644 $basedir/packaging/debian/builder-web.service $systemddir/builder-web.service

# install debian metadata
install -m 0644 $basedir/packaging/debian/control $debiandir/control
install -m 0644 $basedir/packaging/debian/changelog $debiandir/changelog
install -m 0644 $basedir/packaging/debian/copyright $debiandir/copyright
install $basedir/packaging/debian/postinst $debiandir/postinst

dpkg-deb --build $rootdir $basedir/builder-web.deb
echo 'bin: [ "builder-web.deb" ]' > $basedir/builder-web.install
echo 'doc: [ "README.md" ]' >> $basedir/builder-web.install
