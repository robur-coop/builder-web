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
sbindir=$rootdir/usr/sbin
systemddir=$rootdir/usr/lib/systemd/system
debiandir=$rootdir/DEBIAN
libexecdir=$rootdir/usr/libexec
sharedir=$rootdir/usr/share/builder-web
confdir=$rootdir/etc/builder-web

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$sbindir" "$debiandir" "$systemddir" "$libexecdir" "$sharedir" \
	"$confdir" "$confdir/upload-hooks"

# stage app binaries
install "$bdir/builder-web" "$libexecdir/builder-web"
install "$bdir/builder-migrations" "$sbindir/builder-migrations"
install "$bdir/builder-db" "$sbindir/builder-db"

# service script
install -m 0644 "$basedir/packaging/debian/builder-web.service" "$systemddir/builder-web.service"

# visualizations scripts
install "$basedir/packaging/batch-viz.sh" "$confdir/batch-viz.sh"
install "$basedir/packaging/visualizations.sh" "$confdir/upload-hooks/visualizations.sh"

# example repo scripts
install "$basedir/packaging/dpkg-repo.sh" "$sharedir/dpkg-repo.sh"
install "$basedir/packaging/FreeBSD-repo.sh" "$sharedir/FreeBSD-repo.sh"

# install debian metadata
install -m 0644 "$basedir/packaging/debian/control" "$debiandir/control"
install -m 0644 "$basedir/packaging/debian/changelog" "$debiandir/changelog"
install -m 0644 "$basedir/packaging/debian/copyright" "$debiandir/copyright"
install -m 0644 "$basedir/packaging/debian/conffiles" "$debiandir/conffiles"
install "$basedir/packaging/debian/postinst" "$debiandir/postinst"

ARCH=$(dpkg-architecture -q DEB_TARGET_ARCH)
sed -i -e "s/^Architecture:.*/Architecture: ${ARCH}/" "$debiandir/control"

dpkg-deb --build "$rootdir" "$basedir/builder-web.deb"
echo 'bin: [ "builder-web.deb" ]' > "$basedir/builder-web.install"
echo 'doc: [ "README.md" ]' >> "$basedir/builder-web.install"
