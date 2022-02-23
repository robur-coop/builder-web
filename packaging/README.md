# Package repository creation and update

Builder-web calls hooks when an upload of a successful build finished. These
shell scripts automatically push builds to deb repositories (using aptly) and
FreeBSD package repositories (using pkg).

Thus, as a client of the infrastructure, system packages can be easily
installed using the package repositories (and updates are straightforward).

The tricky part is verioning: different input may result in the same output
(i.e. if the build system is updated, it is unlikely this will result in change
of output, and clients do not need to update their packages), and also due to
the nature of opam, if a dependency (opam package) is released, the output may
differ (although the final package version is not increased). We solve the
latter by adapting the version number of packages: package version 1.5.2 becomes
1.5.2-TIMESTAMP-SHA256. The timestamp is of the form YYYYMMDDhhmmss. The SHA256
is the hex-encoded SHA256 checksum of the original binary package and can be
used for lookup in the database.

## DPKG package repository

The dependencies are aptly and dpkg.

For the initial setup, a GPG private key is needed:
```
$ gpg --full-generate-key
$ gpg --export --armor > gpg.pub
```

Set REPO_KEYID in the shell script to the key identifier generated
(`gpg --list-keys`), and make the gpg.pub available to clients
(`cp gpg.pub ~/.aptly/public/`).

On clients, when the `~/.aptly/public` is served via http(s), add it to your
/etc/apt/source.list and import the gpg public key (`apt-key add <gpg.pub>`):

```
deb https://apt.robur.coop/ debian-10 main
```

The `debian-10` can be exchanged with any platform you're building debian
packages for.

## FreeBSD package repository

The dependency is FreeBSD's pkg utility.

For the initial setup, a RSA private key is needed:
```
$ openssl genrsa -out repo.key 4096
$ chmod 0400 repo.key
$ openssl rsa -in repo.key -out repo.pub -pubout
```

And a directory that acts as package repository (`mkdir /usr/local/www/pkg`).
Copy the public key to the package repository
(`cp repo.pub /usr/local/www/pkg`) to make it available for clients.

Both can be configured in the shell script itself (REPO and REPO_KEY). The
public key needs to be distributed to clients - e.g. put it at the root of the
repository.

On clients, when that directory is served via http(s), it can be added to
/usr/local/etc/pkg/repos/robur.conf:

```
robur: {
  url: "https://pkg.robur.coop/${ABI}",
  mirror_type: "srv",
  signature_type: "pubkey",
  pubkey: "/path/to/repo.pub",
  enabled: yes
}
```
