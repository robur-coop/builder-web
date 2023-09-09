# Builder-web - a web frontend for reproducible builds

Builder-web takes in submissions of builds, typically from [builder](https://github.com/robur-coop/builder/), and displays the produced artifacts in a way that makes it easy to compare checksums and build status.
Produced binaries can be downloaded and executed.
[builds.robur.coop](https://builds.robur.coop/) itself runs builder-web.

## Overview

Builder-web is a single binary web server using a sqlite3 database with versioned schemas.
Finished builds from [builder](https://github.com/robur-coop/builder/) are uploaded to builder-web, stored and indexed in the database and presented in the web interface to the user.
Users can:

* Get an overview of *jobs* - a job is typically script or opam package that is run and builds an artifact,
* Browse all *builds* of a job - each job may have multiple builds, that are executed periodically by builder
* Browse individual *build* and download artifacts and build information for reproducing the same binary.
* Compare two builds, observing the differences in used opam packages, environment variables, and system packages.
* Search for the SHA-256 hash of a binary to view a build that resulted in that binary.

## Installation

Installing from source can be done with opam: `opam install builder-web`.

We also provide [reproducible binary packages](https://builds.robur.coop/job/builder-web/).

## Setup

Builder-web consists of a binary `builder-web` that runs a web server on port 3000 listening on all interfaces by default.
These values can be changed with the `--port` and `--host` flags respectively.
See `builder-web --help` for more information.

Service scripts for FreeBSD and systemd are provided.

The web server expects a sqlite3 database in its data directory.
An empty database can be created with `builder-db migrate`.

## Database migrations

The sqlite3 database builder-web uses contains versioning information.
On every schema change the database schema version is updated, and migration and rollback scripts are provided.
The tool for database migrations is `builder-migrations`.
See the `builder-migrations --help` output for each migration for further details.

## Less common workflows

Here are listed some less common but useful workflows:

### Extracting builds from one server to another

This is useful for development on a separate machine that doesn't run the build jobs itself.

On the source server:
```ocaml
builder-db extract-build <build-hash> --dest <build-hash>.full
```

After copying the file over the destination server (you need a user first,
see `builder-db user-add --help`):
```ocaml
curl --data-binary @<build-hash>.full http://<user>:<passwd>@localhost:<builder-web-port>/upload
```
