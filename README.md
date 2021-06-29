# Builder-web - a web frontend for reproducible builds

Builder-web takes in submissions of builds, typically from [builder](https://github.com/roburio/builder/), and displays the produced artifacts in a way that makes it easy to compare checksums and build status.
Produced binaries can be downloaded and run by users.

## Overview

Builder-web is a single binary web server using a sqlite3 database with versioned schemas.
Finished builds from [builder](https://github.com/roburio/builder/) are uploaded to builder-web, stored and indexed in the database and presented in the web interface to the user.
Users can:

* Get an overview of *jobs* - a job is typically script or opam package that is run and builds an artifact, 
* Browse all *builds* of a job - each job may have multiple builds, that are executed periodically by builder
* Browse individual *build* and download artifacts and build information for reproducing the same binary. 
Each build has a single binary as output - which checksum is recorded and can be searched for. The build information (opam acpackges, environment variables, host system packages) can be compared, and they may differ even if the checksum of the binary is identical.

The build artifacts are stored on the filesystem, its metadata is stored in the database.

## Setup

Builder-web consists of a binary `builder-web` that runs a web server on port 3000 listening on all interfaces by default.
These values can be changed with the `--port` and `--host` flags respectively.
See `builder-web --help` for more information.

The web server expects a sqlite3 database in its data directory.

## Database migrations

The sqlite3 database builder-web uses contains versioning information.
On every schema change the database schema version is updated, and migration and rollback scripts are provided.
The tool for database migrations is `builder-migrations`.
See the `--help` output for each migration for further details.
