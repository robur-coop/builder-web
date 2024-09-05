## v0.2.0 (2024-09-05)

A whole slew of changes. Internally, we made a lot of incremental changes and improvements without doing a release. Thus this release is rather big. There is a lot of database migrations to apply, and unfortunately they have to be applied one at a time.

* Add a /failed-builds/ endpoint that lists the most recent failed builds.
* By default don't display failed builds on the front page.
* Times are printed with the 'Z' time zone offset indicator.
* Link to comparisons of builds take into account whether the "input", among others the list of dependencies, is different.
* New subcommand `builder-db extract-build` takes a build UUID and extracts the builder "full" file.
* Add /job/<job>/build/<build>/all.tar.gz endpoint with a gzip compressed tar archive of all build artifacts.
* Visual overhaul.
* Add (optional) visualizations displaying package dependencies ("opam-graph") and for unikernels a "modulectomy" view of how much each OCaml module is contributing to the final binary size. The visualizations are read from a cache on disk and can be generated from a script.
* A script hook is added on file upload. It may be used to generate visualizations or publish system packages to a repository.
* The 404 file not found page tries to be more informative.
* The build page for a unikernel build displays the solo5 device manifest, e.g. `with block devices "storage", and net devices "service"`.
* URLs with trailing slash redirect to without the trailing slash.
* Builder-web will try to be more helpful if its database doesn't exist or the database version is wrong.
* The opam diff works for mirage 4 unikernels taking into account the opam-monorepo/duniverse packages.
* Markdown rendering is now done using cmarkit instead of omd.
* Builder-web doesn't display jobs older than 30 days (customizable with `--expired-jobs` command line argument) on the front page.
* Build artifacts are stored by their content, and artifacts are automatically deduplicated. This makes builder-web much more space efficient on deployments that don't use deduplication on the filesystem level.
* New subcommands `builder-db vacuum *` to remove older builds. Can be called from a cron job to keep disk usage bounded.
* Lots of other improvements and bug fixes.

## v0.1.0 (2021-11-12)

* Initial public release
