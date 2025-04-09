- routes:
  / - all builds except failing
  /all-builds - all builds including failing
  /failed-builds - only failed builds

  :job is just a string (ok for vif)
  :build is a uuid (we need a regex and a map from a recognized string to uuidm)
- sqlite request with caqti
- user space and authorization
  it's a simple Basic Authorization
- datadir contains the database, artifacts and cached visualizations
  - we need load the database with Caqti
