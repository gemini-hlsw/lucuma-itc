# lucuma-itc


This is a graphql server acting as a proxy for the old ocs2-based itc server

# Run

It is possible to run locally using sbt

```
   sbt ~service/reStart
```

When using sbtn there is no output (see https://github.com/spray/sbt-revolver/issues/99).

Note, it is important to have [`git lfs`](https://git-lfs.com) installed in
order to obtain the necessary classes for running the ITC.  See (`git lfs` and
Legacy ITC Code below).

## Env

The app needs an environment variable, `REDISCLOUD_URL`, which points to the
redis server used for caching. For example: `REDISCLOUD_URL = "redis://localhost"`.

## Caching

ITC calculations are relatively expensive and they are pure (a given input always produces the same output)
the lucuma ITC server uses redis to store the results linking them from the request parameters to the results

The cache design is optimized to use minimal space given some of the responses (graphs) are fairly large.
We are also assuming we'll never need to go inside the cached data to edit the data and we can
discard items at any moment.

The keys are stored as `itc:prefix:hash` where hash is just the hash of the parameters

A few diferent encodings were tested to reduce size. Here are some measurement

* Plain json: 1441864
* Compressed json: 589896
* Boopickle: 262216

## Cache flushing

The only reason for the remote values to be stale is if the old itc changes (happens not very often)
`lucuma-itc` will check on startup and verify if the itc data has changed. If so it will flush the whole cache

## `git lfs` and Legacy ITC Code

The itc calculations are mostly done in java and scala using legacy technologies,
in particular libraries like scala 2.11, scalaz, argonaut.  We were wrapping this
code in an http server but that incurred considerable overhead especially when
the graph data was needed.  As an alternative we can now directly call the java
code but given the use of legacy libraries this requires the jar files to be
loaded dynamically by the application and be called via reflection with a custom
classloader

In case the code in ocs2 changes we need to update the jar files using the
`update.sh` script.  The jar files are fairly large (they contain the data files
used to calculate the itc results). Given github limitations these need to be
stored in `git lfs`.

[Install `git lfs`](https://git-lfs.com) to get the necessary jar files.

## Long term

Ideally we'd port the old ITC codebase and integrate it here. This is no small task but an initial
attempt was started on the `legacy-port` branch

## Buildpacks

This project needs the following buildpacks to be used in heroku (order is important)

```
=== itc-master Buildpack URLs
1. https://github.com/radian-software/heroku-buildpack-git-lfs
2. heroku/scala
3. https://github.com/opencounter/heroku-buildpack-post-build-clean.git
```

First we need git lfs to get the files stored in github lfs
Then scala to build the app
And finally post-build-clean to reduce the slug size
