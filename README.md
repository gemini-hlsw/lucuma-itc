# lucuma-itc


This is a graphql server acting as a proxy for the old ocs2-based itc server

# Run

It is possible to run locally using sbt
```
   sbt ~service/reStart
```

note that using sbtn there is no output, see

https://github.com/spray/sbt-revolver/issues/99

## Env

The app needs two environment variable
* ITC_URL which needs to point to the address where old ocs2-based itc runs. e.g.
    ITC_URL = "https://itc-server-exp.herokuapp.com/"
* REDISCLOUD_URL which points to the redis server used for caching. e.g.
    REDISCLOUD_URL = "redis://localhost"

## Caching
ITC calculations are relatively expensive and they are pure (a given input always produces the same output)
the lucuma ITC server uses redis to store the results linking them from the request parameters to the results

The cache design is optimized to use minimal space given some of the responses (graphs) are fairly large.
We are also asuming we'll never need to go inside the cached data to edit the data and we can
discard items at any moment.

The keys are stored as `itc:prefix:hash` where hash is just the hash of the parameters

A few diferent encodings were tested to reduce size. Here are some measurement

* Plain json: 1441864
* Compressed json: 589896
* Boopickle: 262216

## Cache flushing
The only reason for the remote values to be stale is if the old itc changes (happens not very often)
`lucuma-itc` will check every 1h and verify if the itc data has changed. If so it will flush the whole cache

## Long term
Ideally we'd port the old ITC codebase and integrate it here. This is no small task but an initial
attempt was started on the `legacy-port` branch
