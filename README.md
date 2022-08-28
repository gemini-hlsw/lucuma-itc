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

## Long term
Ideally we'd port the old ITC codebase and integrate it here. This is no small task but an initial
attempt was started on the `legacy-port` branch
