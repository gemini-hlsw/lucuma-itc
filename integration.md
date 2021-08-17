# explore integration with ITC

Explore will need to interact with ITC for several tasks, including integration
time estimations and displaying graphs

On explore's configuration table we show all possible modes (up to 400) and we need
to calculate the integration time for each with a call to ITC. Often we won't need to do the
calculation for every mode but tens of modes will probably be needed for each observation.

The call to calculate the time is essentially a function written in Scala as `ItcParams => F[ItcResult]`
and at the moment it can take from 1-5s to complete. 

Given the max theoretical of 400 (and growing) modes per observation this will quickly escalate and
we should take scalability into consideration from the start.

# ITC as an independent server
A first approach would be to integrate this into the API server but given ITC is stateless and the 
scalability requirements, we may rather have it as a separate server. This allows us to scale 
horizontally without worrying about e.g. db calls or the performance impact to the API users

We should move the implementation of ItcImpl from `basic-case` to `https://github.com/gemini-hlsw/itc-server`
and expose it via an http server.

# Format

The output of ITC at this stage is fairly simple and could be just plain json. An alternative is to
use GraphQL instead to be consistent with the rest of GPP.

# Some ideas to improve performance

* Reduce the amount of modes being queried from explore
* Improve caching on the ITC requests (may need to switch to GET requests)
* Add caching at the border. Heroku provides fastly for example
* Cache intermediate results with e.g. redis, memcached
* Review/rewrite the internal ITC code

# Move the code from old ocs2
We may need to eventually move the old ocs2 code to this repo but it is not needed right away
