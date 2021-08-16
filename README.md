# itc-server


This is a *temporary* packaged-up version of the Gemini ITC server with the subset of the library jars required to implement just the JSON endpoint. We can deploy this to Heroku and make it available externally.

## Setting Up

First, build the ITC in your ocs codebase.

    sbt
    > project app_itc
    > ocsDist Test

Now run `update.sh`, passing the generated bundle directory. Something like

    $ ./update.sh ../ocs/app/itc/target/itc/2016A-test.1.1.1/Test/itc/bundle/
    🔸 Reading from /Users/rnorris/Scala/ocs/app/itc/target/itc/2016A-test.1.1.1/Test/itc/bundle
    🔸 Writing to   /Users/rnorris/Scala/itc-server/lib
    🔸 Removing old library bundles.
    🔸 Copying bundles.
    $

This will copy the necessary jars into the local `lib/` directory. This directory is ignored by Git and is not part of the repository.

## Running

Once you're set up you can run locally with `sbt run`; or with `bloop run itc-server` if you have bloop set up. There is a sample request in `data.json` so if you're in the project root you can say:

    curl http://localhost:8080/json -d @data.json

and get back a response like

    {
      "ItcSpectroscopyResult" : {
        "ccds" : [
          {
            "wellDepth" : 125000.0,
            "peakPixelFlux" : 0.03607547711165799,
            "warnings" : [],
            "singleSNRatio" : 4.6417881419574975E-194,
            "totalSNRatio" : 7.339311472273343E-194,
            "ampGain" : 5.11
          },
          ...
        ],
        "chartGroups" : []
      }
    }

It may so happen that different inputs will ened up traversing into code that's not in the `lib/` directory so if that happens we just need to add missing jarfile to the update script.

## Deploying

To deploy you need to first be logged into Heroku and also logged into the docker registry. So

    heroku login

and then

    heroku container:login

You probably only need to do this once, I don't know. Anyway you can then build the app with

    sbt docker:publish

which will build and upload the `web` image to the Docker registry for the `gemini-new-itc`. To deploy this new version you say

    heroku container:release -a gemini-new-itc web

To watch the logs you can say

    heroku logs -a gemini-new-itc -t

The endpoint for the Heroku app is

    https://gemini-new-itc.herokuapp.com/

