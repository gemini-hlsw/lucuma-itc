#!/bin/bash

npm ci

./fetchODBSchema.mjs $@
node schemastitcher.js modules/service/src/main/resources/graphql/ObservationDB.graphql modules/service/src/main/resources/graphql/itc_base.graphql modules/service/src/main/resources/graphql/itc.graphql
