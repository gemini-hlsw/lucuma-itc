#!/bin/bash

# gq https://lucuma-odb-development.herokuapp.com/odb --introspect >templates/src/main/resources/lucuma/schemas/ObservationDB.graphql
gq https://lucuma-odb-master.herokuapp.com/odb --introspect > modules/service/src/main/resources/graphql/ObservationDB.graphql
