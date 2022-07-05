#!/usr/bin/env bash

GIT_SHA1=$(git rev-parse HEAD)

echo "-- @""generated" > src/Pack/Version.idr
echo "module Pack.Version" >> src/Pack/Version.idr
echo "export" >> src/Pack/Version.idr
echo "version : String" >> src/Pack/Version.idr
echo "version = \"$GIT_SHA1\"" >> src/Pack/Version.idr
