#!/usr/bin/env bash

GIT_SHA1=$(git rev-parse HEAD)

echo "version = \"$GIT_SHA1\"" >> src/Pack/Version.idr
