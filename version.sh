#!/usr/bin/env bash

GIT_SHA1=""

git status > /dev/null 2>&1

if [ $? -eq 0 ]; then
  GIT_SHA1=$(git rev-parse HEAD)
fi

echo "version = \"$GIT_SHA1\"" >> src/Pack/Version.idr
