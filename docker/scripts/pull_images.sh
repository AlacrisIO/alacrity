#!/usr/bin/env bash

echo "Pulling prerequisites images....please wait, this might take a while depending on your Internet connection speed"

run() {
  $*
  if [ $? -ne 0 ]
  then
    echo "$* failed with exit code $?"
    return 1
  else
    return 0
  fi
}

run docker pull gcr.io/legicash-demo-1950/legicash-demo/alacrity_build_prerequisites:latest
run docker pull gcr.io/legicash-demo-1950/legicash-demo/alacrity_demo:latest
run docker pull gcr.io/legicash-demo-1950/legicash-demo/alacrity_dev:latest
run docker pull gcr.io/legicash-demo-1950/legicash-demo/alacrity_geth:v1.8.21

