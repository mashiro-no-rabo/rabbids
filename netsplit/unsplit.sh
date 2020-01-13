#!/usr/bin/env bash

set -ex

docker network connect bc nodeb
docker network connect bc nodec
