#!/usr/bin/env bash

set -ex

docker network disconnect bc nodeb
docker network disconnect bc nodec
