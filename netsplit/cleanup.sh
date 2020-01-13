#!/usr/bin/env bash

set -x

docker rm -f nodea nodeb nodec lb

docker network rm ab ac bc
