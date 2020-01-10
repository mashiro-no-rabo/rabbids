#!/usr/bin/env bash

set -x

docker rm -f nodea nodeb nodec

docker network rm ab ac bc
