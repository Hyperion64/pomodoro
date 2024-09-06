#!/bin/bash

FILE=$1
VOLUME=$2

VOLUME=$((VOLUME * 65536 / 100))

paplay --volume=$VOLUME "$FILE"
