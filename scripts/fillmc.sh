#!/bin/bash
# Fill memcached with random data
#
# Usage: ./fillmc.sh <host>
#
set -euo pipefail

host="$1"

for i in {1..100000}; do
    (
        echo -en "set key$i 0 900 1024\r\n"
        head -c 1024 < /dev/urandom
        echo -en "\r\n"
    )
done | nc -q0 "$host" 11211

