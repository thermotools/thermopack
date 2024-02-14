#!/bin/bash
set -e
for file in ./*.py; do
    [ -f "$file" ] || continue
    echo "$file"
    python -- "$file"
done
