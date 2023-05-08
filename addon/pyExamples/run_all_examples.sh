#!/bin/bash
for file in ./*.py; do
    [ -f "$file" ] || continue
    echo "$file"
    python -- "$file"
done
