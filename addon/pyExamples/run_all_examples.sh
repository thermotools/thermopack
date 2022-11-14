#!/bin/bash
for file in ./*.py; do
   [ -f "$file" ] || continue
   arch -x86_64 python -- "$file"
done
