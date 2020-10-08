#!/bin/bash
for file in ./*.py; do
   [ -f "$file" ] || continue
   python -- "$file"
done
