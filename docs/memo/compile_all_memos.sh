#!/bin/bash

# Loop through all subfolders
for dir in */ ; do
    # Ignore "thermomemo"
    if [ "$dir" == "thermomemo/" ]; then
        continue
    fi

    # Enter the directory
    cd "$dir" || continue

    # Find all .tex files in the current directory
    tex_files=$(find . -maxdepth 1 -type f -name "*.tex")

    if [ -n "$tex_files" ]; then
        for tex_file in $tex_files; do
            # Remove the leading './' from the file name
            tex_file=${tex_file#./}

            echo "Processing $tex_file in $dir"

            # Run the required commands
            pdflatex "$tex_file"
            bibtex "${tex_file%.tex}"
            pdflatex "$tex_file"
            pdflatex "$tex_file"
        done
    else
        echo "No .tex files found in $dir"
    fi

    # Return to the parent directory
    cd ..
done
