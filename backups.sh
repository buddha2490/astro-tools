#!/bin/bash

# Assign arguments to variables
source_dir="/Volumes/Astro-SSD"
destination_dir="/Volumes/Office HDD/Astro-HDD"

# Check if the source directory exists
if [ ! -d "$source_dir" ]; then
  echo "Source directory '$source_dir' does not exist."
  exit 1
fi

# Check if the destination directory exists, create it if not
if [ ! -d "$destination_dir" ]; then
  echo "Destination directory '$destination_dir' does not exist. Creating it."
  mkdir -p "$destination_dir"
fi

# Copy contents without overwriting
cp -n -r "$source_dir"/* "$destination_dir"

echo "Files copied from '$source_dir' to '$destination_dir' without overwriting.