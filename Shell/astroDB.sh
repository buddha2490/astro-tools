#!/bin/bash
# astro_backup.sh — mirror Astro-SSD to shared/astro-ssd
/Library/Frameworks/R.framework/Resources/R CMD BATCH --no-save --no-restore "/Volumes/Office-SSD/Astronomy/astro-tools/astroDB/astroDB.R" "/Volumes/Office-SSD/Astronomy/astro-tools/logs/astroDB.Rout"
