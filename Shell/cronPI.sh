#!/bin/bash
# astro_backup.sh — mirror Astro-SSD to shared/astro-ssd
/Library/Frameworks/R.framework/Resources/R CMD BATCH --no-save --no-restore "/Users/briancarter/Astronomy/astro-tools/cronWBPP.R" "/Users/briancarter/astronomy/astro-tools/logs/cronwbpp.Rout"
