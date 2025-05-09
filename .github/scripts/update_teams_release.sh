#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# update_teams_release.sh
#
# 1. Create an output directory [default = output]
# 2. Run every .R in R/teams/ with that directory as argument
# 3. Find & delete any existing GitHub release tagged "teams"
# 4. Create a fresh "teams" release, uploading all files from the output dir
# -----------------------------------------------------------------------------


OUTPUT_DIR=${1:-output}

echo "→ Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

echo "→ Executing R scripts in R/teams/"
for script in R/teams/*.R; do
  echo "   • Running $script → $OUTPUT_DIR"
  Rscript --vanilla "$script" "$OUTPUT_DIR"
done

NOTES_FILE=$(mktemp)
echo "This release provides structured CSV datasets containing detailed information about teams across multiple professional and collegiate sports leagues. The data has been meticulously collected from official sources and APIs, then processed into a clean, consistent format for easy use in analytics, applications, and research. For a detailed breakdown of data sources and table structures, please see the [Teams Documentation](/R/teams/readme.md)" > "$NOTES_FILE"
echo "" >> "$NOTES_FILE"
echo "`Update on  $(date -u "+%Y-%m-%d %H:%M UTC")`" >> "$NOTES_FILE"

# Delete any existing teams release from repository
if gh release view teams >/dev/null 2>&1; then
  echo "→ Deleting existing release tagged 'teams'"
  gh release delete teams --yes
else
  echo "→ No existing 'teams' release to delete"
fi

echo "→ Creating new 'teams' release"
gh release create teams \
  --title "Teams" \
  --notes-file "$NOTES_FILE" \
  "$OUTPUT_DIR"/*

rm "$NOTES_FILE"
echo "✅ Done."
