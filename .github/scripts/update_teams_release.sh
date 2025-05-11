#!/usr/bin/env bash
set -euo pipefail

OUTPUT_DIR=${1:-output}

echo "→ Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

echo "→ Executing R scripts in R/teams/"
for script in R/teams/*.R; do
  echo "   • Running $script → $OUTPUT_DIR"
  Rscript --vanilla "$script" "$OUTPUT_DIR"
done

NOTES_FILE=$(mktemp)
echo "This release provides structured CSV datasets containing detailed information about teams across multiple professional and collegiate sports leagues. The data has been collected from official sources and APIs, then processed into a clean, consistent format for easy use in analytics, applications, and research. For a detailed breakdown of data sources and table structures, please see the [Teams Documentation](/R/teams/readme.md)" > "$NOTES_FILE"
echo "" >> "$NOTES_FILE"
echo "\`Update on $(date -u "+%Y-%m-%d %H:%M UTC")\`" >> "$NOTES_FILE"

if gh release view teams >/dev/null 2>&1; then
  echo "→ Deleting existing release tagged 'teams'"
  gh release delete teams --yes
else
  echo "→ No existing 'teams' release to delete"
fi

if git rev-parse "teams" >/dev/null 2>&1; then
  echo "→ Deleting existing 'teams' tag"
  git tag -d teams
  git push --delete origin teams
else
  echo "→ No existing 'teams' tag to delete"
fi

assets=()
while IFS= read -r -d $'\0' file; do
  assets+=("$file")
done < <(
  find "$OUTPUT_DIR" -type f \
    -name '*-teams-*.*' \
    -print0
)

echo "→ Creating new 'teams' release"
gh release create teams \
  --title "Teams" \
  --notes-file "$NOTES_FILE" \
  "${assets[@]}"

rm "$NOTES_FILE"
echo "✅ Done."