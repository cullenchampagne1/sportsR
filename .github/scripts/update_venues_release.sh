#!/usr/bin/env bash
set -euo pipefail

OUTPUT_DIR=${1:-output}

echo "→ Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

echo "→ Executing R scripts in R/venues/"
for script in R/venues/*.R; do
  echo "   • Running $script → $OUTPUT_DIR"
  Rscript --vanilla "$script" "$OUTPUT_DIR"
done

NOTES_FILE=$(mktemp)
echo "This release provides structured CSV datasets containing detailed information about venues across multiple professional and collegiate sports leagues. The data has been collected from official wiki pages, then processed into a clean, consistent format for easy use in analytics, applications, and research. For a detailed breakdown of data sources and table structures, please see the [Venues Documentation](/R/venues/readme.md)" > "$NOTES_FILE"
echo "" >> "$NOTES_FILE"
echo "\`Update on $(date -u "+%Y-%m-%d %H:%M UTC")\`" >> "$NOTES_FILE"

if gh release view venues >/dev/null 2>&1; then
  echo "→ Deleting existing release tagged 'venues'"
  gh release delete venues --yes
else
  echo "→ No existing 'venues' release to delete"
fi

if git rev-parse "venues" >/dev/null 2>&1; then
  echo "→ Deleting existing 'venues' tag"
  git tag -d venues
  git push --delete origin venues
else
  echo "→ No existing 'venues' tag to delete"
fi

assets=()
while IFS= read -r -d $'\0' file; do
  assets+=("$file")
done < <(
  find "$OUTPUT_DIR" -type f \
    -name '*-venues-*.*' \
    -print0
)

echo "→ Creating new 'venues' release"
gh release create venues \
  --title "Venues" \
  --notes-file "$NOTES_FILE" \
  "${assets[@]}"

rm "$NOTES_FILE"
echo "✅ Done."