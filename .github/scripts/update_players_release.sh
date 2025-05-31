#!/usr/bin/env bash
set -euo pipefail

OUTPUT_DIR=${1:-output}

echo "→ Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

echo "→ Executing R scripts in R/players/"
for script in R/players/*.R; do
  echo "   • Running $script → $OUTPUT_DIR"
  Rscript --vanilla -e "source('$script'); get_formated_data()"
done

NOTES_FILE=$(mktemp)
echo "This release provides structured CSV datasets containing detailed information about players across multiple professional and collegiate sports leagues. The data has been collected from official sources and APIs, then processed into a clean, consistent format for easy use in analytics, applications, and research. For a detailed breakdown of data sources and table structures, please see the [Players Documentation](/R/players/readme.md)" > "$NOTES_FILE"
echo "" >> "$NOTES_FILE"
echo "\`Update on $(date -u "+%Y-%m-%d %H:%M UTC")\`" >> "$NOTES_FILE"

if gh release view players >/dev/null 2>&1; then
  echo "→ Deleting existing release tagged 'players'"
  gh release delete players --yes
else
  echo "→ No existing 'players' release to delete"
fi

if git rev-parse "players" >/dev/null 2>&1; then
  echo "→ Deleting existing 'players' tag"
  git tag -d players
  git push --delete origin players
else
  echo "→ No existing 'players' tag to delete"
fi

assets=()
while IFS= read -r -d $'\0' file; do
  assets+=("$file")
done < <(
  find "$OUTPUT_DIR" -type f \
    -name '*-players-*.*' \
    -print0
)

echo "→ Creating new 'players' release"
gh release create players \
  --title "Players" \
  --notes-file "$NOTES_FILE" \
  "${assets[@]}"

rm "$NOTES_FILE"
echo "✅ Done."