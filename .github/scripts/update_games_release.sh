#!/usr/bin/env bash
set -euo pipefail

OUTPUT_DIR=${1:-output}

echo "→ Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

echo "→ Executing R scripts in R/games/"
for script in R/games/*.R; do
  echo "   • Running $script → $OUTPUT_DIR"
  Rscript --vanilla -e "source('$script'); get_formated_data()"
done

NOTES_FILE=$(mktemp)
echo "This release provides structured CSV datasets containing detailed information about games across multiple professional and collegiate sports leagues. The data has been collected from official sources and APIs, then processed into a clean, consistent format for easy use in analytics, applications, and research. For a detailed breakdown of data sources and table structures, please see the [Games Documentation](/R/games/readme.md)" > "$NOTES_FILE"
echo "" >> "$NOTES_FILE"
echo "\`Update on $(date -u "+%Y-%m-%d %H:%M UTC")\`" >> "$NOTES_FILE"

if gh release view games >/dev/null 2>&1; then
  echo "→ Deleting existing release tagged 'games'"
  gh release delete games --yes
else
  echo "→ No existing 'games' release to delete"
fi

if git rev-parse "games" >/dev/null 2>&1; then
  echo "→ Deleting existing 'games' tag"
  git tag -d games
  git push --delete origin games
else
  echo "→ No existing 'games' tag to delete"
fi

assets=()
while IFS= read -r -d $'\0' file; do
  assets+=("$file")
done < <(
  find "$OUTPUT_DIR" -type f \
    -name '*-games-*.*' \
    -print0
)

echo "→ Creating new 'games' release"
gh release create games \
  --title "Games" \
  --notes-file "$NOTES_FILE" \
  "${assets[@]}"

rm "$NOTES_FILE"
echo "✅ Done."