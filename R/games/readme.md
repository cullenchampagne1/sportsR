## NFL Games

![Missing Values](../../output/tables/nfl_football_games_missing_data.png)

 Retrieves NFL game data from espn's api and other sources. The combined data is processed into a structured dataframe and saved to a CSV file. 

**Function:** `football-games-nfl::get_formated_data()` 

**Records:** `544 games`

### Returned Data Structure

| # | Column | Type | Description |
|----|--------|------|-------------|
| 1 | id | string | A generated unique identifier for each game |
| 2 | espn_id | string | ESPN-assigned game ID |
| 3 | date | string | Date and time of the game |
| 4 | season | int | Season year |
| 5 | week | int | Week number |
| 6 | title | string | Full title of the game |
| 7 | short_tile | string | Shortened title of the game |
| 8 | venue | string | Venue where the game is played |
| 9 | home_espn_id | string | ESPN ID for the home team |
| 10 | away_espn_id | string | ESPN ID for the away team |

| Sources |
|--------|
| https://site.api.espn.com/ |

