
## NFL Teams

 Retrieves NFL team data from ESPN's API and supplements it with additional  information scraped from the wiki team pages and official nfl webpages. The combined data  is processed into a structured dataframe and saved to a CSV file.

**Function:** `football-teams-nfl::get_formated_data()`
**Records:** 32 teams

### Returned Data Structure

| Column | Type | Description |
|--------|------|-------------|
| id | string | A generated unique identifier for each team |
| espn_id | int | id used be espn to identify team |
| type | string | Always set to NBA for team type |
| abv | string | Abreviation of team name (ex. DEN) |
| full_name | string | Full name of team (ex. Denver Nuggets) |
| short_name | string | Short name of team (ex. Nuggets) |
| primary | string | Primary color of team uniforms in Hex format |
| secondary | string | Secondary color of team uniforms in Hex format |
| logo | string | Link to logo image from ESPN |
| conference | string | Conference team is associated with (ex. Western) |
| division | string | Division team is associated with (ex. Northwest) |
| webiste | string | Website url for team |
| head_coach | string | Current head coach of team |
| offensive_coordinator | string | Current offensive coordinator of team |
| defensive_coordinator | string | Current defensive coordinator of team |
| general_manager | string | Current general manager of team |
| venue | string | Current venue where team plays |

| Sources |
|--------|
| https://site.api.espn.com/ |
| https://en.wikipedia.org/wiki/ |
| https://www.nfl.com/teams/ |


## College Football Teams

 Retrieves college football team data from ESPN's API and supplements  it with additional information scraped from NCAA and CollegeFootballlDB. The combined  data is processed into a structured dataframe and saved to a CSV file.

**Function:** `football-teams-college::get_formated_data()`
**Records:** 606 teams

### Returned Data Structure

| Column | Type | Description |
|--------|------|-------------|
| id | string | A generated unique identifier for each team |
| espn_id | int | id used be espn to identify team |
| ncaa_id | string | id used be ncaa to identify team |
| type | string | Always set to NCAAB for team type |
| abv | string | Abreviation of team name (ex. TOW) |
| full_name | string | Full name of team (ex. Towson Tigers) |
| short_name | string | Short name of team (ex. Tigers) |
| university | string | Uniersity team is located (ex. Towson) |
| division | string | Division team is associated with (ex. I) |
| conference | string | Conference team is associated with (ex. Big West) |
| primary | string | Primary color of team uniforms in Hex format |
| secondary | string | Secondary color of team uniforms in Hex format |
| logo | string | Link to logo image from ESPN |
| head_coach | string | Current head coach of team |
| offensive_coordinator | string | Current offensive coordinator of team |
| defensive_coordinator | string | Current defensive coordinator of team |
| school_url | string | NCAA url for team |
| website | string | Website url for teams school |
| venue | string | Current venue where team plays |

| Sources |
|--------|
| https://site.api.espn.com/ |
| https://api.collegefootballdata.com/teams |
| https://www.ncaa.com/stats/football/ |


## NBA Teams

 Retrieves NBA team data from ESPN's API and supplements it with additional  information scraped from the wiki team pages. The combined data is processed into a  structured dataframe and saved to a CSV file. 

**Function:** `basketball-teams-nba::get_formated_data()`
**Records:** 30 teams

### Returned Data Structure

| Column | Type | Description |
|--------|------|-------------|
| id | string | A generated unique identifier for each team |
| espn_id | int | id used be espn to identify team |
| type | string | Always set to NBA for team type |
| abv | string | Abreviation of team name (ex. DEN) |
| full_name | string | Full name of team (ex. Denver Nuggets) |
| short_name | string | Short name of team (ex. Nuggets) |
| primary | string | Primary color of team uniforms in Hex format |
| secondary | string | Secondary color of team uniforms in Hex format |
| logo | string | Link to logo image from ESPN |
| conference | string | Conference team is associated with (ex. Western) |
| division | string | Division team is associated with (ex. Northwest) |
| twitter | string | Twitter handle of team starting with '@' |
| webiste | string | Website url for team |
| head_coach | string | Current head coach of team |
| general_manager | string | Current general manager of team |
| venue | string | Current venue where team plays |

| Sources |
|--------|
| https://site.api.espn.com/ |
| https://en.wikipedia.org/wiki/ |

## College Basketball Teams

 Retrieves college basketball team data from ESPN's API and supplements  it with additional information scraped from NCAA and CollegeBasketballDB. The combined  data is processed into a structured dataframe and saved to a CSV file. 

**Function:** `basketball-teams-college::get_formated_data()`
**Records:** 355 teams

### Returned Data Structure

| Column | Type | Description |
|--------|------|-------------|
| id | int | A generated unique identifier for each team |
| espn_id | string | id used be espn to identify team |
| ncaa_id | string | id used be ncaa to identify team |
| type | string | Always set to NCAAB for team type |
| abv | string | Abreviation of team name (ex. TOW) |
| full_name | string | Full name of team (ex. Towson Tigers) |
| short_name | string | Short name of team (ex. Tigers) |
| university | string | University team is located at (ex. Towson) |
| division | string | Division team is associated with (ex. I) |
| conference | string | Conference team is associated with (ex. Big West) |
| primary | string | Primary color of team uniforms in Hex format |
| secondary | string | Secondary color of team uniforms in Hex format |
| logo | string | Link to logo image from ESPN |
| head_coach | string | Current head coach of team |
| school_url | string | NCAA url for team |
| website | string | Website url for teams school |
| twitter | string | Twitter handle of team starting with '@' |
| venue | string | Current venue where team plays |

| Sources |
|--------|
| https://site.api.espn.com/ |
| https://api.collegebasketballdata.com/teams |
| https://www.ncaa.com/stats/basketball-men/ |


## MLB Teams

 Retrieves MLB team data from ESPN's API and supplements it with additional  information scraped from the wiki team pages. The combined data is processed into a structured  dataframe and saved to a CSV file. 

**Function:** `baseball-teams-mlb::get_formated_data()`
**Records:** 30 teams

### Returned Data Structure

| Column | Type | Description |
|--------|------|-------------|
| id | string | A generated unique identifier for each team |
| espn_id | int | id used be espn to identify team |
| type | string | Always set to NBA for team type |
| abv | string | Abreviation of team name (ex. DEN) |
| full_name | string | Full name of team (ex. Denver Nuggets) |
| short_name | string | Short name of team (ex. Nuggets) |
| primary | string | Primary color of team uniforms in Hex format |
| secondary | string | Secondary color of team uniforms in Hex format |
| logo | string | Link to logo image from ESPN |
| league | string | League team is associated with (ex. National League) |
| division | string | Division team is associated with (ex. Central Division) |
| twitter | string | Twitter handle of team starting with '@' |
| webiste | string | Website url for team |
| general_manager | string | Current general manager of team |
| manager | string | Current manager of team |
| venue | string | Current venue where team plays |

| Sources |
|--------|
| https://site.api.espn.com/ |
| https://en.wikipedia.org/wiki/ |

