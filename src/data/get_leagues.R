source('requirements.R')
source('src/data/get_api_football_json_from_url.R')

get_leagues <- function(allowCache = TRUE){
	localPath <- 'data/raw/leagues.csv'
	cacheExpirationMin <- 24 * 60

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath) && (file.info(localPath)$ctime + (cacheExpirationMin * 60)) > Sys.time()){
			cols <- cols(
				LeagueId = col_double(),
				LeagueName = col_character(),
				Country = col_character(),
				CountryCode = col_character(),
				Season = col_double(),
				SeasonStartDate = col_date(format = ""),
				SeasonEndDate = col_date(format = ""),
				LogoUrl = col_character(),
				FlagUrl = col_character(),
				IsCurrentSeason = col_logical(),
				HasStandings = col_logical(),
				HasFixtures = col_logical(),
				HasLineups = col_logical(),
				HasTeamStats = col_logical(),
				HasPlayerStats = col_logical(),
				HasPlayers = col_logical(),
				HasTopScorers = col_logical(),
				HasPredictions = col_logical(),
				HasOdds = col_logical()
			)
			leagues <- read_csv(localPath, col_types = cols)
			return (leagues)
		}
	}

	source('src/data/get_api_football_json_from_url.R')

	url <- 'https://api-football-v1.p.rapidapi.com/v2/leagues'

	json <- get_api_football_json_from_url(url)
	leagues <- json$leagues
	leagues <- leagues %>%
		transform(
			LeagueId = league_id,
			LeagueName = name,
			Country = country,
			CountryCode = country_code,
			Season = season,
			SeasonStartDate = season_start,
			SeasonEndDate = season_end,
			LogoUrl = logo,
			FlagUrl = flag,
			IsCurrentSeason = is_current == 1,
			HasStandings = coverage$standings,
			HasFixtures = coverage$fixtures$events,
			HasLineups = coverage$fixtures$lineups,
			HasTeamStats = coverage$fixtures$statistics,
			HasPlayerStats = coverage$fixtures$players_statistics,
			HasPlayers = coverage$players,
			HasTopScorers = coverage$topScorers,
			HasPredictions = coverage$predictions,
			HasOdds = coverage$odds
		) %>%
		select(LeagueId, LeagueName, Country, CountryCode,
			   Season, SeasonStartDate, SeasonEndDate,
			   LogoUrl, FlagUrl, HasStandings, IsCurrentSeason,
			   HasStandings, HasFixtures, HasLineups, HasTeamStats,
			   HasPlayerStats, HasPlayers, HasTopScorers,
			   HasPredictions, HasOdds)
	if(allowCache){
		write_csv(leagues, localPath)
	}
	return (leagues)
}
