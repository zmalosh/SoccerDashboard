source('requirements.R')
source('src/data/get_api_football_json_from_url.R')

get_predictions_by_fixture <- function(fixtureId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/predictions/', fixtureId)
	localPath <- paste0(getwd(), '/data/raw/predictions/pred_', str_pad(fixtureId, 7, pad = '0'), '.rds')
	cacheExpirationMin <- 10

	if(allowCache){
		dirExists <- dir.exists(dirname(localPath))
		if(!dirExists){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath) && (file.info(localPath)$ctime + (cacheExpirationMin * 60)) > Sys.time()){
			df <- read_rds(localPath)
			rownames(df) <- NULL
			return (df)
		}
	}
	json <- get_api_football_json_from_url(url)
	predictions <- json$predictions
	pred <- as.data.frame(predictions)
	pred$fixture_id <- fixtureId
	rownames(pred) <- NULL
	if(allowCache){
		write_rds(pred, localPath)
	}
	return(pred)
}
