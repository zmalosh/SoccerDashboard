source('requirements.R')
source('src/data/get_api_football_json_from_url.R')

get_predictions_by_fixture <- function(fixtureId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/predictions/', fixtureId)
	localPath <- paste0(getwd(), '/data/raw/predictions/predictions_', str_pad(fixtureId, 7, pad = '0'), '.rds')
	cacheExpirationMin <- 240

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
	rownames(pred) <- NULL
	pred$h2h <- as.data.frame(pred$h2h)
	if(allowCache){
		write_rds(pred, localPath)
	}
	return(pred)
}
