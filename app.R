#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

	# Application title
	titlePanel("Soccer Games"),

	# Sidebar with a slider input for number of bins
	sidebarLayout(
		sidebarPanel(
			dateInput('gameDateInput', 'Game Date', format = 'yyyy-mm-dd'),
			checkboxInput('oddsOnlyInput', 'Only Odds Leagues'),
			width = 2
		),


		# Show a plot of the generated distribution
		mainPanel(
			textOutput('predictionImportStatusOutput'),
			textOutput("dateOutput"),
			DT::dataTableOutput('gamesOutput')
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	options(shiny.reactlog = TRUE,
			stringsAsFactors = FALSE)
	useDataCache <- TRUE
	tableLogoHeight <- 20
	url_image_x <- 'https://p1.hiclipart.com/preview/805/253/78/cp39-for-object-dock-red-x-symbol-png-clipart.jpg'

	source('requirements.R')
	source('src/data/get_leagues.R')
	source('src/data/get_fixtures.R')
	source('src/data/get_teams.R')
	source('src/data/get_predictions.R')

	output$dateOutput <- renderText(format(input$gameDateInput, '%Y-%m-%d'))

	leagues <- reactive({
		leagues <- get_leagues(useDataCache)
	})

	dateGames <- reactive({
		gameDate <- format(input$gameDateInput, '%Y-%m-%d')
		dateGames <- get_fixtures_by_date(gameDate, useDataCache)
	})

	predictions <- reactive({
		dateGames <- dateGames()
		if(is.null(dateGames) || nrow(dateGames) == 0){
			return(NULL)
		}
		fixtureIds <- dateGames %>% select(FixtureId)
		fixtureCount <- nrow(fixtureIds)
		preds <- NULL
		withProgress(
			message = 'Importing Predictions',
			detail = 'This may take a few minutes if dust must be cleared',
			value = 0,
			{
				for(i in 1:fixtureCount){
					fixtureId <- fixtureIds[i,]
					fixturePred <- get_predictions_by_fixture(fixtureId) %>%
						mutate(FixtureId = fixture_id,
							   HomePct = as.integer(str_replace(winning_percent$home, '%', '')),
							   DrawPct = as.integer(str_replace(winning_percent$draws, '%', '')),
							   AwayPct = as.integer(str_replace(winning_percent$away, '%', ''))) %>%
						select(FixtureId, HomePct, DrawPct, AwayPct)
					if(is.null(preds)){
						preds <- fixturePred
					}else{
						preds <- rbind(preds, fixturePred)
					}
					incProgress(1/fixtureCount)
				}
			}
		)
		return(rawPreds)
	})

	dateGamesDisplay <- reactive({
		leagues <- leagues()
		dateGames <- dateGames()
		if(is.null(leagues) || nrow(leagues) == 0 || is.null(dateGames) || nrow(dateGames) == 0){
			return(NULL)
		}
		predictions <- predictions()
		usePredictions <- is.null(predictions) || nrow(predictions) == 0
		onlyOdds <- input$oddsOnlyInput
		x <- dateGames %>%
			inner_join(leagues, by = 'LeagueId') %>%
			filter(!onlyOdds | HasOdds) %>%
			transform(FixtureId = FixtureId,
					  GameDate = GameDate,
					  FlagUrl = ifelse(is.null(FlagUrl), url_image_x, FlagUrl),
					  LogoUrl = ifelse(is.null(LogoUrl), url_image_x, LogoUrl),
					  LeagueDisplay = paste0('<img src="', FlagUrl, '" height="', tableLogoHeight, '"></img>&nbsp;', LeagueName),
					  HomeTeamDisplay = paste0('<img src="', HomeTeamLogo, '" height="', tableLogoHeight, '"></img>&nbsp;', HomeTeamName),
					  AwayTeamDisplay = paste0('<img src="', AwayTeamLogo, '" height="', tableLogoHeight, '"></img>&nbsp;', AwayTeamName),
					  GameTime = substr(GameDate, str_locate(GameDate, 'T') + 1, length(GameDate)) %>% substr(., 1, 5),
					  HomePct = ' ',
					  DrawPct = ' ',
					  AwayPct = ' ')

		if(usePredictions){
			x <- x %>%
				inner_join(preds, by = 'FixtureId') %>%
				transform(HomePct = HomePct.y,
						  DrawPct = DrawPct.y,
						  AwayPct = AwayPct.y)
		}

		x <- x %>%
			select(HomeTeam = HomeTeamDisplay,
				   AwayTeam = AwayTeamDisplay,
				   `H%` = HomePct,
				   `D%` = DrawPct,
				   `A%` = AwayPct,
				   League = LeagueDisplay,
				   `Start(EST)` = GameTime)
		return(x)
	})

	output$gamesOutput <- DT::renderDataTable(DT::datatable(dateGamesDisplay(), escape = FALSE, options = list(pageLength = 1000, lengthMenu = c(25, 50, 100, 250, 500, 1000))))
}

# Run the application
shinyApp(ui = ui, server = server)

