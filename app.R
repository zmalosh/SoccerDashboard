#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
	tabsetPanel(id='tabs',
		tabPanel('Games',
			titlePanel("Soccer Games"),

			sidebarLayout(
				sidebarPanel(
					dateInput('GameDateInput', 'Game Date', format = 'yyyy-mm-dd'),
					uiOutput('LeagueIdUI'),
					checkboxInput('MajorCompetitionsInput', 'Only Major Leagues', value = TRUE),
					checkboxInput('ConfederationsInput_UEFA', 'Include UEFA (Europe)', value = TRUE),
					checkboxInput('ConfederationsInput_CONCACAF', 'Include CONCACAF (N. America)', value = TRUE),
					checkboxInput('ConfederationsInput_CONMEBOL', 'Include CONMEBOL (S. America)', value = TRUE),
					checkboxInput('ConfederationsInput_CAF', 'Include CAF (Africa)', value = TRUE),
					checkboxInput('ConfederationsInput_AFC', 'Include AFC (Asia)', value = TRUE),
					checkboxInput('ConfederationsInput_OFC', 'Include OFC (Oceania)', value = TRUE),
					checkboxInput('OddsOnlyInput', 'Only Odds Leagues'),
					width = 3
				),

				mainPanel(
					textOutput('predictionImportStatusOutput'),
					DT::dataTableOutput('gamesOutput')
				)
			)
		),
		tabPanel(value = 'GameDetailsTab',
				 title = 'Game Details',
				 fluidRow(
				 	column(3,
				 		   htmlOutput('gdt_homeTeamName'),
				 		   htmlOutput('gdt_homeTeamLogo')),
				 	column(3,
				 		   htmlOutput('gdt_awayTeamName'),
				 		   htmlOutput('gdt_awayTeamLogo'))
				 ),
				 fluidRow(
				 	htmlOutput('gdt_round'),
				 	htmlOutput('gdt_venue'),
				 	htmlOutput('gdt_referee')
				 ),
				 fluidRow(
				 	column(6,
				 		   plotOutput('gdt_last_five_form_graph', height = '250px'))
				 ),
				 fluidRow(
				 	htmlOutput('gdt_advice'),
				 	HTML('<br />'),
				 	htmlOutput('gdt_pred_pcts')),

			 	tabsetPanel(id = 'GameDetailsDetailTabs',
			 		tabPanel(value='GameDetailsH2HTab',
			 				 title='H2H',
			 				 fluidRow(HTML('<h4>H2H</H4>')),
			 				 fluidRow(DT::dataTableOutput('gdt_h2h_games'))
			 		),
			 		tabPanel(value='GameDetailsOddsTab',
			 				 title='Odds',
			 				 fluidRow(HTML('<h4>Odds</h4>')),
			 				 fluidRow(
			 				 	htmlOutput('gdt_odds_winner_title'),
			 				 	DT::dataTableOutput('gdt_odds_winner')
			 				 ),
			 				 fluidRow(
			 				 	htmlOutput('gdt_odds_total_title'),
			 				 	DT::dataTableOutput('gdt_odds_total')
			 				 ),
			 				 fluidRow(
			 				 	htmlOutput('gdt_odds_spread_title'),
			 				 	DT::dataTableOutput('gdt_odds_spread')
			 				 )
			 		)
			 	)
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
	options(shiny.reactlog = TRUE,
			stringsAsFactors = FALSE)
	useDataCache <- TRUE
	tableLogoHeight <- 20
	notSelectedVal <- -1
	url_image_x <- 'https://p1.hiclipart.com/preview/805/253/78/cp39-for-object-dock-red-x-symbol-png-clipart.jpg'

	selectedDetailedFixtureId <- reactiveVal(NA)

	buttonInput <- function(FUN, baseId, ids, ...) {
		len <- length(ids)
		inputs <- character(len)
		for (i in seq_len(len)) {
			id <- ids[i]
			inputs[i] <- as.character(FUN(paste0(baseId, id), ...))
		}
		inputs
	}

	source('requirements.R')
	source('src/data/get_leagues.R')
	source('src/data/get_fixtures.R')
	source('src/data/get_teams.R')
	source('src/data/get_predictions.R')
	source('src/data/get_odds.R')

	output$dateOutput <- renderText(format(input$GameDateInput, '%Y-%m-%d'))

	leaguePriorities <- reactive({
		leaguePriorities <- readr::read_csv(file =  'data/raw/leaguePriorities.csv',
											col_types = cols(
												LeagueName = col_character(),
												Country = col_character(),
												IsMajor = col_logical(),
												Priority = col_integer(),
												Confederation = col_character()
											))
		return(leaguePriorities)
	})

	leagues <- reactive({
		print('START GET leagues')
		leaguePriorities <- leaguePriorities()
		if(is.null(leaguePriorities) || nrow(leaguePriorities) == 0){
			print('END GET leagues (return NULL)')
			return(NULL)
		}
		leagues <- get_leagues(useDataCache) %>%
			left_join(leaguePriorities, by = c('Country', 'LeagueName')) %>%
			filter(!input$MajorCompetitionsInput | IsMajor) %>%
			filter(Confederation != 'UEFA' | input$ConfederationsInput_UEFA) %>%
			filter(Confederation != 'CONMEBOL' | input$ConfederationsInput_CONMEBOL) %>%
			filter(Confederation != 'CONCACAF' | input$ConfederationsInput_CONCACAF) %>%
			filter(Confederation != 'AFC' | input$ConfederationsInput_AFC) %>%
			filter(Confederation != 'CAF' | input$ConfederationsInput_CAF) %>%
			filter(Confederation != 'OFC' | input$ConfederationsInput_OFC)
		print(paste0('END GET leagues (return ', nrow(leagues), ' rows)'))
		return(leagues)
	})

	gameDate <- reactive({
		print('START GET gameDate')
		if(is.null(input$GameDateInput)){
			print('START GET gameDate (return NULL)')
			return(NULL)
		}
		gameDate <- format(input$GameDateInput, '%Y-%m-%d')
		print(paste0('END GET gameDate (', gameDate, ')'))
		return(gameDate)
	})

	dateGames <- reactive({
		print('START GET dateGames')
		leagues <- leagues()
		if(is.null(leagues) || nrow(leagues) == 0){
			return(NULL)
		}
		gameDate <- gameDate()
		if(is.null(gameDate)){
			return(NULL)
		}
		dateGames <- get_fixtures_by_date(gameDate, useDataCache) %>%
			inner_join(leagues %>% select(LeagueId), by = 'LeagueId')
		print('END GET dateGames')
		return(dateGames)
	})

	leagueOptions <- reactive({
		dateGames <- dateGames()
		if(is.null(dateGames)){
			leagueOptions <- list('ALL LEAGUES GOOD' = notSelectedVal)
			return(leagueOptions)
		}
		gameDate <- gameDate()
		leagues <- leagues()
		if(is.null(leagues) || nrow(leagues) == 0 || is.null(dateGames) || nrow(dateGames) == 0){
			leagueOptions <- list('ALL LEAGUES GOOD' = notSelectedVal)
		}
		else{
			onlyOdds <- input$OddsOnlyInput
			x <- dateGames %>%
				inner_join(leagues, by = 'LeagueId') %>%
				filter(!onlyOdds | HasOdds) %>%
				transform(LeagueDisplay = paste0('<img src="', FlagUrl, '" height="', tableLogoHeight, '"></img>&nbsp;', Country, ' - ', LeagueName)) %>%
				transform(LeagueDisplay = paste(Country, '-', LeagueName)) %>%
				select(LeagueId, LeagueDisplay, LeagueName, Country) %>%
				unique() %>%
				arrange(Country, LeagueDisplay)
			leagueOptions <- c(notSelectedVal, as.list(x$LeagueId))
			leagueNames <- c('ALL LEAGUES', as.list(x$LeagueDisplay))
			setNames(leagueOptions, leagueNames)
		}
	})

	output$LeagueIdUI <- renderUI({
		selectInput('LeagueId', 'Leagues', leagueOptions())
	})

	selectedLeagueId <- reactive({
		if(is.null(input$LeagueId)){
			return(NULL)
		}
		selectedLeagueId <- input$LeagueId
	})

	predictions <- reactive({
		print('START GET predictions')
		dateGames <- dateGames()
		if(is.null(dateGames) || nrow(dateGames) == 0){
			print('END GET predictions (return NULL)')
			return(NULL)
		}
		fixtureIds <- dateGames %>% select(FixtureId)
		fixtureCount <- nrow(fixtureIds)
		preds <- NULL
		withProgress(
			message = 'Importing Predictions',
			detail = 'This may take a few minutes',
			value = 0,
			{
				for(i in 1:fixtureCount){
					fixtureId <- fixtureIds[i,]
					fixturePred <- get_predictions_by_fixture(fixtureId) %>%
						mutate(FixtureId = fixtureId,
							   HomePct = as.integer(str_replace(winning_percent$home, '%', '')),
							   DrawPct = as.integer(str_replace(winning_percent$draws, '%', '')),
							   AwayPct = as.integer(str_replace(winning_percent$away, '%', ''))) %>%
						select(FixtureId, HomePct, DrawPct, AwayPct)
					rownames(fixturePred) <- c(fixturePred$FixtureId)
					if(is.null(preds)){
						preds <- fixturePred
					}else{
						preds <- bind_rows(preds, fixturePred)
					}
					incProgress(1/fixtureCount)
				}
			}
		)
		print(paste0('END GET predictions (return ', nrow(preds), ' rows)'))
		return(preds)
	})

	dateGamesDisplay <- reactive({
		leagues <- leagues()
		dateGames <- dateGames()
		if(is.null(leagues) || nrow(leagues) == 0 || is.null(dateGames) || nrow(dateGames) == 0){
			return(NULL)
		}
		selectedLeagueId <- selectedLeagueId()
		noLeagueSelected <- is.null(selectedLeagueId) || selectedLeagueId == notSelectedVal
		onlyOdds <- input$OddsOnlyInput
		if(noLeagueSelected){
			x <- dateGames
		} else {
			x <- dateGames %>%
				filter(noLeagueSelected | selectedLeagueId == LeagueId)
		}
		x <- x %>%
			inner_join(leagues, by = 'LeagueId') %>%
			filter(!onlyOdds | HasOdds) %>%
			transform(FixtureId = FixtureId,
					  GameDate = GameDate,
					  FlagUrl = ifelse(is.null(FlagUrl), url_image_x, FlagUrl),
					  LogoUrl = ifelse(is.null(LogoUrl), url_image_x, LogoUrl),
					  LeagueDisplay = paste0('<img src="', FlagUrl, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', LeagueName),
					  HomeTeamDisplay = paste0('<img src="', HomeTeamLogo, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', HomeTeamName, '</span>'),
					  AwayTeamDisplay = paste0('<img src="', AwayTeamLogo, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', AwayTeamName, '</span>'),
					  GameTime = substr(GameDate, 12, 16),
					  HomePct = ' ',
					  DrawPct = ' ',
					  AwayPct = ' ')

		predictions <- predictions()
		usePredictions <- !is.null(predictions) && nrow(predictions) > 0

		if(usePredictions){
			x <- x %>%
				inner_join(predictions, by = 'FixtureId') %>%
				transform(HomePct = HomePct.y,
						  DrawPct = DrawPct.y,
						  AwayPct = AwayPct.y)
		}

		detailButtons <- buttonInput(FUN = actionButton,
									 baseId = 'button_',
									 ids = x$FixtureId,
									 label = 'DETAILS',
									 onclick = 'Shiny.onInputChange(\"detailsButton\",  this.id)')
		x$Action <- detailButtons
		x <- x %>%
			select(Action,
				   HomeTeam = HomeTeamDisplay,
				   AwayTeam = AwayTeamDisplay,
				   `H%` = HomePct,
				   `D%` = DrawPct,
				   `A%` = AwayPct,
				   League = LeagueDisplay,
				   `Start(EST)` = GameTime)

		return(x)
	})

	observeEvent(input$detailsButton, {
		id <- as.integer(str_replace(input$detailsButton, 'button_', ''))
		selectedDetailedFixtureId(id)
		updateTabsetPanel(session, 'tabs', selected = 'GameDetailsTab')
	})

	output$gamesOutput <- DT::renderDataTable(DT::datatable(dateGamesDisplay(), escape = FALSE, options = list(pageLength = 1000, lengthMenu = c(25, 50, 100, 250, 500, 1000))))

	####
	#### GAME DETAILS TAB
	####

	gameSummary <- reactive({
		fixtureId <- selectedDetailedFixtureId()
		if(is.null(fixtureId) || is.na(fixtureId)){
			return(NULL)
		}
		dateGames <- dateGames()
		if(is.null(dateGames) || nrow(dateGames) == 0){
			return(NULL)
		}
		gameSummary <- dateGames %>% filter(FixtureId == fixtureId)
		if(is.null(gameSummary) || nrow(gameSummary) != 1){
			return(NULL)
		}
		return(gameSummary)
	})

	gdt_homeTeamName <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary)){
			return(NULL)
		}
		return(paste(gameSummary$HomeTeamName, '(H)'))
	})

	gdt_homeTeamLogo <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary)){
			return(NULL)
		}
		return(gameSummary$HomeTeamLogo)
	})

	gdt_awayTeamName <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary)){
			return(NULL)
		}
		return(paste(gameSummary$AwayTeamName, '(A)'))
	})

	gdt_awayTeamLogo <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary)){
			return(NULL)
		}
		return(gameSummary$AwayTeamLogo)
	})

	gdt_referee <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary) || is.null(gameSummary$Referee) || is.na(gameSummary$Referee)){
			return(NULL)
		}
		return(paste('Referee:', gameSummary$Referee))
	})

	gdt_venue <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary) || is.null(gameSummary$Venue) || is.na(gameSummary$Venue)){
			return(NULL)
		}
		return(paste('Venue:', gameSummary$Venue))
	})

	gdt_round <- reactive({
		gameSummary <- gameSummary()
		if(is.null(gameSummary) || is.null(gameSummary$Round) || is.na(gameSummary$Round)){
			return(NULL)
		}
		return(paste('Round:', gameSummary$Round))
	})

	gdt_predictions <- reactive({
		fixtureId <- selectedDetailedFixtureId()
		if(is.null(fixtureId) || is.na(fixtureId)){
			return(NULL)
		}
		predictions <- get_predictions_by_fixture(fixtureId)
		return(predictions)
	})

	gdt_pred_advice <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		return(predictions$advice)
	})

	gdt_pred_score <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		homeScore <- round(as.numeric(str_replace(predictions$goals_home, '-', '')), digits = 1)
		awayScore <- round(as.numeric(str_replace(predictions$goals_away, '-', '')), digits = 1)
		return(paste0('Score: ', homeScore, '-', awayScore))
	})

	gdt_pred_pcts <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		if(is.null(predictions$winning_percent)){
			return(NULL)
		}
		odds <- gdt_odds_winner()
		if(!is.null(odds) && nrow(odds) > 0){
			x <- odds %>% summarise(HomeProb = mean(HomeProb),
									DrawProb = mean(DrawProb),
									AwayProb = mean(AwayProb))

			homePct <- round(x$HomeProb, digits = 1) %>% paste0(., '%')
			drawPct <- round(x$DrawProb, digits = 1) %>% paste0(., '%')
			awayPct <- round(x$AwayProb, digits = 1) %>% paste0(., '%')
		} else {
			homePct <- predictions$winning_percent$home
			drawPct <- predictions$winning_percent$draws
			awayPct <- predictions$winning_percent$away
		}
		return(paste0(homePct, '-', drawPct, '-', awayPct))
	})

	####
	#### GAME DETAILS TAB - FORM
	####

	gdt_form_home_team <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		if(is.null(predictions$teams) || is.null(predictions$teams$home))
		{
			return(NULL)
		}
		team <- predictions$teams$home
		return(team)
	})

	gdt_form_away_team <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		if(is.null(predictions$teams) || is.null(predictions$teams$away))
		{
			return(NULL)
		}
		team <- predictions$teams$away
		return(team)
	})

	gdt_form_last_five <- reactive({
		home <- gdt_form_home_team()
		if(is.null(home) || is.null(home$last_5_matches)){
			return(NULL)
		}
		away <- gdt_form_away_team()
		if(is.null(away) || is.null(away$last_5_matches)){
			return(NULL)
		}
		homeName <- home$team_name
		awayName <- away$team_name
		lastFive <- data.frame(
			Team = factor(c(homeName,awayName,homeName,awayName,homeName,awayName,homeName,awayName,homeName,awayName), c(homeName, awayName)),
			FormType = factor(c('Form','Form','Att','Att','Def','Def','AvgScored','AvgScored','AvgAllowed','AvgAllowed'), c('Form', 'Att', 'Def', 'AvgScored', 'AvgAllowed')),
			Value = c(str_replace(home$last_5_matches$forme, '%', '') %>% as.integer(.),
					  str_replace(away$last_5_matches$forme, '%', '') %>% as.integer(.),
					  str_replace(home$last_5_matches$att, '%', '') %>% as.integer(.),
					  str_replace(away$last_5_matches$att, '%', '') %>% as.integer(.),
					  str_replace(home$last_5_matches$def, '%', '') %>% as.integer(.),
					  str_replace(away$last_5_matches$def, '%', '') %>% as.integer(.),
					  home$last_5_matches$goals_avg,
					  away$last_5_matches$goals_avg,
					  home$last_5_matches$goals_against_avg,
					  away$last_5_matches$goals_against_avg)
		)
		return(lastFive)
	})

	output$gdt_last_five_form_graph <- renderPlot({
		lastFive <- gdt_form_last_five()
		if(is.null(lastFive)){
			return(NULL)
		}
		df <- lastFive %>% filter(as.character(FormType) %in% c('Att', 'Def', 'Form'))
		x <- ggplot(data=df, aes(x=FormType, y = Value, fill = Team)) +
			geom_bar(stat='identity', position=position_dodge()) +
			ylim(0, 100) +
			ylab('Form %') +
			xlab(NULL)
		x
	})

	####
	#### GAME DETAILS TAB - H2H
	####

	gdt_h2h <- reactive({
		predictions <- gdt_predictions()
		if(is.null(predictions)){
			return(NULL)
		}
		h2h <- predictions$h2h
		if(is.null(h2h)){
			return(NULL)
		}
		h2h <- x <- as.data.frame(h2h[1])
		if(nrow(h2h) == 0){
			return(NULL)
		}
		return(h2h)
	})

	gdt_h2h_games_display <- reactive({
		leagues <- leagues()
		if(is.null(leagues) || nrow(leagues) == 0){
			return(NULL)
		}
		h2h <- gdt_h2h()
		if(is.null(h2h) || nrow(h2h) == 0){
			return(NULL)
		}
		h2h <- h2h %>% inner_join(leagues, by = c('league_id' = 'LeagueId'))
		draw_image_url <- 'https://cdn.iconscout.com/icon/premium/png-256-thumb/pass-soccer-ball-1424555-1204788.png'
		display <- h2h %>%
			mutate(
				HomeLogo = homeTeam$logo,
				AwayLogo = awayTeam$logo,
				HomeName = homeTeam$team_name,
				AwayName = awayTeam$team_name,
				HomeTeam = paste0('<img src="', HomeLogo, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', HomeName, '</span>'),
				AwayTeam = paste0('<img src="', AwayLogo, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', AwayName, '</span>'),
				GameDate = substr(event_date, 1, 10),
				HomeScore = goalsHomeTeam,
				AwayScore = goalsAwayTeam,
				Status = statusShort,
				Competition = LeagueName,
				WinnerName = ifelse(HomeScore > AwayScore, HomeName, ifelse(HomeScore < AwayScore, AwayName, 'Draw')),
				WinnerLogo = ifelse(HomeScore > AwayScore, HomeLogo, ifelse(HomeScore < AwayScore, AwayLogo, draw_image_url)),
				Score = paste0(HomeScore, '-', AwayScore),
				Winner = paste0('<img src="', WinnerLogo, '" height="', tableLogoHeight, '"></img>&nbsp;<span>', WinnerName, '</span>'),
				Venue = venue,
				Referee = referee
			) %>%
			select(Competition, HomeTeam, AwayTeam, Score, Winner, Status, GameDate, Venue, Referee) %>%
			arrange(desc(GameDate))
		return(display)
	})

	output$gdt_h2h_games <- DT::renderDataTable(DT::datatable(gdt_h2h_games_display(), escape = FALSE, options = list(pageLength = 50, lengthMenu = c(10, 25, 50, 100, 150, 200))))

	####
	#### GAME DETAILS TAB - ODDS
	####

	gdt_odds <- reactive({
		fixtureId <- selectedDetailedFixtureId()
		if(is.null(fixtureId)){
			return(NULL)
		}
		odds <- get_odds_by_fixture(fixtureId)
		if(is.null(odds)){
			return(NULL)
		}
		if(nrow(odds) == 0){
			return(NULL)
		}
		return(odds)
	})

	output$gdt_homeTeamName <- renderText(paste0('<h2>', gdt_homeTeamName(), '</h2>'))
	output$gdt_awayTeamName <- renderText(paste0('<h2>', gdt_awayTeamName(), '</h2>'))
	output$gdt_homeTeamLogo <- renderText(paste0('<img style="height:80px;" src="', gdt_homeTeamLogo(), '"></img>'))
	output$gdt_awayTeamLogo <- renderText(paste0('<img style="height:80px;" src="', gdt_awayTeamLogo(), '"></img>'))
	output$gdt_advice <- renderText(paste0('Prediction: ', gdt_pred_advice(), '<br />', gdt_pred_score()))
	output$gdt_pred_pcts <-  renderText(paste0('Home-Draw-Away<br/>', gdt_pred_pcts()))
	output$gdt_round <- renderText(gdt_round())
	output$gdt_venue <- renderText(gdt_venue())
	output$gdt_referee <- renderText(gdt_referee())

	gdt_odds_winner <- reactive({
		allOdds <- gdt_odds()
		if(is.null(allOdds)){
			return(NULL)
		}
		if(nrow(allOdds) == 0){
			return(NULL)
		}
		odds <- allOdds %>% filter(BetTypeId == 1)
		if(nrow(odds) == 0){
			return(NULL)
		}
		result <- odds %>%
			group_by(BookmakerId, BookmakerName, MarketName) %>%
			summarise(Line = min(MarketLine)) %>%
			pivot_wider(names_from = MarketName, values_from = Line) %>%
			mutate(Home = as.numeric(Home),
				   Draw = as.numeric(Draw),
				   Away = as.numeric(Away),
				   HomeProb = round(100 * (1/Home) / ((1/Away)+(1/Draw)+1/Home), digits = 1),
				   DrawProb = round(100 * (1/Draw) / ((1/Away)+(1/Draw)+1/Home), digits = 1),
				   AwayProb = round(100 * (1/Away) / ((1/Away)+(1/Draw)+1/Home), digits = 1)) %>%
			ungroup() %>%
			select(BookmakerName, Home, Draw, Away, HomeProb, DrawProb, AwayProb) %>%
			arrange(BookmakerName)
		return(result)
	})

	gdt_odds_winner_title <- reactive({
		odds <- gdt_odds_winner()
		if(is.null(odds)){
			return(NULL)
		}
		return('<h4>Match Winner</h4>')
	})

	gdt_odds_total <- reactive({
		allOdds <- gdt_odds()
		if(is.null(allOdds)){
			return(NULL)
		}
		if(nrow(allOdds) == 0){
			return(NULL)
		}
		odds <- allOdds %>% filter(BetTypeId == 5)
		if(nrow(odds) == 0){
			return(NULL)
		}
		result <- odds %>%
			mutate(Value = str_replace(MarketName, 'Over ', '') %>% str_replace(., 'Under ', '') %>% as.numeric(),
				   Choice = ifelse(str_detect(MarketName, 'Over'), 'Over', 'Under')) %>%
			group_by(BookmakerId, BookmakerName, Value, Choice) %>%
			summarise(Line = min(MarketLine)) %>%
			pivot_wider(names_from = Choice, values_from = Line) %>%
			mutate(Over = as.numeric(Over),
				   Under = as.numeric(Under),
				   OverProb = round(100 * (1/Over) / ((1/Over)+(1/Under)), digits = 1),
				   UnderProb = round(100 * (1/Under) / ((1/Over)+(1/Under)), digits = 1)) %>%
			ungroup() %>%
			select(BookmakerName, Value, Over, Under, OverProb, UnderProb) %>%
			arrange(BookmakerName)
		return(result)
	})

	gdt_odds_total_title <- reactive({
		odds <- gdt_odds_total()
		if(is.null(odds)){
			return(NULL)
		}
		return('<h4>Total Goals</h4>')
	})

	gdt_odds_spread <- reactive({
		allOdds <- gdt_odds()
		if(is.null(allOdds)){
			return(NULL)
		}
		if(nrow(allOdds) == 0){
			return(NULL)
		}
		odds <- allOdds %>% filter(BetTypeId == 9)
		if(nrow(odds) == 0){
			return(NULL)
		}
		result <- odds %>%
			mutate(Value = str_replace(MarketName, 'Home ', '') %>% str_replace(., 'Away ', '') %>% str_replace(., 'Draw ', '') %>% as.numeric(),
				   Choice = ifelse(str_detect(MarketName, 'Home'), 'Home', ifelse(str_detect(MarketName, 'Away'), 'Away', 'Draw'))) %>%
			group_by(BookmakerId, BookmakerName, Value, Choice) %>%
			summarise(Line = min(MarketLine)) %>%
			pivot_wider(names_from = Choice, values_from = Line) %>%
			mutate(Home = as.numeric(Home),
				   Draw = as.numeric(Draw),
				   Away = as.numeric(Away),
				   HomeProb = round(100 * (1/Home) / ((1/Away)+(1/Draw)+1/Home), digits = 1),
				   DrawProb = round(100 * (1/Draw) / ((1/Away)+(1/Draw)+1/Home), digits = 1),
				   AwayProb = round(100 * (1/Away) / ((1/Away)+(1/Draw)+1/Home), digits = 1)) %>%
			ungroup() %>%
			select(BookmakerName, Value, Home, Draw, Away, HomeProb, DrawProb, AwayProb) %>%
			arrange(BookmakerName)
		return(result)
	})

	gdt_odds_spread_title <- reactive({
		odds <- gdt_odds_spread()
		if(is.null(odds)){
			return(NULL)
		}
		return('<h4>Spread</h4>')
	})

	output$gdt_odds_winner <- DT::renderDataTable(DT::datatable(gdt_odds_winner(), escape = FALSE, options = list(pageLength = 1000, lengthMenu = c(25, 50, 100, 250, 500, 1000))))
	output$gdt_odds_winner_title <- renderText(gdt_odds_winner_title())
	output$gdt_odds_total <- DT::renderDataTable(DT::datatable(gdt_odds_total(), escape = FALSE, options = list(pageLength = 1000, lengthMenu = c(25, 50, 100, 250, 500, 1000))))
	output$gdt_odds_total_title <- renderText(gdt_odds_total_title())
	output$gdt_odds_spread <- DT::renderDataTable(DT::datatable(gdt_odds_spread(), escape = FALSE, options = list(pageLength = 1000, lengthMenu = c(25, 50, 100, 250, 500, 1000))))
	output$gdt_odds_spread_title <- renderText(gdt_odds_spread_title())

}

# Run the application
shinyApp(ui = ui, server = server)

