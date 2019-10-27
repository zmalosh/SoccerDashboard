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
			dateInput('gameDateInput', 'Game Date', format = 'yyyy-mm-dd')
		),


		# Show a plot of the generated distribution
		mainPanel(
			textOutput("dateOutput"),
			tableOutput('gamesOutput')
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	options(shiny.reactlog = TRUE,
			stringsAsFactors = FALSE)
	useDataCache <- TRUE

	source('requirements.R')
	source('src/data/get_leagues.R')
	source('src/data/get_fixtures.R')
	source('src/data/get_teams.R')

	output$dateOutput <- renderText(format(input$gameDateInput, '%Y-%m-%d'))

	dateGames <- reactive({
		gameDate <- format(input$gameDateInput, '%Y-%m-%d')
		dateGames <- get_fixtures_by_date(gameDate, useDataCache)
	})

	output$gamesOutput <- renderTable(dateGames())
}

# Run the application
shinyApp(ui = ui, server = server)

