#### data: show data ##############################
# Gonzalo García-Castro, zombdata@gmail.com

#### set up #######################################
# load packages
library(shiny)
library(DT)
library(dplyr)
library(googlesheets4)

# provide credentials
options(
	gargle_oauth_cache = ".secrets",
	gargle_oauth_email = "zombdata@gmail.com"
)

sheets_auth(
	token = readRDS("google_token.rds"),
	cache = ".secrets/",
	email = "zombdata@gmail.com",
	scopes = "https://www.googleapis.com/auth/spreadsheets"
)

#### import data ##############################
data <- sheets_read("1NScfQetZxxVcX5hlrTU-37yd-Pdm86As0f6q78884Z8", sheet = "Data")

#### define user interface
ui <- fluidPage(DT::dataTableOutput("table"))

#### devine server logic
server <- function(input, output) {
	
	output$table <- DT::renderDataTable(
		data %>%
			datatable(rownames = FALSE, width = "2000px", height = "4000px", style = "bootstrap") %>%
			formatStyle(columns = "Title",
						fontWeight = "bold", backgroundColor = "white") %>%
			formatStyle(columns = c("Type", "Year", "Author", "Country", "Budget", "Box", "Duration", "Producer"),
						background = "transparent", 
						backgroundColor = "transparent",
						color = "black",
						`text-align` = "center") %>%
			formatString(c("Duration"), suffix = " min.") %>%
			formatCurrency(columns = c("Budget", "Box"), currency = "$", mark = ",", digits = 2, before = TRUE) %>%
			formatStyle(columns = "Type", background = "transparent") 
	)
}

#### run application
shinyApp(ui = ui, server = server)
