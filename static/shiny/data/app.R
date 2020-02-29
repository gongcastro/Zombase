#### data: show data ##############################
# Gonzalo García-Castro, zombase.database@gmail.com

#### set up #######################################
# load packages
library(shiny)
library(DT)
library(dplyr)
library(googlesheets4)

# provide credentials
options(
	gargle_oauth_cache = ".secrets",
	gargle_oauth_email = "zombase.database@gmail.com"
)
sheets_auth(cache = ".secrets/", email = "zombase.database@gmail.com")

#### import data ##############################
data <- sheets_read("https://docs.google.com/spreadsheets/d/1p-DpOQABFoB-u9vmDr_-VxoGeJkGTY54eqDHf3-LPtQ/edit#gid=1036030952", sheet = "data")

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
