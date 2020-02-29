#### data: # contribute to the database ############
# Gonzalo García-Castro, zombase.database@gmail.com

#### set up ######################################

# load packages
library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)
library(dplyr)
library(maps)
library(googledrive)
library(rlang)
library(googlesheets4)
library(lubridate)
library(here)
library(stringr)
library(data.table)

# provide credentials
options(
	gargle_oauth_cache = ".secrets",
	gargle_oauth_email = "zombase.database@gmail.com"
)
sheets_auth(cache = ".secrets/",
			email = "zombase.database@gmail.com",
			scopes = "https://www.googleapis.com/auth/spreadsheets")

#### import data #################################
country_list <- sort(unique(iso3166$ISOname)) # alphabetical country list
data <- sheets_read("1p-DpOQABFoB-u9vmDr_-VxoGeJkGTY54eqDHf3-LPtQ", sheet = "data")

#### user interface ##############################
ui <- fluidPage(title = "Contribution",
				
				titlePanel(title = "Contribution", windowTitle = "Contribution"),
				
				fluidRow(
					
					column(width = 3,
						   wellPanel(
						   	textInput(inputId = "ContribEmail",
						   			  label = "Your email",
						   			  value = NA_integer_,
						   			  placeholder = "your@email.com"),
						   	selectInput(inputId = "ContribCountry",
						   				label = "Your country",
						   				choices = country_list,
						   				selectize = TRUE,
						   				multiple = FALSE),
						   ), 
					),
					
					column(width = 3,
						   textInput(inputId = "Title",
						   		  label = "Title",
						   		  value = NA_integer_),
						   dateInput(inputId = "Date",
						   		  label = "Date",
						   		  format = "dd-mm-yyyy",
						   		  startview = "year",
						   		  autoclose = TRUE,
						   		  weekstart = 1,
						   		  language = "en",
						   		  value = NA
						   ),
						   selectInput(inputId = "Country",
						   			label = "Country",
						   			choices = country_list,
						   			selectize = TRUE,
						   			multiple = FALSE),
						   textInput(inputId = "Author",
						   		  label = "Author(s)",
						   		  value = NA_integer_),
						   helpText("Please provide author names separated by a forward slash (/)"),
						   textInput(inputId = "IMDBLink",
						   		  label = "IMDB link",
						   		  value = "")
					),
					column(width = 3,
						   radioButtons(inputId = "Type",
						   			 label = "Type",
						   			 choices = c("Film", "Series", "Novel", "Academic", "Documentary", "Video game"),
						   			 inline = TRUE),
						   numericInput(inputId = "Budget",
						   			 label = "Budget ($)",
						   			 value = NA_real_,
						   			 step = 0.01,
						   			 min = 0),
						   numericInput(inputId = "Box",
						   			 label = "Box ($)",
						   			 step = 0.01,
						   			 min = 0,
						   			 value = NA_real_),
						   numericInput(inputId = "Duration",
						   			 label = "Duration (min.)",
						   			 value = NA_integer_,
						   			 min = 0),
						   textInput(inputId = "Producer",
						   		  label = "Producer",
						   		  value = "")
					),
					tags$head(
						tags$style(
							HTML("
							.btn {
								font-weight:bold;
								padding:10px;
								font-size:150%;
								border: 5px solid black
							}
								 "))
					),
					useShinyalert(),
					column(3,
						actionButton("send", "Send contribution"),
					),
					
					fluidRow(
						column(width = 12, offset = 0,
							   DT::dataTableOutput(outputId = "data")
						)
					)
				)
)
					

#### server ######################################
server <- function(input, output) {
	
	output$data <- DT::renderDataTable({
		data %>%
			filter(str_detect(string = Title, pattern = fixed(ifelse(is.null(input$ContribEmail), "", input$ContribEmail), ignore_case = TRUE)) |
				   	str_detect(string = Author, pattern = fixed(ifelse(is.null(input$Author), "", input$Author), ignore_case = TRUE))) %>%
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
	})
	
	observeEvent(input$send, {
		contrib_data <- data.frame(
			ContribDate    = now(),
			ContribEmail   = ifelse(is.null(input$ContribEmail), NA_integer_, input$ContribEmail),
			ContribCountry = ifelse(is.null(input$ContribCountry), NA_integer_, input$ContribCountry),
			Title          = ifelse(is.null(input$Title), NA_integer_, input$Title),
			Type           = ifelse(is.null(input$Type), NA_integer_, input$Type),
			Date           = ifelse(is.null(input$Date), NA_integer_, input$Date),
			Author         = ifelse(is.null(input$Author), NA_integer_, input$Author),
			Country        = ifelse(is.null(input$Country), NA_integer_, input$Country),
			Budget         = ifelse(is.null(input$Budget), NA_integer_, input$Budget),
			Box            = ifelse(is.null(input$Box), NA_integer_, input$Box),
			Duration       = ifelse(is.null(input$Duration), NA_integer_, input$Duration),
			Producer       = ifelse(is.null(input$Producer), NA_integer_, input$Producer),
			IMDBLink       = ifelse(is.null(input$IMDBLink), NA_integer_, input$IMDBLink)
		)
		sheets_append(ss = "https://docs.google.com/spreadsheets/d/1fZZp9FSXt3IRqifMPbmnLaeVdKhCiXMNNweUtO9NCtk/edit#gid=0", data = contrib_data)
		shinyalert("Saved", "Thank you for you contribution!", type = "success")
		

	})

}

#### run app ####################################
shinyApp(ui = ui, server = server)
