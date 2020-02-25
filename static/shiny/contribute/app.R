# contribute to the database

#### set up ######################################

# load packages
library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)
library(dplyr)
library(maps)
library(rlang)
library(googlesheets4)
library(lubridate)
library(here)
library(stringr)
library(data.table)

# create functions

# load data
country_list <- unique(iso3166$ISOname) # country list
data <- fread(file = here("zombies.txt"),
			  sep = "\t",
			  dec = ".",
			  header = TRUE,
			  na.strings = "NA",
			  stringsAsFactors = FALSE,
			  verbose = FALSE)

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
								background-color:#c91812;
								color:white;
								border-color:white;
								font-weight:bold;
								padding:10px;
								font-size:150%;
								font:liga;
								border-radius:100%;
								border: 5px solid white
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
			filter(str_detect(string = Title, pattern = fixed(input$Title, ignore_case = TRUE)) |
				   	str_detect(string = Author, pattern = fixed(input$Author, ignore_case = TRUE))) %>%
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
			ContribEmail   = input$ContribEmail,
			ContribCountry = input$ContribCountry,
			Title          = input$Title,
			Date           = input$Date,
			Country        = input$Country,
			Author         = input$Author,
			Type           = input$Type,
			Budget         = input$Budget,
			Box            = input$Box,
			Duration       = input$Duration,
			Producer       = input$Producer,
			IMDBLink       = input$IMDBLink
		)
		googlesheets4::sheets_append(ss = "https://docs.google.com/spreadsheets/d/1fZZp9FSXt3IRqifMPbmnLaeVdKhCiXMNNweUtO9NCtk/edit#gid=0", data = contrib_data)
		shinyalert("Saved", "Thank you for you contribution!", type = "success")
		

	})

}

#### run app ####################################
shinyApp(ui = ui, server = server)
