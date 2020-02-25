#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
library(shiny)
library(DT)
library(dplyr)
library(data.table)
# Define UI for application that draws a histogram
ui <- fluidPage(DT::dataTableOutput("table"))

# Define server logic required to draw a histogram
server <- function(input, output) {
	
	output$table <- DT::renderDataTable(
		fread(file = "zombies.txt",
			  sep = "\t",
			  dec = ".",
			  header = TRUE,
			  na.strings = "NA",
			  stringsAsFactors = FALSE,
			  verbose = FALSE) %>%
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

# Run the application 
shinyApp(ui = ui, server = server)
