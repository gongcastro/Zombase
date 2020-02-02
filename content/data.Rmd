---
title: "Zombase database"
date: "Updated: `r format(Sys.Date(), '%B %dth,  %y')`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}

# load packages
library(DT)
library(dplyr)
library(tidyr)
library(readxl)
library(here)
```

```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}

# show main table
data <- read_xlsx(here("static", "dat", "zombies.xlsx"))
datatable(data,
		  options = list(pageLength = 30),
		  rownames = FALSE,
		  autoHideNavigation = TRUE,
		  extensions = c("FixedColumns", "FixedHeader"),
		  filter = "top") %>%
	formatStyle(names(data), colour = "#ffffff", backgroundColor = "#0d0d0d") %>%
	formatStyle("Title", fontWeight = "bold") %>%
	formatCurrency(c("Budget", "Box"), currency = "$") %>% 
	formatString("Duration", suffix = " min.") 

#formatStyle("Title", color = "white", backgroundColor = "transparent", fontWeight = "bold") %>%
#formatStyle(c("Type", "Year", "Author", "Country", "Budget", "Box", "Duration", "Producer"),
 #           			color = "white", backgroundColor = "transparent")
```
