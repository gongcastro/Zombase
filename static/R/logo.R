#### logo: create Zombdata logo#############
# Gonzalo García-Castro, zombdata@gmail.com

#### set up #################################

# load packages
library(ggplot2)
library(dplyr)
library(showtext)
library(hexSticker)
library(here)

# load fonts
font_add_google("Bangers")
font_add_google("Fira Sans Condensed")

# for reproducibility
set.seed(888)

#### generate data ##########################
data <- data.frame(
	x = rnorm(n = 10000, sd = 9, mean = 0),
	y = rnorm(n = 10000, sd = 9, mean = 0)
) 
#### create graph ###########################
plot <- ggplot(data, aes(x, y)) +
	stat_bin_2d(binwidth = 1, colour = "black", size = 0.01) +
	scale_fill_gradientn(colors = c("black", "red")) +
	coord_polar() +
	theme(
		panel.background = element_rect(fill = "transparent"),
		plot.background = element_rect(fill = "transparent"),
		panel.border = element_blank(),
		panel.grid = element_blank(),
		legend.position = "none",
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()
	)

#### create logo ############################
sticker(
	subplot = plot,
	package = "zombdata", 
	p_color = "white",
	h_size = 2,
	white_around_sticker = FALSE,
	p_family = "bangers",
	p_size = 10,
	p_x = 1,
	p_y = 1.3,
	s_x = 1,
	s_y = 1,
	s_width = 2.5,
	s_height = 2.5,
	h_fill = "black",
	h_color = "red",
	dpi = 200,
	filename = here("static", "images", "zombdata.png")
)

