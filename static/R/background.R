#### logo: create Zombdata logo#############
# Gonzalo García-Castro, zombdata@gmail.com

#### set up #################################

# load packages
library(ggplot2)
library(dplyr)
library(here)

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
	coord_polar(clip = "on") +
	theme(
		panel.background = element_rect(fill = "black"),
		plot.background = element_rect(fill = "black"),
		panel.border = element_blank(),
		panel.grid = element_blank(),
		legend.position = "none",
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()
	) +
	ggsave(here("static", "images", "background.png"))
