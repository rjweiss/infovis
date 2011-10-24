setwd("~/Documents/cs448b/")
library(ggplot2)

data <- read.csv("highway_fatalities.csv")
attach(data)

fig <- ggplot(data, aes(
	x = Year, 
	y = Fatalities,
	fill = Type, #using fill and color as one variable is redundant
	color = Type,
	label = Type)) +
	stat_smooth(size = 1, se = FALSE) +
	geom_text(data = data[data$Year == 2002,],  aes(y = Fatalities, label = Type, hjust = 0.85, vjust = -1.2, size = 1)) +
	facet_wrap(~ Vehicle, ncol = 3) +
	opts(
		title = "Fatalities by vehicle and passenger type (1994-2009) \n Data from the U.S. National Highway Traffic Safety Administration",
		plot.title = theme_text(face = "bold", size = 12),
		axis.text.x = theme_text(angle = -45),
		panel.grid.minor = theme_line(size = 0.1, colour = "#F0F0F0"),
		panel.grid.major = theme_line(colour = NA),
		panel.background = theme_rect(fill = "#FCFCFC"),
		panel.border = theme_rect(
                     colour = "#000000"),
		     legend.position = "none") +
        scale_colour_brewer(palette = "Dark2") 

#save a .png for web
ggsave("cs448b_rweiss.png", fig, dpi = 72)

#save a .pdf for print
ggsave("cs448b_rweiss.pdf", fig)