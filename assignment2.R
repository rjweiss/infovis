#Author: Rebecca Weiss
#CS448b Assignment2

rm(list=ls(all=T))
gc()

library(ggplot2)
library(gdata)
library(plyr)
library(maps)
library(grid)
library(gridExtra)

setwd("/Users/Rebecca/Dropbox/cs448b/assignment2")

##################################
#READING IN DATA AND PREPROCESSING
##################################

#Voter information
#from the Current Population Survey (Dept of Labor)
voters <- read.csv("Table 04a.csv")
names(voters) <- c(
	"state",
	"totalpop",
	"totalcitpop",
	"totalreg",
	"percregtot",
	"moe",
	"percregcit",
	"moe",
	"totalvote",
	"percvottot",
	"moe",
	"percvotcit",
	"moe"
	)
voters <- subset(voters, select = c(
	"state",
	"percregcit",
	"percvotcit")
	)
voters$state <- tolower(voters$state)
voters <- voters[c(-1, -3, -10, -13), ]
row.names(voters) = NULL

#state latitude and longitude information
#from the maps package
state_df <- map_data("state")
state_df$subregion = NULL
names(state_df) <- c(
	"long",
	"lat",
	"group",
	"order",
	"state"
)
state_df <- state_df[-which(state_df$state == c("district of columbia")),]

#population totals for each state
#from the 2008 US census
state_pop <- read.csv("state_population.csv", )
state_pop$state <- tolower(as.character(state_pop$state))
state_pop <- state_pop[c(-2, -9, -12),]
row.names(state_pop) = NULL
state_pop$pop <- as.numeric(gsub(",", "", state_pop$pop))

#contributions to obama's campaign 2008
#from the federal election commission
obama_in <- read.csv("obama_contributions.txt")
obama_in <- subset(obama_in, select = c(contbr_st, contb_receipt_amt))
obama_in <- obama_in[obama_in$contbr_st %in% state.abb, ,drop = TRUE]
obama_in$contbr_st <- factor(obama_in$contbr_st)
obama_in <- as.data.frame(table(obama_in$contbr_st))
names(obama_in) = c("state", "total_receipts")
obama_in$state <- tolower(state.name[match(obama_in$state, state.abb)])
obama_in <- obama_in[c(-1, -11),]
row.names(obama_in) = NULL

#receipts of obama's campaign 2008
#from the federal election commission
obama_out <- read.csv("obama_expenses.txt")
obama_out <- subset(obama_out, select = c(recipient_st, disb_amt))
obama_out <- obama_out[obama_out$recipient_st %in% state.abb, ]
obama_out$recipient_st <- factor(obama_out$recipient_st)
obama_out <- as.data.frame(table(obama_out$recipient_st))
names(obama_out) = c("state", "total_bills")
obama_out$state <- tolower(state.name[match(obama_out$state, state.abb)])
obama_out <- obama_out[c(-1, -11),]
row.names(obama_out) = NULL

#merging population and campaign finance information
finance <- merge(state_pop, obama_in, c("state"))
finance <- merge(finance, obama_out, c("state"))
finance$receipts_norm <- finance$total_receipts / finance$pop
finance$bills_norm <- finance$total_bills / finance$pop

#########
#PLOTTING
#########

none <- theme_blank()

#creating a data frame with all variables for plotting
data <- merge(finance, voters, by = c("state"))

#normalizing vectors to have 0 mean and unit variance
#allows for comparisons between variables that have different distributions
data$percvotcit <- scale(data$percvotcit, center = TRUE)
data$percregcit <- scale(data$percregcit, center = TRUE)
data$receipts_norm <- scale(data$receipts_norm, center = TRUE)
data$bills_norm <- scale(data$bills_norm, center = TRUE)

#adapted from hadley wickham's choropleth example here: https://gist.github.com/233134

choropleth <- merge(state_df, data, by = c("state"))

percvotcit <- ggplot(choropleth, 
	aes(long, lat, group = group)) +
	geom_polygon(aes(fill = percvotcit), colour = alpha("white", 1/2), size = 0.2) +
	geom_polygon(data = state_df, colour = "black", size = 0.25, fill = NA) +
	scale_fill_gradient(low = "white", high = "blue") +
	xlab(NULL) + ylab(NULL)  + 
	opts(
		title = "...citizens vote?",
		panel.grid.minor = theme_line(colour = NA),
		panel.grid.major = theme_line(colour = NA),
		panel.background = theme_rect(colour = NA),
		panel.border = none,
     		axis.text.x = none,
     		axis.text.y = none, 
     		axis.ticks = none,
     		panel.border = none,
     		legend.position = 'none')

percregcit <- ggplot(choropleth, 
	aes(long, lat, group = group)) +
	geom_polygon(aes(fill = percregcit), colour = alpha("white", 1/2), size = 0.2) +
	geom_polygon(data = state_df, colour = "black", size = 0.25, fill = NA) +
	scale_fill_gradient(low = "white", high = "purple") +
	xlab(NULL) + ylab(NULL)  + 
	opts(
		title = "...citizens registered to vote?",
		panel.grid.minor = theme_line(colour = NA),
		panel.grid.major = theme_line(colour = NA),
		panel.background = theme_rect(colour = NA),
		panel.border = none,
     		axis.text.x = none,
     		axis.text.y = none, 
     		axis.ticks = none,
     		panel.border = none,
     		legend.position = 'none')

receipts_norm <- ggplot(choropleth, 
	aes(long, lat, group = group)) +
	geom_polygon(aes(fill = receipts_norm), colour = alpha("white", 1/2), size = 0.2) +
	geom_polygon(data = state_df, colour = "black", size = 0.25, fill = NA) +
	scale_fill_gradient(low = "white", high = "dark green") +
	xlab(NULL) + ylab(NULL)  + 
	opts(
		title = "...individual contributions to \n the Obama campaign?",
		panel.grid.minor = theme_line(colour = NA),
		panel.grid.major = theme_line(colour = NA),
		panel.background = theme_rect(colour = NA),
		panel.border = none,
     		axis.text.x = none,
     		axis.text.y = none, 
     		axis.ticks = none,
     		panel.border = none,
     		legend.position = 'none')

bills_norm <- ggplot(choropleth, 
	aes(long, lat, group = group)) +
	geom_polygon(aes(fill = bills_norm), colour = alpha("white", 1/2), size = 0.2) +
	geom_polygon(data = state_df, colour = "black", size = 0.1, fill = NA) +
	scale_fill_gradient(low = "white", high = "red") +
	xlab(NULL) + ylab(NULL)  + 
	opts(
		title = "...number of bills from \n the Obama campaign?",
		panel.grid.minor = theme_line(colour = NA),
		panel.grid.major = theme_line(colour = NA),
		panel.background = theme_rect(colour = NA),
		panel.border = none,
     		axis.text.x = none,
     		axis.text.y = none, 
     		axis.ticks = none,
     		panel.border = none,
     		legend.position = 'none')

#pdf("assignment2.pdf", width = 10)
png("assignment2.png", width = 650)

grid.arrange(
	percregcit,
	percvotcit,
	receipts_norm,
	bills_norm,
	main = textGrob("In 2008, which states had the most...", gp=gpar(fontsize = 30))
)
dev.off()
#dev.off()
