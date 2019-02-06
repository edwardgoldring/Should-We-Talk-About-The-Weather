################################################################################
### R Script to produce the figures for the Issue Attention project with Ed Goldring and Brandon Beomseob Park, Party Politics
###
###
################################################################################

rm(list=ls())

require(ggplot2)
require(grid)
library(foreign)

# Set up working directory
#setwd("")

###############################################################################################
## Figure 1: Parties' Economic Emphasis and the Public's Concern for the Economy
###############################################################################################
ts <- read.dta("Emphasis.dta", convert.underscore=TRUE)
gdata <- subset(ts, party == "1 Greens")
spddata <- subset(ts, party == "2 SPD")
fdpdata <- subset(ts, party == "3 FDP")
cdudata <- subset(ts, party == "4 CDU/CSU")

### Change the labels
labels <- c("1 Greens" = "Greens", "2 SPD" = "SPD", "3 FDP" = "FDP", "4 CDU/CSU" = "CDU/CSU")

### Facet by parties
all <- ggplot(dat = ts, aes(x = ts, y = perc)) + geom_line()
all <- all + facet_grid(party ~ ., labeller = labeller(party = labels))
all <- all + ylab("Economic Emphasis (%)") + xlab("")
all <- all + scale_x_continuous(breaks = c(504, 528, 552, 576, 600), labels = c("2002m1", "2004m1", "2006m1", "2008m1", "2010m1"))
all <- all + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom")  

### Salience of the Economy (MIP)
m <- ggplot(data = gdata, aes(x = ts, y = mip.econ)) + geom_line()
m <- m + ylab("MIP: Economy (%)") + xlab("")
m <- m + scale_x_continuous(breaks = c(504, 528, 552, 576, 600), labels = c("2002m1", "2004m1", "2006m1", "2008m1", "2010m1"))
m <- m + theme_bw()

# Combine the two plots onto one figure
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,1)))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(all, vp = vplayout(1:4, 1))
print(m, vp = vplayout(5, 1))


###############################################################################################
## Figure 2: Dynamic simulations of the long-term direct, spatial and total effects of a 1-standard deviation increase in issue attention (13%) at time t-1 on economic emphasis: SPD and Greens coalition
###############################################################################################
e = read.dta("Spatial-Long-Term Effects.dta", convert.underscore = TRUE)

e$effectname.f = factor(e$effectname, levels = c('direct', 'spatial', 'ctotal', 'total'), labels = c("Direct", "Indirect", "Cumulative Total", "Total"))

### Dynamic simulations
d <- ggplot(data = subset(e, effectname != "total" & order <= 4), aes(x = order, y = effect, color = partyname)) + geom_line(aes(linetype = partyname)) + geom_point() + facet_wrap(~ effectname.f)
d <- d + theme_minimal() + theme(legend.title = element_blank(), legend.position = "bottom")
d <- d + scale_x_continuous(breaks = c(0, 1, 2, 3, 4), labels = c("t", "t+1", "t+2", "t+3", "t+4"))
d <- d + xlab("Time") + ylab("Effect")
d

