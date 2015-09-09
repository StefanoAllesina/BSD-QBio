library('ggplot2')#load ggplot

#You can set the directory to the directory you want. I used setwd() to move to the appropriate directory, but it will be different for you. You can provide your own directory, or navigate using the GUI.
setwd('../Data')

# load the Country data from the working directory
popData <- read.csv('UN population forecasts from Economist charts - Country.csv')

# Make a ggplot object using the population compared to the percent change in population from 2015 to 2050
plotObj <- ggplot(popData,aes(x=X2015*1000,y = (popData$X2050-popData$X2015)/popData$X2015,colour = Region))+geom_point(size=3)
plotObj

# Change x axis to a log scale, so you can see what's going on
plotObj <- plotObj + scale_x_log10(limits = c(1e2,1e10))
plotObj

# Get rid of ugly gray background
plotObj<- plotObj + theme_bw()
plotObj

# Add axis labels and title
plotObj <- plotObj + xlab('Population (2015)') + ylab('Percent Change in Population (2015 to 2050)') + ggtitle('Population Growth vs. Population Size')
plotObj

# Make fonts to your liking
plotObj <- plotObj + theme(axis.title = element_text(size=14,face='bold'),legend.text = element_text(size=10,face='bold'),legend.title = element_text(size=12,face='bold'),plot.title = element_text(size=16,face='bold'))
plotObj