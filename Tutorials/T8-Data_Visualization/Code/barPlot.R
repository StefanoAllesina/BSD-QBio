library('ggplot2') #load ggplot
library('reshape2') #load reshape2 for reshaping data.frame objects

#You can set the directory to the directory you want. I used setwd() to move to the appropriate directory, but it will be different for you. You can provide your own directory, or navigate using the GUI.
setwd('../Data') 

# load the Region data from the working directory
df <- read.csv('UN population forecasts from Economist charts - Region.csv')

# melt reshapes a data.frame from multiple columns to three columns: an ID column, in this case Region, a variable column, which contains the column headers for each value, and a value Column for the populations. Have a look at the output to understand what it does.
DF <- melt(df[,c('Region','X2100','X2050','X2015')],id.vars = 'Region',variable.name='Year',value.name='Population')

#Change population to millions, because I want to
DF$Population = DF$Population/1000

# Make a ggplot object with the new data.frame, using Region and population as x and y
plotObj <- ggplot(DF,aes(x=Region,y=Population))

# Make a barplot object. Fill assigns the colours to different Regions, and alpha assigns transparency based on Year value data.
plotObj <- plotObj + geom_bar(aes(fill=Region,alpha=Year),position='dodge',stat='identity',colour='black')
# I included plotObj command to update the figure as we go along.
plotObj

# Flip x and y axis, because I like it better
plotObj <- plotObj + coord_flip()
plotObj

# Change the axis label to match reality
plotObj <- plotObj + ylab('Population (in millions)')
plotObj

# add a title
plotObj <- plotObj + ggtitle('Population Forecast by Region')
plotObj

# get rid of the gray background, it's ugly
plotObj <- plotObj + theme_bw()
plotObj

# Set the text properties you desire
plotObj <- plotObj + theme(axis.title = element_text(size=14,face='bold'),legend.text = element_text(size=10,face='bold'),legend.title = element_text(size=12,face='bold'),plot.title = element_text(size=16,face='bold'),axis.text = element_text(size=10))
plotObj

# Change the year names to drop the x's, and set the range of transparencies alpha
plotObj <- plotObj + scale_alpha_discrete(name='Year',labels=c('2100','2050','2015'),range=(c(.3,1)))
plotObj


