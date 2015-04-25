#
#   This script builds the plot that answers the following question:   
#
#   3. Of the four types of sources indicated by the type (point, nonpoint, 
#   onroad, nonroad) variable, which of these four sources have seen decreases 
#   in emissions from 1999–2008 for Baltimore City? Which have seen increases 
#   in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
#   answer this question.
#
library(reshape2)
library(ggplot2)

doPlot3 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
    # filter the data to just contain Baltimore data 
    NEI_Baltimore = NEI[NEI$fips == "24510", ]
    
    # reshape the data so that its ID is based on the year and source type
    NEImelt = melt(NEI_Baltimore, id = c("year", "type"), 
                   measure.vars="Emissions")
    
    # cast the data based on year using totals
    NEImelt_total_by_year = dcast(NEImelt, year + type ~ variable, sum)
    
    # Plot the results
    png("plot3.png", width = 720)
    gplot = qplot(year, Emissions, data=NEImelt_total_by_year, facets=.~type,
                  geom=c("point","smooth"), method="lm")
   
    print(gplot)
    dev.off()
}