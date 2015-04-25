#
#   This script builds the plot that answers the following question:   
#
#   1. Have total emissions from PM2.5 decreased in the United States from 1999 
#   to 2008? Using the base plotting system, make a plot showing the total PM2.5 
#   emission from all sources for each of the years 1999, 2002, 2005, and 2008.
#
library(reshape2)

doPlot1 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
   # reshape the data so that its ID is based on the year
   NEImelt = melt(NEI, id = "year", measure.vars="Emissions")
   
   # cast the data based on year using totals
   NEImelt_total_by_year = dcast(NEImelt, year ~ variable, sum)
   
   # Plot the results
   png("plot1.png")
   with(NEImelt_total_by_year, {
       plot(year, Emissions, type = "n", xlab="year", ylab="total emissions")
       lines(year, Emissions)
   })
   dev.off()
   
}