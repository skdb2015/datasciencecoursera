#
#   This script builds the plot that answers the following question:   
#
#   2. Have total emissions from PM2.5 decreased in the Baltimore City, 
#   Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
#   to make a plot answering this question.
#
library(reshape2)

doPlot2 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
    # filter the data to just contain Baltimore data 
    NEI_Baltimore = NEI[NEI$fips == "24510", ]
    
    # reshape the data so that its ID is based on the year
    NEImelt = melt(NEI_Baltimore, id = "year", measure.vars="Emissions")
    
    # cast the data based on year using totals
    NEImelt_total_by_year = dcast(NEImelt, year ~ variable, sum)
    
    # Plot the results
    png("plot2.png")
    with(NEImelt_total_by_year, {
        plot(year, Emissions, type = "n", 
             xlab="year", ylab="total emissions in Baltimore")
        lines(year, Emissions)
    })
    dev.off()
    
}