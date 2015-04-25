#
#   This script builds the plot that answers the following question:   
#
#   4. Across the United States, how have emissions from coal combustion-
#   related sources changed from 1999–2008?
#
library(reshape2)

doPlot4 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
    # read the Source Classification Code (SCC) table
    SCC <- readRDS("data/Source_Classification_Code.rds")
    
    # extract the rows with SCC codes that correspond to coal based sources
    SCC_coal = SCC[getCoalSources(SCC),]
    
    # extract the NEI data that correspond to coal based SCC codes
    NEI_coal = NEI[NEI$SCC %in% SCC_coal$SCC,]
    
    # reshape the data so that its ID is based on the year
    NEI_coal_melt = melt(NEI_coal, id = "year", measure.vars="Emissions")    
    
    # cast the data based on year using totals
    NEI_coal_melt_total_by_year = dcast(NEI_coal_melt, year ~ variable, sum)
    
    # Plot the results
    png("plot4.png")
    with(NEI_coal_melt_total_by_year, {
        plot(year, Emissions, type = "n", xlab="year", ylab="coal emissions")
        lines(year, Emissions)
    })
    dev.off()

}

#
#   Iterate through the Short.Name column of the SCC data frame and return a 
#   true/false vector corresponding to the indices that have "coal" in the 
#   string with some exceptions like coal mining and charcoal. The focus is to
#   find coal sources that correspond to coal combustion
#
getCoalSources <- function(SCC)
{
    resultVector = vector()
    coalstr = "coal"
    exclusionStr1 = "Coal Mining"
    exclusionStr2 = "charcoal"
    idx = 1
    for (sourceName in SCC$Short.Name)
    {
        # do not include charcoal burning or coal mining
        if( length(grep(exclusionStr1, sourceName, ignore.case = TRUE)) > 0 |
                length(grep(exclusionStr2, sourceName, ignore.case = TRUE)) > 0)
        {
            resultVector[idx] = FALSE
        }
        # once past the earlier filter, check if the string contains coal
        else if( length(grep(coalstr, sourceName, ignore.case = TRUE)) > 0)
        {
            resultVector[idx] = TRUE
        }
        else
        {
            resultVector[idx] = FALSE
        }
        idx = idx + 1
    }
    resultVector
}