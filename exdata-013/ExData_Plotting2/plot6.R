#
#   This script builds the plot that answers the following question:   
#
#   6. Compare emissions from motor vehicle sources in Baltimore City with 
#   emissions from motor vehicle sources in Los Angeles County, California 
#   (fips == "06037"). Which city has seen greater changes over time in motor 
#   vehicle emissions?
#
library(reshape2)
library(ggplot2)

doPlot6 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
    # read the Source Classification Code (SCC) table
    SCC <- readRDS("data/Source_Classification_Code.rds")
    
    # extract the rows with SCC codes that correspond to motor vehicle sources
    SCC_motor_vehicle = SCC[getMotorVehicleSources(SCC),]
    
    # filter the data to just contain Baltimore & LA data 
    NEI_Balt_LA = NEI[NEI$fips %in% c("24510","06037"), ]
    
    # filter the Baltimore data to just have the motor vehicle emission data
    NEI_City_MotorVehicle = 
        NEI_Balt_LA[NEI_Balt_LA$SCC %in% SCC_motor_vehicle$SCC,]
    
    # reshape the data so that its ID is based on the year
    NEI_City_MotorVehicle_melt = 
        melt(NEI_City_MotorVehicle, 
             id = c("year", "fips"), 
             measure.vars="Emissions")    
    
    # cast the data based on year using totals
    NEI_City_MotorVehicle_melt_total_by_year = 
        dcast(NEI_City_MotorVehicle_melt, year + fips ~ variable, sum)    
    
    # plot the data
    png("plot6.png", width = 720)
    gplot = qplot(year, Emissions, 
                  data=NEI_City_MotorVehicle_melt_total_by_year,
                  color = fips, geom=c("point","smooth"), method="lm")
    print(gplot)
    dev.off()
}

##
##  the function returns a vector of TRUE/FALSE corresponding to row indices
##  that contain motor vehicle emissions codes
##  approach: look in the EI.Sector column for strings that contain the words
##  "mobile" and "vehicle"
##
getMotorVehicleSources <- function(SCC)
{
    resultVector = vector()
    vehicleStr = "vehicle"
    motorStr = "mobile"
    idx = 1
    for (emissionSource in SCC$EI.Sector)
    {
        if( length(grep(vehicleStr, emissionSource, ignore.case = TRUE)) > 0 &&
                length(grep(motorStr, emissionSource, ignore.case = TRUE)) > 0 )
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