#
#   This script builds the plot that answers the following question:   
#
#   5. How have emissions from motor vehicle sources changed from 1999–2008 in 
#   Baltimore City?
#
library(reshape2)

doPlot5 <- function()
{
    # read the National Emissions Inventory (NEI) table
    NEI <- readRDS("data/summarySCC_PM25.rds")
    
    # read the Source Classification Code (SCC) table
    SCC <- readRDS("data/Source_Classification_Code.rds")
    
    # extract the rows with SCC codes that correspond to motor vehicle sources
    SCC_motor_vehicle = SCC[getMotorVehicleSources(SCC),]

    # filter the data to just contain Baltimore data 
    NEI_Baltimore = NEI[NEI$fips == "24510", ]
    
    # filter the Baltimore data to just have the motor vehicle emission data
    NEI_Baltimore_MotorVehicle = 
        NEI_Baltimore[NEI_Baltimore$SCC %in% SCC_motor_vehicle$SCC,]
    
    # reshape the data so that its ID is based on the year
    NEI_Baltimore_MotorVehicle_melt = 
        melt(NEI_Baltimore_MotorVehicle, id = "year", measure.vars="Emissions")    
    
    # cast the data based on year using totals
    NEI_Baltimore_MotorVehicle_melt_total_by_year = 
        dcast(NEI_Baltimore_MotorVehicle_melt, year ~ variable, sum)
    
    # Plot the results
    png("plot5.png")
    with(NEI_Baltimore_MotorVehicle_melt_total_by_year, {
        plot(year, Emissions, type = "n", 
             xlab="year", ylab="motor vehicle emissions - Baltimore")
        lines(year, Emissions)
    })
    dev.off()
    
}

##
##  the function returns a vector of TRUE/FALSE corresponding to row indices
##  that contain motor vehicle emissions codes. 
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