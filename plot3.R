library(datasets)

#============================================================================= start getPwrMsrmnts #
# pwrMsrmnts <- function(file)
# Reads the input file and returns a data.frame with the mesuremnts
# Where :
#    file - input file containig all mesurements in format:
#               (Date; Time; Global_active_power; Global_reactive_power; Voltage; 
#                Global_intensity; Sub_metering_1; Sub_metering_2; Sub_metering_3)
#==================================================================================================#
getPwrMsrmnts <- function(file) {
    
    cClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric", "numeric")
    
    data <- read.csv2(file, header = TRUE, sep = ";", quote = "\"", na.strings ="?",
                      dec = ".", fill = TRUE, comment.char = "", colClasses = cClasses)
    
    # remove all incomplete mesurements 
    goodMsurs <- complete.cases(data)
    data <- data[goodMsurs,]
    
    data
}
#=============================================================================== end getPwrMsrmnts #

#========================================================================== start getDataForPeriod #
# getDataForPeriod(data, startDate, endDate)
# Returns data for specific pertiod between StartDate and endDate
# Where :
#    data - input data.frame containig all mesurements
#    startData - start date of the period, defaults to "1/2/2007"
#    endDate   - end date of the period, defaults to "2/2/2007"
#==================================================================================================#
getDataForPeriod <- function(data, startDate = "1/2/2007", endDate = "2/2/2007") {
    
    startDate = as.Date(startDate,"%d/%m/%Y")
    endDate = as.Date(endDate,"%d/%m/%Y")
    
    # Cast to Date format
    data[[1]] <- as.Date( data[[1]], "%d/%m/%Y")
    
    data <- within( 
        subset( data, data$Date >= startDate & data$Date <= endDate,), {
            # add Datetime column
            DateTime <- strptime(paste( as.character(Date), Time),"%Y-%m-%d %T")
        })
    # Uncomment for DBG 
    print(str(data))
    data
}
#============================================================================= end getDataForPeriod #

#===================================================================================== start submtrdays #
# submtrdays(data, plotFile) - creates png plot file for Energy sub mettering/time chart
# Where :
#     data -     data.frame with columns :
#                    (Date, Time, Global_active_power, Global_reactive_power, Voltage, 
#                     Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3, DateTime)
#     plotFile - output plot file
#================================================================================================= #
submtrdays <- function(inData, plotFile) {
    png(filename = plotFile,
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white",  res = NA,
        type = c("cairo", "cairo-png", "Xlib", "quartz"))
    
    with (inData, {
        # Plot Energy Sub Mettering
        plot( DateTime, Sub_metering_1, 
              type = "l", 
              ylab = "Energy sub mettering", xlab = "")
        points(DateTime,Sub_metering_2, type = "l", col = "red")
        points(DateTime,Sub_metering_3, type = "l", col = "blue")
        legend("topright" , lwd = 2, lty = 1, col = c("black", "red", "blue"), 
               legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    })
    dev.off()
}
#====================================================================================== end submtrdays #

pwrData <- getPwrMsrmnts( "./household_power_consumption.txt")

submtrdays( getDataForPeriod(pwrData, "1/2/2007", "2/2/2007"), "plot3.png")
