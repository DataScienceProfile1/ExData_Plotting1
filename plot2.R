## Exploratory Data Analysis Project 1
## Plot 2

# Load electric power consumption data from the UC Irvine Machine Learning Repository
# and construct exploratory plots as laid out in the course requirements

# Load required libraries
    require(dplyr)
    require(lubridate)

#Specify URL and name of zip file on course website, name of txt file containing data
    fURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipFileName <- "./HouseholdPower.zip"
    txtFileName <- "./household_power_consumption.txt"

# If txt file not in working directory, download from course page and extract to working directory
    if (!file.exists(txtFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName)
        unzip(zipFileName)
    } else {message("Data file found in working directory")}

# read data into table from txt file and convert date from character to POSIXLT
    if (!exists("tblPower")){
    # Calculate a rough estimate of how much memory the dataset will require
    # in memory before reading into R. Make sure your computer has enough memory
    fileSizeMb <- file.info(txtFileName)$size*1.0e-6
    memLimit <- memory.limit()
    
    # Exit with error message if computer does not have enough memory
    if (fileSizeMb > memLimit){
        stop("Insufficient memory to read table - aborting script.")
    } else {
    message("Reading table from data file")
    tblPower <- read.table(file = txtFileName,sep = ";", header = TRUE,na.strings = "?",colClasses = c(rep("character",2),rep("numeric",7)))
    tblPower$Date <- as.Date(tblPower$Date,"%d/%m/%Y")    
    }} else{ message("Using data table from memory")}
    
# Create subset of data only from 2007/02/01 and 2007/02/02 using dplyr::filter 
    tblPowerSmall <- filter(tblPower, Date == as.Date("2007/02/01") | Date == as.Date("2007/02/02"))
# Combine date and time into single PosixLT date column
    tblPowerSmall <- mutate(tblPowerSmall,DateTime = ymd_hms(paste(tblPowerSmall$Date,tblPowerSmall$Time)))
    message("Generating plots")
    
# Generate Plot 2
    png("plot2.png",width=480,height = 480)
    par(mfrow = c(1,1))
    plot(x = tblPowerSmall$DateTime, tblPowerSmall$Global_active_power, type="l",
         col = "black",ylab = "Global Active Power",xlab = "")
    dev.off()