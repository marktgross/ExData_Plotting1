plot2 <- function() {
  
  ##define zip_url
  zip_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  ##download .zip file to working directory
  download.file(zip_url, destfile = "project_data.zip")
  ##unzip data file to "project" folder in working directory
  unzip("project_data.zip", exdir = "project")
  
  ##create column class types
  types <- c("character", "character", "numeric", "numeric", "numeric", "numeric", 
             "numeric", "numeric", "numeric")
  
  ##read full file into R
  data <- read.table(paste(getwd(), "project", list.files("./project"), sep = "/"), 
                     header = TRUE, sep = ";", na.strings = c("?"), colClasses = types)
  ##create subset for specific dates
  sub <- subset(data, Date == "1/2/2007"|Date == "2/2/2007")
  ##order subset by Date and Time
  sub <- sub[order(sub$Date, sub$Time), ]
  
  ##convert Date column to date format
  sub$Date <- as.Date(sub$Date, "%d/%m/%Y")
  ##create x-axis labels (abbreviated weekdays)
    ##find days included
    days <- unique(sub$Date)
    days <- c(days, days[2] + 1)
    ##create abbreviated weekdays vector
    x <- weekdays(days, abbreviate = TRUE)
    
  ##figure out where tick marks should be
    ##create logical vector to determine where 2007-02-02 records are in the data set
    dt <- sub$Date == "2007-02-02"
    ##create vector of x-axis positions
      ##first position = 0
      ##second position = first occurrence of 2007-02-02
      ##third position = number of rows in the subset
    x_tick <- c(0, min(which(dt == TRUE)), nrow(sub))
  
  ##set up png file
  png(file = "plot2.png", width = 480, height = 480)
  
  ##plot Global Active Power data as a line graph
  plot(sub$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", 
       xlab = NA, xaxt = "n")
  
  ##add x-axis tick marks and labels
  axis(side = 1, at = x_tick, labels = x, tck = -.04)
  
  ##run dev.off() to close the png file and save it to working directory
  dev.off()
  
}