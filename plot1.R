plot1 <- function() {
  
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
  
  ##set up png file
  png(file = "plot1.png", width = 480, height = 480)
  
  ##create histogram as specified in project
  with(sub, hist(Global_active_power, 
                 main = "Global Active Power", 
                 xlab = "Global Active Power (kilowatts)",
                 col = "orangered3"))
  
  ##run dev.off() to close the png file and save it to working directory
  dev.off()
}