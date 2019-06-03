################################ PART 1 ########################################
#  this part just downloads the data and saves it to 
# "./data/stormData.csv"

# see download_and_read_storm_data   for details

################################ PART 2 ########################################
library(data.table)
##   A) read the downloaded data (takes a bit of time):
data <- read.csv("./data/stormData.csv") 

##   B) read it into data.table for faster processing:
DT <- data.table(data)   #dim(DT) = 902297rows    37cols

##   C) select only the following observations:
DT <- DT[DT$FATALITIES > 0 | DT$INJURIES > 0 | DT$PROPDMG > 0 | DT$CROPDMG > 0,]

##   D) further, select only the observations from the 50 states
DT <- DT[STATE %in% state.abb]

##   E) Now save this smaller file. We'll use this one in furter analysis
data <- file.path(getwd(), "SmallerDataFile.csv") 
write.csv(DT, file = datafile, row.names=FALSE) 