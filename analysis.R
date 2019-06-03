################################################################################
library(data.table)
# read the smaller data (created with `reduce_dataset_and_save.R`) into dataframe
df <- read.csv("SmallerDataFile.csv", header=TRUE)
# convert it to a `data.table` for faster processing
dt <- data.table(df)
################################################################################

#### A)  first, convert the names to lowercase:

setnames(dt, names(dt), tolower(names(dt)))

#### B)  see how many unique Event Types we have:
cat("Unique Event Types: ", length(unique(dt$evtype)))


#### C) This is the most important step. For this bewildering variety of event 
#       types, we will create the following 5 categories. We chose this based on: 
#       http://www.ncdc.noaa.gov/oa/climate/sd/annsum2009.pdf
#
#       The smaller datset has 471 different categories (down from 985)
#       We put these different events into 5 categories as follows:
#
#      ##  Create an indicator variable for each category: ##


###########################################
#     Category 1: `Convection` 
#     Lightning, Tornado, Thunderstorm, Wind, and Hail fall into this category
###########################################
lightning <- "\\bL\\S+?G\\b"
tornado <- "(NADO)|(\\bTOR\\S+?O\\b|(\\bFUN))"
thunderstorm <- "THUNDERSTORM|TSTM"
wind <- "(WIND)|(WND)"
hail <- "HAIL"
regex <- paste(lightning, tornado, thunderstorm, wind, hail, sep = "|")


indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)

dt <- dt[, ConvectionI := indicator]
#here is the list:
uniqueEventTypes <- unique(dt[indicator, evtype])
#show(uniqueEventTypes[order(uniqueEventTypes)])

###########################################
#     Category 2: `Extreme Temperatures`
#     Cold and Heat fall into this category
###########################################
regex <- "COLD|HEAT"
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, ExtremeTempI := indicator]
#here is the list:
uniqueEventTypes <- unique(dt[indicator, evtype])
show(uniqueEventTypes[order(uniqueEventTypes)])


###########################################
#     Category 3: `Flood`
#     Flod and Rain fall into this category
###########################################
flood <- "(\\bFL\\S+?D)"
rain <- "RAIN|PRECIP|SHOWER"
regex <- paste(flood, rain, sep = "|")
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, FloodI := indicator]
uniqueEventTypes <- unique(dt[indicator, evtype])
#show(uniqueEventTypes[order(uniqueEventTypes)])


###########################################
#     Category 4: `Winter`
#     Snow, Ice, Freeze, or Winter Weather fall into this category
###########################################
regex <- "(SNOW)|(ICE)|(ICY)|(FREEZ)|(WINT)"
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, WinterI:= indicator]
uniqueEventTypes <- unique(dt[indicator, evtype])
#show(uniqueEventTypes[order(uniqueEventTypes)])


###########################################
#     Category 5: `Other`
#     Everything else that hasn't been categorized so far falls into this category
###########################################

dt <- dt[, OtherI:= ConvectionI == FALSE & ExtremeTempI == FALSE & 
           FloodI == FALSE & WinterI == FALSE]



############################################
# Finally, cross-tabulate:

groupby <- expression(list(ConvectionI, ExtremeTempI, FloodI, WinterI, OtherI))
dt[, .N, eval(groupby)][order(ConvectionI, ExtremeTempI, FloodI, WinterI, 
                              OtherI, decreasing = TRUE)]



################################################################################
## Now that we've set up the categories, an additional step is needed
# We note that several records can have multiple events listed in the `EVTYPE` variable.


dt <- dt[, Category := ifelse(ConvectionI, 1, ifelse(ExtremeTempI, 
                                                     2, ifelse(FloodI, 3, ifelse(WinterI, 4, ifelse(OtherI, 5, NA)))))]
CategoryName <- c("Convection", "Extreme Temperature", "Flood", "Winter", "Other")
dt <- dt[, Category := factor(Category, labels = CategoryName)]
dt[, .N, Category]


################################################################################
##
#    This reveals that a disproportionate number of events fall under the `Convection` 
#    category. Perhaps there were more observations taken for this category.
#    Let's consider the dates.
#
dt$bgn_date <- as.Date(dt$bgn_date, format="%m/%d/%Y")
#class(dt$bgn_date)

year <- year(min(dt$bgn_date[dt$Category == "Convection"]))
cat("Earliest observation for Event Convection: ", year)

year <- year(min(dt$bgn_date[dt$Category != "Convection"]))
cat("Earliest observation for Event that is NOT Convection: ", year)

############ 
# Therefore, we further subset the dataset and only consider the events after 1993
min.year <- year(min(dt$bgn_date[dt$Category != "Convection"]))
max.year <- year(max(dt$bgn_date)) # this is 2011

dt <- dt[min.year <= year(bgn_date) & year(bgn_date) <= max.year]
dim(dt)





################################################################################
##
#
#
## this portion of the script deal with the damages found in the storm data
#
#

################## property damges ################

dt <- dt[, `:=`(propdmgexp, toupper(propdmgexp))]
dt[, .N, propdmgexp]



dt <- dt[, property.damage := ifelse(propdmgexp == "B", propdmg * 1e+09, ifelse(propdmgexp == "M", propdmg * 1e+06, ifelse(propdmgexp == "K", propdmg * 1000, ifelse(propdmgexp =="H", propdmg * 100, propdmg))))]

summary(dt$property.damage)


################## crop damages ##################
dt <- dt[, `:=`(cropdmgexp, toupper(cropdmgexp))]
dt[, .N, cropdmgexp]
dt <- dt[, crop.damage := ifelse(cropdmgexp == "B", cropdmg * 1e+09, ifelse(cropdmgexp == "M", cropdmg * 1e+06, ifelse(cropdmgexp == "K", cropdmg * 1000, ifelse(cropdmgexp =="H", cropdmg * 100, cropdmg))))]

summary(dt$crop.damage)





############################################################################
#
#
#   Plotting the results
