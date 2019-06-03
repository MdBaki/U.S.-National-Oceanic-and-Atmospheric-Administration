---
title: "Untitled"
author: "baki"
date: "6/2/2019"
output: html_document
---

```{r setup, include=FALSE}
# set global chunk options
opts_chunk$set(cache=TRUE, eval=TRUE)
```




Exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) Storm Database
========================================================

## Synopsis
The NOAA database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. In this project we explore the NOAA Storm Database and answer a few basic questions about severe weather events. In particular, we are interested in the following: which types of events are most harmful with respect to population health across the United States, and which types of events have the greatest economic consequences across the United States. We find that, over the period from 1993 through 2011, extreme temperature events resulted in the most fatalities, Illinois being the state that suffered the most (998 fatalities). Texas suffered from the greatest number of flood events, resulting in the most number of injuries (6,951). The state of California suffered the most property damages, mainly from flood events ($117.4 billion). 

## Organization
This document is organized as follows
- *Data Preprocessing.* This involves downloading the NOAA data and loading it into `R`, and can be time consuming. (The zipped database is about 49 Mb and unzips to about one-half Gb `csv` file).
- *Analysis*
  - *Categorization of Storm Events.* The NOAA data contains a vast number of event categories (variable `EVTYPE`) - 985 categories. We group these events into 5 major categories: `Convection`, `Extreme Temperatures`, `Flood`, `Winter`, and ` Other`. The rationale for choosing these categories is drawn from [this document] (http://www.ncdc.noaa.gov/oa/climate/sd/annsum2009.pdf).
  - *Property and Crop Damages*
  - *Injuries and Fatalities*
- *Results*

## Data Processing
### Getting Data
The data (in `bz2` zipped format) were downloaded from [this url] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) and stored locally as a `csv` (comma-separated values) file. The following piece of `R` code accomplishes this:

```{r downloadData}
if (!file.exists("data"))  dir.create("data")
if (!file.exists("./data/stormData.csv")) {
    temp <- tempfile()
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, temp)
    data <- read.csv(bzfile(temp))
    write.csv(data, file="./data/stormData.csv", row.names=FALSE)
    unlink(temp)
}
```

We read the file into `R` and check the first few rows (of `902,297` total rows).
Of the total of `37` variables only a handful will be of interest to us.

```{r readData, echo=FALSE}
data <- read.csv("./data/stormData.csv")
dim(data)
head(data[, 1:7])
```


### Preprocessing

We begin with the `csv` file that was created by running the previous step. We 
convert the data to a `data.table` (using package `data.table`) for faster processing. 
We then extract those observations that contain entries for fatalities, injuries,
crop damage, or property damage:

A few things we do at the beginning:
- Convert the data to a `data.table` (package `data.table`) for faster processing
- Extract those observations that contain entries for fatalities, injuries,
crop damage, or property damage
- Further restrict the observations to the 50 US states only 
- We will need to clean up the `EVTYPE` (`event type`) column of this data. The
previous step saved us from a lot of unnecessary cleanup by removing non-essential data,



```{r shrinkData, echo=FALSE}
library(data.table)
DT <- data.table(data)
##   A) read the downloaded data (takes a bit of time):
data <- read.csv("./data/stormData.csv") 
##   B) read it into data.table for faster processing:
DT <- data.table(data)   #dim(DT) = 902297rows    37cols
##   C) select only the following observations:
DT <- DT[DT$FATALITIES > 0 | DT$INJURIES > 0 | DT$PROPDMG > 0 | DT$CROPDMG > 0,]
##   D) further, select only the observations from the 50 states
DT <- DT[STATE %in% state.abb]
data <- file.path(getwd(), "SmallerDataFile.csv") 
# read the smaller data (created with `reduce_dataset_and_save.R`) into dataframe
df <- read.csv("SmallerDataFile.csv", header=TRUE)
# convert it to a `data.table` for faster processing
dt <- data.table(df)
```

After the preprocessing step, the dataset has been reduced to 253,210 observations - 
a size a bit more manageable than that of the original dataset.

We next create the 5 different categories for the `Event Types`, and find the events 
that fall into these 5 categories. Also, create *intidactor variables* for each 
of the categories. For any event, its indicator variable takes on the value of 
`TRUE` if that occurred and `FALSE` otherwise. For example `FloodI` indicator shows 
whether the event in question was categorizes as a `Flood` event or not.

```{r categorizeData1, echo=FALSE, results='hide'}
setnames(dt, names(dt), tolower(names(dt)))
#     Category 1: `Convection` 
#     Lightning, Tornado, Thunderstorm, Wind, and Hail fall into this category
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
```




```{r categorizeData2TEMP, echo=FALSE, results='hide'}
#     Category 2: `Extreme Temperatures`
#     Cold and Heat fall into this category
regex <- "COLD|HEAT"
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, ExtremeTempI := indicator]
#here is the list:
uniqueEventTypes <- unique(dt[indicator, evtype])
show(uniqueEventTypes[order(uniqueEventTypes)])
```

For example, the following events fall into the `Extreme Temperatures` category:
```{r}
uniqueEventTypes <- unique(dt[indicator, evtype])
show(uniqueEventTypes[order(uniqueEventTypes)])
```



```{r categorizeData3FLOOD, echo=FALSE}
#     Category 3: `Flood`
#     Flod and Rain fall into this category
flood <- "(\\bFL\\S+?D)"
rain <- "RAIN|PRECIP|SHOWER"
regex <- paste(flood, rain, sep = "|")
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, FloodI := indicator]
uniqueEventTypes <- unique(dt[indicator, evtype])
#show(uniqueEventTypes[order(uniqueEventTypes)])
```

```{r categorizeData4WINTER, echo=FALSE}
#     Category 4: `Winter`
#     Snow, Ice, Freeze, or Winter Weather fall into this category
regex <- "(SNOW)|(ICE)|(ICY)|(FREEZ)|(WINT)"
indicator <- grepl(regex, dt$evtype, ignore.case = TRUE)
dt <- dt[, WinterI:= indicator]
uniqueEventTypes <- unique(dt[indicator, evtype])
#show(uniqueEventTypes[order(uniqueEventTypes)])
```

```{r categorizeData5OTHER, echo=FALSE}
#     Category 5: `Other`
#     Everything else that hasn't been categorized so far falls into this category
dt <- dt[, OtherI:= ConvectionI == FALSE & ExtremeTempI == FALSE & FloodI == FALSE & WinterI == FALSE]
```

And here are the results cross-tabulated:

```{r crossTabulate, echo=FALSE}
# Cross Tabulate the results
groupby <- expression(list(ConvectionI, ExtremeTempI, FloodI, WinterI, OtherI))
#head(dt)
dt[, .N, eval(groupby)][order(ConvectionI, ExtremeTempI, FloodI, WinterI, 
                              OtherI, decreasing = TRUE)]
```

```{r echo=FALSE}
## Now that we've set up the categories, an additional step is needed
# We note that several records can have multiple events listed in the `EVTYPE` variable.
dt <- dt[, Category := ifelse(ConvectionI, 1, ifelse(ExtremeTempI, 
                                                               2, ifelse(FloodI, 3, ifelse(WinterI, 4, ifelse(OtherI, 5, NA)))))]
CategoryName <- c("Convection", "Extreme Temperature", "Flood", "Winter", "Other")
dt <- dt[, Category := factor(Category, labels = CategoryName)]
dt[, .N, Category]
```


This reveals that a disproportionate number of events fall under the `Convection` 
category. Perhaps there were more observations taken for this category.

Looking at the dates more carefully, we see that:

```{r, echo=FALSE}
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
#dim(dt)
```

Thus we will be considering data from 1993 to 2011. This further reduces the 
number of observations:

```{r}
dim(dt)
```
## Results 
### Damage Assessment
The property and crop damages inflicted by the storms during the years of 1993-2011 were 
as follows (`B` stands for `billion`, `M`- for `million`, `K` - for `thousand`, all in USD): 

```{r PropAndCropDamage, cache=TRUE, echo=FALSE}
dt <- dt[, `:=`(propdmgexp, toupper(propdmgexp))]
dt[, .N, propdmgexp]
dt <- dt[, property.damage := ifelse(propdmgexp == "B", propdmg * 1e+09, ifelse(propdmgexp == "M", propdmg * 1e+06, ifelse(propdmgexp == "K", propdmg * 1000, ifelse(propdmgexp =="H", propdmg * 100, propdmg))))]
summary(dt$property.damage)
################## crop damages ##################
dt <- dt[, `:=`(cropdmgexp, toupper(cropdmgexp))]
dt[, .N, cropdmgexp]
dt <- dt[, crop.damage := ifelse(cropdmgexp == "B", cropdmg * 1e+09, ifelse(cropdmgexp == "M", cropdmg * 1e+06, ifelse(cropdmgexp == "K", cropdmg * 1000, ifelse(cropdmgexp =="H", cropdmg * 100, cropdmg))))]
summary(dt$crop.damage)
```


### Fatalities and Injuries

```{r reshapeData, echo=FALSE}
####################################################
# First, reshape the data
D <- dt
labels <- c("Convection", "Extreme Temperature", "Flood", "Winter", "Other")
D2 <- rbind(D[, list(state, year = year(bgn_date), Category = factor(Category, 
                                                                           labels = labels), outcome = "Fatalities (thousands)", value = fatalities/1000)], 
            D[, list(state, year = year(bgn_date), Category = factor(Category, 
                                                                           labels = labels), outcome = "Injuries (thousands)", value = injuries/1000)], 
            D[, list(state, year = year(bgn_date), Category = factor(Category, 
                                                                           labels = labels), outcome = "Property damage ($, billions)", value = propdmg/1e+09)])
```

```{r tabulate, echo=FALSE}
###Tabulate by state and category, and by state. For the state tabulation, rank each state according to outcome.
tabulateStateCategory <- D2[, list(value = sum(value)), list(state, Category, 
                                                               outcome)]
tabulateState <- D2[, list(value = sum(value)), list(state, outcome)]
tabulateState <- tabulateState[, `:=`(rank, abs(rank(value, ties.method = "random") - 
                                                        51)), list(outcome)]
```

#### Fatalities
The fatalities resulting from extreme weather over this period are shown in the 
following table. For example, the state of Illinois suffered the most 
fatalities, 998, due to extreme temperatures:

```{r fatalities, echo=FALSE}
#
library(xtable)
top <- tabulateState[grepl("Fatal", outcome) & rank <= 1, state]
where <- expression(state %in% top & grepl("Fatal", outcome))
select <- expression(list(state, value = format(value * 1000, big.mark = ","), 
                          Category))
tabulation <- tabulateStateCategory[eval(where), eval(select)]
tabulation <- tabulation[order(value, decreasing = TRUE)]
# print(xtable(tabulation, digits = 0), type = "html", include.rownames = FALSE)
print(tabulation)
```

#### Injuries
The injuries resulting from extreme weather are also shown. For example, the 
state of Texas suffered 6,951 injuries as a result of floods over this period:

```{r injuries, echo=FALSE}
top <- tabulateState[grepl("Inj", outcome) & rank <= 1, state]
where <- expression(state %in% top & grepl("Inj", outcome))
select <- expression(list(state, value = format(value * 1000, big.mark = ","), 
                          Category))
tabulation <- tabulateStateCategory[eval(where), eval(select)]
tabulation <- tabulation[order(value, decreasing = TRUE)]
#print(xtable(tabulation, digits = 0), type = "html", include.rownames = FALSE)
print(tabulation)
```

### Plots
```{r plotResults, echo=FALSE, fig.width=11, fig.align='center'}
library(ggplot2)
ggplot(tabulateStateCategory, aes(x = state, y = value, fill = Category)) + 
    geom_bar(alpha = 1/2, stat = "identity") + scale_fill_brewer(name = "Category", 
                                                                 palette = 3) + scale_x_discrete(name = "") + scale_y_continuous(name = "") + 
    facet_wrap(~outcome, scales = "free", nrow = 3, ncol = 1) + theme(legend.position = "bottom")
```



### Acknowledgments:
* The rationale for selecting these 5 categories was drawn [from here] (http://www.ncdc.noaa.gov/oa/climate/sd/annsum2009.pdf). And the ideas and methods on how to do this using regular expressions were gleaned from various sources, including the discussion forums at `stackoverflow` and coursera's discussion forums ([e.g.this](https://class.coursera.org/repdata-002/forum/thread?thread_id=32) or [this](https://class.coursera.org/repdata-002/forum/thread?thread_id=46)).
