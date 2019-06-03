# this portion of the script deal with the damages found in the storm data
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