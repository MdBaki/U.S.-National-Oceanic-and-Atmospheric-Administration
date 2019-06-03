# RESULTS
#

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




################################################################################
###Tabulate by state and category, and by state. For the state tabulation, rank each state according to outcome.


tabulateStateCategory <- D2[, list(value = sum(value)), list(state, Category, 
                                                             outcome)]
tabulateState <- D2[, list(value = sum(value)), list(state, outcome)]
tabulateState <- tabulateState[, `:=`(rank, abs(rank(value, ties.method = "random") - 
                                                  51)), list(outcome)]



################################################################################
library(ggplot2)
ggplot(tabulateStateCategory, aes(x = state, y = value, fill = Category)) + 
  geom_bar(alpha = 1/2, stat = "identity") + scale_fill_brewer(name = "Category", 
                                                               palette = "Set1") + scale_x_discrete(name = "") + scale_y_continuous(name = "") + 
  facet_wrap(~outcome, scales = "free", nrow = 3, ncol = 1) + theme(legend.position = "bottom")

################################################################################
### Fatalities
#
#
library(xtable)
top <- tabulateState[grepl("Fatal", outcome) & rank <= 1, state]
where <- expression(state %in% top & grepl("Fatal", outcome))
select <- expression(list(state, value = format(value * 1000, big.mark = ","), 
                          Category))
tabulation <- tabulateStateCategory[eval(where), eval(select)]
tabulation <- tabulation[order(value, decreasing = TRUE)]
print(xtable(tabulation, digits = 0), type = "html", include.rownames = FALSE)

print(tabulation)


################################################################################
### Injuries
#
#
top <- tabulateState[grepl("Inj", outcome) & rank <= 1, state]
where <- expression(state %in% top & grepl("Inj", outcome))
select <- expression(list(state, value = format(value * 1000, big.mark = ","), 
                          Category))
tabulation <- tabulateStateCategory[eval(where), eval(select)]
tabulation <- tabulation[order(value, decreasing = TRUE)]
print(xtable(tabulation, digits = 0), type = "html", include.rownames = FALSE)

print(tabulation)


################################################################################
### Property Damage
#
#
top <- tabulationState[grepl("Prop", outcome) & rank <= 1, state]
where <- expression(state %in% top & grepl("Prop", outcome))
select <- expression(list(state, value = sprintf("$%s billion", format(round(value, 
                                                                             digits = 1), big.mark = ",")), Category))
tabulation <- tabulationStateCategory[eval(where), eval(select)]
tabulation <- tabulation[order(value, decreasing = TRUE)]
print(xtable(tabulation, digits = 0), type = "html", include.rownames = FALSE)

print(tabulation)