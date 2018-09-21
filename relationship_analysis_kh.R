setwd("C:/Users/kevin/OneDrive/School/MIDS/W203 - Statistics for Data Science/Lab 1/W203_lab1_corruption")
#library(car)
#library(grid)
#library(ggplot2)

load("Corrupt.Rdata")

## Correct Data Problems
#Fix majoritymulsim where value = -1, should be 0
FMcorrupt[FMcorrupt$majoritymuslim == -1 & ! is.na(FMcorrupt$majoritymuslim), "majoritymuslim"] = 0

# Add missing counties.  Reference: https://www.worldatlas.com/aatlas/ctycodes.htm
FMcorrupt[FMcorrupt$wbcode == "ARE", "country"] = "United Arab Emirates"
FMcorrupt[FMcorrupt$wbcode == "CAF", "country"] = "Central African Republic"
FMcorrupt[FMcorrupt$wbcode == "CAN", "country"] = "Canada"
FMcorrupt[FMcorrupt$wbcode == "COL", "country"] = "Columbia"
FMcorrupt[FMcorrupt$wbcode == "ECU", "country"] = "Ecuador"
FMcorrupt[FMcorrupt$wbcode == "JAM", "country"] = "Jamaica"
FMcorrupt[FMcorrupt$wbcode == "LVA", "country"] = "Latvia"
FMcorrupt[FMcorrupt$wbcode == "NOR", "country"] = "Norway"
FMcorrupt[FMcorrupt$wbcode == "PAN", "country"] = "Panama"
FMcorrupt[FMcorrupt$wbcode == "SWE", "country"] = "Sweden"
FMcorrupt[FMcorrupt$wbcode == "TUR", "country"] = "Turkey"

# Create named regions variable using region
FMcorrupt$region_name = NA
FMcorrupt[FMcorrupt$region == 1 & ! is.na(FMcorrupt$region), "region_name"] = "Caribbean"
FMcorrupt[FMcorrupt$region == 2 & ! is.na(FMcorrupt$region), "region_name"] = "South America"
FMcorrupt[FMcorrupt$region == 3 & ! is.na(FMcorrupt$region), "region_name"] = "Europe"
FMcorrupt[FMcorrupt$region == 4 & ! is.na(FMcorrupt$region), "region_name"] = "Asia" # "South Asia"
FMcorrupt[FMcorrupt$region == 5 & ! is.na(FMcorrupt$region), "region_name"] = "Oceania"
FMcorrupt[FMcorrupt$region == 6 & ! is.na(FMcorrupt$region), "region_name"] = "Africa"
FMcorrupt[FMcorrupt$region == 7 & ! is.na(FMcorrupt$region), "region_name"] = "Middle East" # "Western Asia"

FMcorrupt$region_name = factor(FMcorrupt$region_name)

# Remove 66 rows that do not have relevant data to the key analyses
corrupt = subset(FMcorrupt, !is.na(violations) & !is.na(mission) & !is.na(staff) )

# split data in to pre and post, before and after enforcement changes
cor_pre = subset(corrupt, prepost == "pre")
cor_pos = subset(corrupt, prepost == "pos")

# Merge both the above to one line with pre and pos appeneded to variable names (prepos removed)
cor_oneline = merge(cor_pre, cor_pos, by = "wbcode", suffixes = c(".pre", ".pos"))

# Grab only the variables that are needed.
cor_oneline = cor_oneline[, c("wbcode", "violations.pre",  "violations.pos", "fines.pre", "fines.pos",
                              "mission.pre", "staff.pre", "spouse.pre", "gov_wage_gdp.pre", "pctmuslim.pre", "majoritymuslim.pre", "trade.pre",
                              "cars_total.pre", "cars_mission.pre", "pop1998.pre", "gdppcus1998.pre", "ecaid.pre", "milaid.pre", "corruption.pre", "totaid.pre",
                              "r_africa.pre", "r_middleeast.pre", "r_europe.pre", "r_southamerica.pre", "r_asia.pre",
                              "country.pre", "distUNplz.pre",
                              "region.pre", "region_name.pre"
)]

# Remove suffix where not needed. 
colnames(cor_oneline) =  c("wbcode", "violations.pre",  "violations.pos", "fines.pre", "fines.pos",
                           "mission", "staff", "spouse", "gov_wage_gdp", "pctmuslim", "majoritymuslim", "trade",
                           "cars_total", "cars_mission", "pop1998", "gdppcus1998", "ecaid", "milaid", "corruption", "totaid",
                           "r_africa", "r_middleeast", "r_europe", "r_southamerica", "r_asia",
                           "country", "distUNplz",
                           "region", "region_name"
)


# Violations and corrution are the target variables, however we need to treat violations so that the size of the mission doesn't skew results.  
# See what the "Key" relationship is by finding correlations using multiple treatments of violations.

remove(correlation_matrix_input)
correlation_matrix_input = cor_oneline[, c("corruption", "violations.pre", "fines.pre", "violations.pos", "fines.pos",
                                                        "staff", "spouse", "majoritymuslim", "pctmuslim", "trade", "cars_total", "cars_mission", "totaid"
                                                        )]

######################################################
# add violations treated with total cars
correlation_matrix_input$violations_weighted.cars_total.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$cars_total
correlation_matrix_input$violations_weighted.cars_total.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$cars_total

# add violations treated with cars_mission
correlation_matrix_input$violations_weighted.cars_mission.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$cars_mission
correlation_matrix_input$violations_weighted.cars_mission.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$cars_mission

# add violations treated with staff
correlation_matrix_input$violations_weighted.staff.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$staff
correlation_matrix_input$violations_weighted.staff.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$staff

# add violations treated with total people (staff + spouse)
correlation_matrix_input$total_people = correlation_matrix_input$staff + correlation_matrix_input$spouse

correlation_matrix_input$violations_weighted.total_people.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$total_people
correlation_matrix_input$violations_weighted.total_people.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$total_people

ignore = c("cars_total", "staff", "total_people", "cars_mission") # this are only part of the treatment (keeping violations for below)

# see what it looks like now.  
cor_correlations = round(cor(correlation_matrix_input[ , !(names(correlation_matrix_input) %in% ignore)], use = "complete.obs"), 3)
# Sorting, so strong postive correlations at begining, and negative at end.  
sort(cor_correlations[,c("corruption")][], decreasing = TRUE)

# Strong positive correlations with pctmuslim, majoritymuslim, violations_weighted.staff.pos, violations_weighted.total_people
# Strong negative correlations with trade

# Further treatment for muslim varibles required (below)
# Fines is a proxy to violations, note the similar correlation, don't need to use fines, violations are part of the treatment.
# The cars_mission treatments provide weak correlations
ignore = c(ignore, "fines.pre", "fines.pos", "violations.pre", "violations.pos", "violations_weighted.cars_mission.pre", "violations_weighted.cars_mission.pos")

######################################################
# The two muslim variables correlate with treated violations, 
# however, further treatment is required since they are percentage and bool.  
# Not looking at bool for correlations, and cars are not muslim, so I'm not bothering with that either.

# add violations treated with staff and pctmuslim
correlation_matrix_input$violations_weighted.staff_pctmuslim.pre = correlation_matrix_input$violations.pre/(correlation_matrix_input$staff * (correlation_matrix_input$pctmuslim + .00001))
correlation_matrix_input$violations_weighted.staff_pctmuslim.pos = correlation_matrix_input$violations.pos/(correlation_matrix_input$staff * (correlation_matrix_input$pctmuslim + .00001))

# add violations treated with total people and pctmuslim
correlation_matrix_input$violations_weighted.total_people_pctmuslim.pre = correlation_matrix_input$violations.pre/(correlation_matrix_input$total_people * (correlation_matrix_input$pctmuslim + .00001))
correlation_matrix_input$violations_weighted.total_people_pctmuslim.pos = correlation_matrix_input$violations.pos/(correlation_matrix_input$total_people * (correlation_matrix_input$pctmuslim + .00001))

ignore = c(ignore, "pctmuslim", "majoritymuslim")
# see what it looks like now.  
cor_correlations = round(cor(correlation_matrix_input[ , !(names(correlation_matrix_input) %in% ignore)], use = "complete.obs"), 3)
# Sorting, so strong postive correlations at begining, and negative at end.  
sort(cor_correlations[,c("corruption")][], decreasing = TRUE)

# after the treatment, the two muslim variables have a weak correaltion
ignore = c(ignore, "violations_weighted.staff_pctmuslim.pre",  "violations_weighted.staff_pctmuslim.pos", "violations_weighted.total_people_pctmuslim.pre", "violations_weighted.total_people_pctmuslim.pre")
# see what it looks like now.  
cor_correlations = round(cor(correlation_matrix_input[ , !(names(correlation_matrix_input) %in% ignore)], use = "complete.obs"), 3)
# Sorting, so strong postive correlations at begining, and negative at end.  
sort(cor_correlations[,c("corruption")][], decreasing = TRUE)

#cor_key_variables = subset(cor_oneline, c("corruption", "staff", ))
  
par(mfrow = c(3, 2))
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.staff.pre)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.staff.pre), col="blue")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.staff.pos)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.staff.pos), col="red")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.total_people.pre)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.total_people.pre), col="blue")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.total_people.pos)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.total_people.pos), col="red")
plot(correlation_matrix_input$corruption, correlation_matrix_input$trade)
plot(1,1)


# Added log + 1 (+1 to make the abline function work, might not be the right choice)
par(mfrow = c(3, 2))
plot(correlation_matrix_input$corruption, log(correlation_matrix_input$violations_weighted.staff.pre + 1))
abline(lm(correlation_matrix_input$corruption ~ log(correlation_matrix_input$violations_weighted.staff.pre + 1)), col="blue")
plot(correlation_matrix_input$corruption, log(correlation_matrix_input$violations_weighted.staff.pos + 1))
abline(lm(correlation_matrix_input$corruption ~ log(correlation_matrix_input$violations_weighted.staff.pos + 1)), col="red")
plot(correlation_matrix_input$corruption, log(correlation_matrix_input$violations_weighted.total_people.pre + 1))
abline(lm(correlation_matrix_input$corruption ~log( correlation_matrix_input$violations_weighted.total_people.pre + 1)), col="blue")
plot(correlation_matrix_input$corruption, log(correlation_matrix_input$violations_weighted.total_people.pos + 1))
abline(lm(correlation_matrix_input$corruption ~ log(correlation_matrix_input$violations_weighted.total_people.pos + 1)), col="red")
plot(correlation_matrix_input$corruption, log(correlation_matrix_input$trade + 1))
plot(1,1)
par(mfrow = c(1,1))

# I feel the best key variable is violations_weighted.staff.

# Same as above, but this data groupbed by region.
#cor_tmp_subset = FMcorrupt[c("region", "prepost", "violations", "cars_total", "staff", "spouse", "corruption")]
#Cor_by_region = aggregate(.~region+prepost, cor_tmp_subset, sum, na.rm=TRUE)
#summary(Cor_by_region)

ggplot(corrupt, aes(factor(region_name), violations, fill = factor(prepost))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")

barplot(c(mean(correlation_matrix_input$violations_weighted.staff.pre), mean(correlation_matrix_input$violations_weighted.staff.pos))
        , main = "Effectiveness of Parking Inforcement Changes on US Diplomates"
        , xlab = "Mean Parking Tickets for Mission Staff", col=c("darkblue", "red"),
        legend = c("Before Enforcement Change", "After Enforcement Change")
        )

#        , main="Car Distribution by Gears and VS",
#        xlab="Number of Gears", col=c("darkblue","red"),
#        legend = rownames(counts))


# clean up
remove(correlation_matrix_input, ignore, cor_correlations)