setwd("C:/Users/kevin/OneDrive/School/MIDS/W203 - Statistics for Data Science/Lab 1/W203_lab1_corruption")
library(car)
#library(grid)
#library(ggplot2)
library(knitr)
library(kableExtra)

load("Corrupt.Rdata")

## Correct Data Problems
#Fix majoritymulsim where value = -1, should be 0
FMcorrupt[FMcorrupt$majoritymuslim == -1 & ! is.na(FMcorrupt$majoritymuslim), "majoritymuslim"] = 0

# Add missing counties.  Reference: https://www.worldatlas.com/aatlas/ctycodes.htm
FMcorrupt[FMcorrupt$wbcode == "ARE", "country"] = "UNITED ARAB EMIRATES"
FMcorrupt[FMcorrupt$wbcode == "CAF", "country"] = "CENTRAL AFRICAN REPUBLIC"
FMcorrupt[FMcorrupt$wbcode == "CAN", "country"] = "CANADA"
FMcorrupt[FMcorrupt$wbcode == "COL", "country"] = "COLUMBIA"
FMcorrupt[FMcorrupt$wbcode == "ECU", "country"] = "ECUADOR"
FMcorrupt[FMcorrupt$wbcode == "JAM", "country"] = "JAMAICA"
FMcorrupt[FMcorrupt$wbcode == "LVA", "country"] = "LATVIA"
FMcorrupt[FMcorrupt$wbcode == "NOR", "country"] = "NORWAY"
FMcorrupt[FMcorrupt$wbcode == "PAN", "country"] = "PANAMA"
FMcorrupt[FMcorrupt$wbcode == "SWE", "country"] = "SWEDEN"
FMcorrupt[FMcorrupt$wbcode == "TUR", "country"] = "TURKEY"

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

# Rename FMcorrupt to ensure we don't use it accidentally
cor_nas = FMcorrupt
remove(FMcorrupt)

# Functions
#plot the relationship
ggplotRegression <- function (fit, title, x, y) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = title, subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                                         "Intercept =",signif(fit$coef[[1]],5 ),
                                         " Slope =",signif(fit$coef[[2]], 5),
                                         " P =",signif(summary(fit)$coef[2,4], 5)),
         x = x,
         y = y)
}
names(cor_oneline)

correlation_matrix_input = cor_oneline[, c("corruption", "violations.pre", "fines.pre", "violations.pos", "fines.pos",
                                           "staff", "spouse", "majoritymuslim", "pctmuslim", "trade", "cars_total", "cars_mission", "totaid", "gov_wage_gdp",  "distUNplz", "gdppcus1998", "totaid"
)]


#correlation_matrix_input$violations_weighted.total_people.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$total_people
#correlation_matrix_input$violations_weighted.total_people.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$total_people

# add violations treated with cars_mission
correlation_matrix_input$violations_weighted.cars_mission.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$cars_mission
correlation_matrix_input$violations_weighted.cars_mission.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$cars_mission

# add violations treated with staff
#correlation_matrix_input$violations_weighted.staff.pre = correlation_matrix_input$violations.pre/correlation_matrix_input$staff
#correlation_matrix_input$violations_weighted.staff.pos = correlation_matrix_input$violations.pos/correlation_matrix_input$staff


ignore = c("fines.pre", "violations.pos", "fines.pos",
           "spouse", "majoritymuslim", "cars_total", "cars_mission") # this are only part of the treatment (keeping violations for below)

# see what it looks like now.  
cor_correlations = round(cor(correlation_matrix_input[ , !(names(correlation_matrix_input) %in% ignore)], use = "complete.obs"), 3)
# Sorting, so strong postive correlations at begining, and negative at end.  
sort(cor_correlations[,c("corruption")][], decreasing = TRUE)

# There is a strong correlation between staff and trade (.427)
# correlation wage, gdp and corruption
# total aid and violations

head(subset(corrupt, corrupt$wbcode %in% c("CAN", "NOR")))
head(subset(cor_nas, cor_nas$wbcode %in% c("CAN", "NOR")))
head(subset(cor_nas, cor_nas$region  == 1))

par(mfrow = c(3, 2))
plot(correlation_matrix_input$corruption, correlation_matrix_input$pctmuslim)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$pctmuslim), col="blue")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.staff.pos)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.staff.pos), col="red")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.total_people.pre)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.total_people.pre), col="blue")
plot(correlation_matrix_input$corruption, correlation_matrix_input$violations_weighted.total_people.pos)
abline(lm(correlation_matrix_input$corruption ~ correlation_matrix_input$violations_weighted.total_people.pos), col="red")
plot(correlation_matrix_input$corruption, correlation_matrix_input$trade)
plot(1,1)


# Investigate the relationships between wage, gdp pc and corruption.

scatterplotMatrix( ~ corruption + gov_wage_gdp + gdppcus1998 + totaid, data = correlation_matrix_input,
                   var.labels = c("Corruption Index", "GDP Per. Capita", "Gvmnt Wage % of GDP", "Total US Aid Received"),
                   main = "Relationship of Economic Factors to Corruption", na.rm = TRUE)


lm5 <- lm(gdppcus1998~corruption, data=correlation_matrix_input)
ggplotRegression(lm5,'Relationship between corruption and GDP Per Capita','Corruption Index','GDP Per Capita (in 1998 $US)')

correlation_matrix_input$totaid.log = log(correlation_matrix_input$totaid + 1)
lm5 <- lm(totaid.log~corruption, data=correlation_matrix_input)
ggplotRegression(lm5,'Relationship between corruption and GDP Per Capita','Corruption Index','GDP Per Capita (in 1998 $US)')

