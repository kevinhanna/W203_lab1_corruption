setwd("/Users/asang/Downloads/")
library(car)
library(grid)
library(ggplot2)
library(DataExplorer)


load("Corrupt.Rdata")
summary(FMcorrupt)
dim(FMcorrupt)
colnames(FMcorrupt)  # Note objects() does not preserve order
head(FMcorrupt)



# split data in to pre and post

cor_pre = subset(FMcorrupt, prepost == "pre")
cor_pos = subset(FMcorrupt, prepost == "pos")

#data quality exploration

for (col in colnames(FMcorrupt)){
  print(length(unique(FMcorrupt[,col])))
}
colSums(is.na(FMcorrupt))
nrow(cor_pre)

#Data quality issues:

"""Data quality issues:
1) We have several extra observations that have NaN values for key variables. These data likely reflect a merging 
process wherein the desired dataset regarding parking violations and fines was merged with economic data. 
+wbcode is impacted by this. There are 213 unique values for wbcode, whereas there are only 151 countries with pre and post-legislation 
observations
++++Suggestion: We drop the N/A variables and note the number of countries for which we have no data.
2) The violations and fines data are odd. The instructions do not provide adequate background regarding these variables.
This is a particular problem because these variable are at the core of the proposed analysis.
Oddities:
+violations has 7 decimal places. This is odd because we would expect a positive integer (in the case of a single year)
or at least a figure that is recognizable as an average of some sort (in the case of multiple years)
3) The mission, staff, and spouse variables also contain oddities. For example, the entries for HKG and PRI are 0. These are the only zeros in 
those variables. Looking at the values for other variables for those two observations, some other oddities emerge. For example:
+HKG's majoritymuslim (which should be a dummy variable of either 1 or 0) is -1
++++Suggestion: Given that our research question regards the impact of the corruption on willingness to incur parking
violations and fines in NYC, we probably do not care about observations without a mission in NYC.
As such, taking all of these oddities and the low desirability of the data, I recommend excluding. where 
mission equals 0 or NaN
4) The variable gov_wage_gdp is not specified in the instructions, and no source for the data is provided. We don't know if this comes from 
an official sector body or from some potentially less rigorous institution. Furthermore, the data appears somewhat disconnected from our 
core violations and fines variables, as there are numerous NaNs for gov_wage_gdp where valid values exist for violations and fines. I do think
the concept that this variable hints at - the impact of government workers' wages on their willingness to occur fines - would be interesting to
examine in relation to the degree of decline from the pre to the post subsets. The thinking being that those government workers with lower average
salaries would be less willing to incur fines out of their own pocket. This is potentially problematic though as it is not clear that the
workers themselves would definitely be the ones paying for the fines. 
++++Suggestion: Given that we don't know the provenance of this dataset nor how it was calculated, I would not treat results using this
dataset with a high degree of confidence. Rather, I would be inclined to largely exclude this variable from the analysis
At most, it should be used with caution in it's own separate section.
5) The variable pctmuslim is among the more complete variables outside the core violations and fines dataset. We are not told the origin of this variable
either, so we have no idea of its veracity other than a common sense check. The thought behind including this variable may be that religion
and perhaps, Islam in particular, would have an impact on ethical or ethical (corrupt) behavior. It is not clear to me why Islam would be 
included and all other religions excluded. A more appropriate variable would be the percent of population which practices religion. It is 
also not clear to me that the ethics-religion association is necessarily as strong as some might think it is, though a more valid non-biased
variable could be used to test that relationship.
"""

#Make the suggested adjustments

#This accomplishes #1 and #3 exclusions
FMcorrupt <- FMcorrupt[!is.na(FMcorrupt$violations),]

#univariate exploration

#because violations is centered closely to 0 with a long right-hand tail (right skew), a log transformation may benefit our ability to compare and analyze
#the data. Though it does complicate our ability to interpret our findings
hist(log(cor_pre$violations))
hist(log(cor_pos$violations))

#same log transform
hist(log(cor_pre$fines))
hist(log(cor_pre$fines))

#this one is more debatable, but we may also benefit from log transform here as well. 
hist(log(cor_pre$staff))

hist(log(cor_pos$spouse))

#This one is also debatable
hist(log(cor_pre$gov_wage_gdp))

#I would not take the log of this one nor apply any other transforms.
hist(cor_pre$pctmuslim)