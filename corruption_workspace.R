setwd("C:/Users/kevin/OneDrive/School/MIDS/W203 - Statistics for Data Science/Lab 1/W203_lab1_corruption")
library(car)
library(grid)
library(ggplot2)


load("Corrupt.Rdata")
summary(FMcorrupt)
dim(FMcorrupt)
colnames(FMcorrupt)  # Note objects() does not preserve order
head(FMcorrupt)



# split data in to pre and post

cor_pre = subset(FMcorrupt, prepost == "pre")
cor_pos = subset(FMcorrupt, prepost == "pos")

head(cor_pre)
head(cor_pos)

#merge to same line and append .pre or .pos to variables
cor_merged = merge(cor_pre, cor_pos, by = "wbcode", suffixes = c(".pre", ".pos"))
summary (cor_merged)


# Grouped bar graph, too many variables for it to be readable
ggplot(FMcorrupt, aes(factor(wbcode), violations, fill = factor(prepost))) + 
         geom_bar(stat="identity", position = "dodge") +
         scale_fill_brewer(palette = "Set1")


# Same as above, but this data groupbed by region.
cor_tmp_subset = FMcorrupt[c("region", "prepost", "violations", "cars_total", "staff", "spouse", "corruption")]
Cor_by_region = aggregate(.~region+prepost, cor_tmp_subset, sum, na.rm=TRUE)
summary(Cor_by_region)

ggplot(Cor_by_region, aes(factor(region), violations, fill = factor(prepost))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")


# Add boxplots to a scatterplot (custom function that got more complicated that it should have, 
# need to execute the block of code down to "}" before "enhanced_scatter" lines below)
enhanced_scatter = function(x, y, xlab = "Violations", ylab = "", main_title = "") {
  
  if (!typeof(y) %in% c("double", "integer")) {
    print(paste(typeof(y), "is not supported"))
    #cat("Unable to plot ", ylab, " it is of type ", typeof(y))
    return()
  }
  
  grid.newpage()
  par(fig=c(0,0.8,0,0.8), new=TRUE)
  plot(x, y, xlab=xlab, ylab=ylab)
  abline(lm(x ~ y))
  par(fig=c(0,0.8,0.55,1), new=TRUE)
  boxplot(x, horizontal=TRUE, axes=FALSE)
  par(fig=c(0.65,1,0,0.8),new=TRUE)
  boxplot(y, axes=FALSE)
  mtext(main_title, side=3, outer=TRUE, line=-3)
}

# scatter plot of corruption per staff both pre and post for each country
cor_pre = subset(FMcorrupt, prepost == "pre")
cor_pos = subset(FMcorrupt, prepost == "pos")

enhanced_scatter((cor_pre$violations/cor_pre$staff), cor_pre$corruption, "Violations/Staff", "Corruption", 
                 paste("Before 2002 Change - Corelation:", cor((cor_pre$violations/cor_pre$staff), cor_pre$corruption, use = "complete.obs")))
enhanced_scatter((cor_pos$violations/cor_pre$staff), cor_pos$corruption, "Violations/Staff", "Corruption", 
                 paste("After 2002 Change - Corelation:", cor((cor_pos$violations/cor_pre$staff), cor_pos$corruption, use = "complete.obs")))

# plain scatter plots
plot((cor_pre$violations/cor_pre$staff), cor_pre$corruption)
abline(lm((cor_pre$violations/cor_pre$staff ~ cor_pre$corruption)), col="blue")

plot((cor_pos$violations/cor_pos$staff), cor_pos$corruption)
abline(lm((cor_pos$violations/cor_pos$staff ~ cor_pos$corruption)), col="red")



# plain scatter plots
plot(log((cor_pre$violations/cor_pre$staff) + 10), cor_pre$corruption)
abline(lm(log((cor_pre$violations/cor_pre$staff) + 10) ~ cor_pre$corruption), col="blue")

plot(log((cor_pos$violations/cor_pos$staff) + 1), cor_pos$corruption)
abline(lm(log((cor_pos$violations/cor_pos$staff) + 1) ~ cor_pos$corruption), col="red")


# scatter plot of corruption per staff both pre and post for each region

cor_pre_reg = subset(Cor_by_region, prepost == "pre")
cor_pos_reg = subset(Cor_by_region, prepost == "pos")

enhanced_scatter((cor_pre_reg$violations/cor_pre_reg$staff), cor_pre_reg$corruption, "Violations/Staff", "Corruption", 
                 paste("Before 2002 Change - Corelation:", cor((cor_pre_reg$violations/cor_pre_reg$staff), cor_pre_reg$corruption, use = "complete.obs")))
enhanced_scatter((cor_pos_reg$violations/cor_pre_reg$staff), cor_pos_reg$corruption, "Violations/Staff", "Corruption", 
                 paste("After 2002 Change - Corelation:", cor((cor_pos_reg$violations/cor_pre_reg$staff), cor_pos_reg$corruption, use = "complete.obs")))


# Scatter Plot Matrix for regions before 2002
scatterplotMatrix( ~ corruption + violations + cars_total + staff, data = cor_pre_reg,
                   main = "Scatterplot Matrix for Regions Before 2002", na.rm = TRUE)

# Scatter Plot Matrix for regions after 2002
scatterplotMatrix( ~ corruption + violations + cars_total + staff, data = cor_pos_reg,
                   main = "Scatterplot Matrix for Regions After 2002", na.rm = TRUE)


