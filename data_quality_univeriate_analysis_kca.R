PreReform = subset(FMcorrupt, prepost == "pre" )

nrow(PreReform)

PostReform = subset(FMcorrupt, prepost == "pos" )

nrow(PostReform)



countriesByRegion1PreReform <- subset(PreReform, region == '1')

countriesByRegion1PostReform <- subset(PostReform, region == '1')



summary(countriesByRegion1PostReform)

countriesByRegion1PreReform



countriesByRegion2PreReform <- subset(PreReform, region == '2')

countriesByRegion2PostReform <- subset(PostReform, region == '2')

countriesByRegion2PreReform



countriesByRegion3PreReform <- subset(PreReform, region == '3')

countriesByRegion3PostReform <- subset(PostReform, region == '3')

countriesByRegion3PreReform



countriesByRegion4PreReform <- subset(PreReform, region == '4')

countriesByRegion4PostReform <- subset(PostReform, region == '4')

countriesByRegion4PreReform



countriesByRegion5PreReform <- subset(PreReform, region == '5')

countriesByRegion5PostReform <- subset(PostReform, region == '5')

countriesByRegion5PreReform



countriesByRegion6PreReform <- subset(PreReform, region == '6')

countriesByRegion6PostReform <- subset(PostReform, region == '6')

countriesByRegion6PreReform



countriesByRegion7PreReform <- subset(PreReform, region == '7')

countriesByRegion7PostReform <- subset(PostReform, region == '7')

countriesByRegion7PreReform



Region – Assigning a number to the countries belonging to a region and continent.

1 – North America [ There is no variable for this continent like others]

2 – South America

3 – Europe

4 – Asia

5 – Australia [There is no variable for this continent like others]

6 – Africa

7 – Middle East



Corruption Index – It didn’t change pre and post reform. So, I would assume Reform has no impact on the corruption level of the country.

plot(corruption~factor(wbcode), PreReform, xlab="", main="Corruption Index across countries preReform")

plot(corruption~factor(wbcode), PostReform, xlab="", main="Corruption Index across countries postReform")

Total Aid – It didn’t change pre and post reform. So, I would assume Reform has no impact on the Total Aid of the country.

plot(totaid~factor(wbcode), PreReform, xlab="", main="Corruption Index across countries preReform")

plot(totaid~factor(wbcode), PostReform, xlab="", main="Corruption Index across countries postReform")

R_Africa, R_MiddleEast, R_Europe, R_SouthAmerica, R_Asia – All these variables are grouping the countries by their region.

Country – An extension of web code but sometimes the value is empty.

distUNplz – It didn’t change pre and post reform. So, I would assume Reform has no impact on the this variable of the country.

plot(distUNplz~factor(wbcode), PreReform, xlab="", main="Corruption Index across countries preReform")

plot(distUNplz~factor(wbcode), PostReform, xlab="", main="Corruption Index across countries postReform")