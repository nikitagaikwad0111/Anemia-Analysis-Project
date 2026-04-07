#  READ ONLY WOMEN data
#  ____________________
#  _____________________


rm(list=ls()) 
w=read.delim("D:/NFHS Data/NFHSIV/IAIR74FL.DAT")
n=nrow(w)



v = c("caseid", "v009", "v010","v012", "v024", "v025","v113", "v116", "v137", "v149","v190","v201","v218","v219","v404","v409","v409a","v410","v410a","v411","v411a","v412","v412a","v445","v454","v457","v458","v472l","v472n","v472o","v472p","v472q","v472r","v481","bord","b4","b8","h42","hw57","hw58","m45" )

l=c(1,56,58,66,99,101,135,140,166,174,220,970,1001,1003,2360,2367,2368,2369,2370,2371,2372,2373,2374,2459,2477,2482,2483,2537,2539,2540,2541,2542,2543,2571,270,530,690,3862,4384,4390,1819)

k=c(15,2,4,2,2,1,2,2,2,1,1,2,2,2,1,1,1,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,2,1,2,1,1,1,1 )

M=matrix(nrow=n,ncol=length(k)) 
# D=D[[1]]
for(r in 1:n)
{
  D=strsplit(as.character(w[r,]),split = "")
  D=D[[1]]
  j=1
  for(i in 1:length(k))
  {
    s=l[i]:(l[i]+k[i]-1)
    M[r,i]=paste(D[s],collapse = "")
  }
}

View(M)

write.csv(M,"C:/NFHS data/NFHSIV/IAIR74FL/IAIR74FL.csv")



########################
library(dplyr)
M1=read.csv("C:/NFHS data/NFHSIV/IAIR74FL/IAIR74FL.csv",stringsAsFactors = FALSE)
View(M1)
write.csv(M1,"C:/NFHS data/NFHSIV/IAIR74FL/IAIR74FL2.csv")

#############-----------------------------------------------------------------------------------------------------------


M1=read.csv("C:/Users/NIKITA/OneDrive/Desktop/MSc PROJECT/IAIR74FL002.csv")
M1[M1==""]=NA
head(M1)
names(M1)


###########----------------------------------------------------------------------------------------------------------------------
#1) Respondents year of birth
# Frequency Table
table(M1$Respondent.s.year.of.birth)
# Proportion Table
prop.table(table(M1$Respondent.s.year.of.birth))*100
# Bar plot
barplot(table(M1$Respondent.s.year.of.birth),main="Respondent.s.year.of.birth",xlab="year of birth",ylab="count",las=1)
##Conclusion - There are more people born in 1980 and 1990 as compared to the 1960 and early 1970.This suggests that peoples those born in 1980 and 2000 are more represented in sample.

# Pie chart
pie(table(M1$Respondent.s.year.of.birth),main="Respondent.s.year.of.birth")
#cross tabulation (Contingency table)
table(M1$Respondent.s.year.of.birth,M1$Respondent.s.month.of.birth)
# Chi-square test of independence
chisq.test(M1$Respondent.s.year.of.birth,M1$Respondent.s.month.of.birth)
##Conclusion -  Here, P_value is very small(less than 0.05). Hence, we reject H0(Null Hypothesis).This means that there is statistically significant relationship betweeen the year of birth and the month of birth of the respondents i.e. the distibution the birth months varies across different birth years.

# Mosaic plot
mosaicplot(table(M1$Respondent.s.year.of.birth,M1$Respondent.s.month.of.birth),main="Mosaic plot for respondent birth month and birth year",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Respondent.s.year.of.birth)
##Conclusion - The most frequent birth year among the respondents dataset is 1990. This means that more respondents born in 1990 than any other year.

# Summary statistics for categorical Variable(Respondent.s.year.of.birth)
summary(M1$Respondent.s.year.of.birth)

####______________________________________________________________________________________________________________________


#2) Respondents Current age
# Frequency Table
table(M1$Respondent.s.current.age)
##Conclusion - In the dataset, the spread of respondents across various ages like 18,25,30 have higher frequencies. The least common ages in the dataset are 49(10460 respondents) and 45(10936 respondents).
# Proportion Table
prop.table(table(M1$Respondent.s.current.age))*100
# Bar plot
barplot(table(M1$Respondent.s.current.age),main="Respondent.s.current.age",xlab="Current Age",ylab="count",las=1,col=rainbow((M1$Respondent.s.current.age)))
legend("topright",legend=names(table(M1$Respondent.s.current.age)),fill=rainbow(M1$Respondent.s.current.age))
# Pie chart
pie(table(M1$Respondent.s.current.age),main="Current Age",col=rainbow(M1$Respondent.s.current.age))
legend("topright",legend=names(table(M1$Respondent.s.current.age)),fill=rainbow(M1$Respondent.s.current.age))
#cross tabulaton (Contingency table)
table(M1$Respondent.s.current.age,M1$Respondent.s.month.of.birth)
# Chi-square test of independence
chisq.test(M1$Respondent.s.current.age,M1$Respondent.s.year.of.birth)
##Conclusion - Here, P_value is so small (<0.05), which means that there is statistically significant associatoin between a respondents current age and their birth month. Respondents ages is not independent of the birth month.
# Mosaic plot
mosaicplot(table(M1$Respondent.s.current.age,M1$Respondent.s.month.of.birth),main="Mosaic plot for respondent birth month and current age",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Respondent.s.current.age)

# Summary statistics for categorical Variable(Respondent.s.year.of.birth)
summary(M1$Respondent.s.current.age)

##density plot
age_density=density(M1$Respondent.s.current.age,na.rm=TRUE)
plot(age_density,main="Density plot of respondents age",xlab="Age in years",ylab="Density",col="blue",lwd=1)


###-------------------------------------------------------------------------------------------------------------------------------------------



#3) Type of place of residence(Urban=1 and Rural=2)
# Frequency Table
table(M1$Type.of.place.of.residence)
##Conclusion - The high frequency of Place of residence of the respondents is 2 i.e. Rural
# Proportion Table
prop.table(table(M1$Type.of.place.of.residence))*100
# Bar plot
barplot(table(M1$Type.of.place.of.residence),main="Type.of.place.of.residence",xlab="rural and urban",ylab="count",las=1)

# Pie chart
pie(table(M1$Type.of.place.of.residence),main="Type.of.place.of.residence")
#cross tabulaton (Contingency table)
table(M1$Respondent.s.current.age,M1$Type.of.place.of.residence)
# Chi-square test of independence
chisq.test(M1$Respondent.s.current.age,M1$Type.of.place.of.residence)
## Conclusion - Here, P_value is less than 0.05. Hence, there is statistically significant association between respondents age and place of residence.
# Mosaic plot
mosaicplot(table(M1$Respondent.s.current.age,M1$Type.of.place.of.residence),main="Mosaic plot for Type.of.place.of.residence and current age",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Type.of.place.of.residence) #Most of the respondents from the Rural area.
# Summary statistics for categorical Variable(Respondent.s.year.of.birth)
summary(M1$Type.of.place.of.residence)



#4) Sources of drinking water
# Frequency Table
table(M1$Source.of.drinking.water)
##Conclusion - Most of the respondents source of drinking water(268565 respondents) is 21 i.e. Tube well or borehole. And second highest there are 115036 respondents source of drinking water is 11 i.e. Piped into dwelling.
# Proportion Table
prop.table(table(M1$Source.of.drinking.water))*100
# Bar plot
barplot(table(M1$Source.of.drinking.water),main="Source.of.drinking.water",xlab="Sources",ylab="count",las=1,col=rainbow(M1$Source.of.drinking.water))
legend("topright",legend=names(table(M1$Source.of.drinking.water)),fill=rainbow(M1$Source.of.drinking.water))
# Pie chart
pie(table(M1$Source.of.drinking.water),main="Source.of.drinking.water",col=rainbow(M1$Source.of.drinking.water))
legend("topright",legend=names(table(M1$Source.of.drinking.water)),fill=rainbow(M1$Source.of.drinking.water))
#cross tabulaton (Contingency table)
table(M1$Source.of.drinking.water,M1$Type.of.place.of.residence)
# Chi-square test of independence
chisq.test(M1$Source.of.drinking.water,M1$Type.of.place.of.residence)
# Mosaic plot
mosaicplot(table(M1$Source.of.drinking.water,M1$Type.of.place.of.residence),main="Mosaic plot for Type.of.place.of.residence and Source.of.drinking.water",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Source.of.drinking.water)  #There are 268565 respondents source of drinking water is 21 i.e. Tube well or borehole.
# Summary statistics for categorical Variable(Source.of.drinking.waterd)
summary(M1$Source.of.drinking.water)




#5) Types of toilet facilities
# Frequency Table
table(M1$Type.of.toilet.facility)
##Conclusion _ There are 230780 respondents have flush to septic tank(12) toilet facility but there are 251917 respondents don't have toilet facility/bush/field(31). There are 69161 respondents have flush to pit latrine toilet facility.
# Proportion Table
prop.table(table(M1$Type.of.toilet.facility))*100
# Bar plot
barplot(table(M1$Type.of.toilet.facility),main="Type.of.toilet.facility",xlab="Types of Toilets",ylab="count",las=1,col=rainbow(M1$Type.of.toilet.facility))
legend("topright",legend=names(table(M1$Type.of.toilet.facility)),fill=rainbow(M1$Type.of.toilet.facility))
# Pie chart
pie(table(M1$Type.of.toilet.facility),main="Type.of.toilet.facility",col=rainbow(M1$Type.of.toilet.facility))
legend("topright",legend=names(table(M1$Type.of.toilet.facility)),fill=rainbow(M1$Type.of.toilet.facility))
#cross tabulaton (Contingency table)
table(M1$Type.of.toilet.facility,M1$Type.of.place.of.residence)
# Chi-square test of independence
chisq.test(M1$Type.of.toilet.facility,M1$Type.of.place.of.residence)
# Mosaic plot
mosaicplot(table(M1$Type.of.toilet.facility,M1$Type.of.place.of.residence),main="Mosaic plot for Type.of.place.of.residence and Type.of.toilet.facility",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Type.of.toilet.facility) # There are 251917 respondents do not have toilet facilities/bush/field.
# Summary statistics for categorical Variable(RNumber.of.children.5.and.under.in.household..de.jure.espondent.s.year.of.birth)
summary(M1$Type.of.toilet.facility)


#6) Number of children 5 and under in household de.jure.
# Frequency Table
table(M1$Number.of.children.5.and.under.in.household..de.jure.)
# Proportion Table
prop.table(table(M1$Number.of.children.5.and.under.in.household..de.jure.))*100
# Bar plot
barplot(table(M1$Number.of.children.5.and.under.in.household..de.jure.),main="Number.of.children.5.and.under.in.household..de.jure.",xlab="Number.of.children.5.and.under.in.household..de.jure.",ylab="count",las=1,col=rainbow(M1$Number.of.children.5.and.under.in.household..de.jure.))
legend("topright",legend=names(table(M1$Number.of.children.5.and.under.in.household..de.jure.)),fill=rainbow(M1$Number.of.children.5.and.under.in.household..de.jure.))
# Pie chart
pie(table(M1$Number.of.children.5.and.under.in.household..de.jure.),main="Number.of.children.5.and.under.in.household..de.jure.",col=rainbow(M1$Number.of.children.5.and.under.in.household..de.jure.))
legend("topright",legend=names(table(M1$Number.of.children.5.and.under.in.household..de.jure.)),fill=rainbow(M1$Number.of.children.5.and.under.in.household..de.jure.))
#cross tabulaton (Contingency table)
table(M1$Number.of.children.5.and.under.in.household..de.jure.,M1$Type.of.place.of.residence)
# Chi-square test of independence
chisq.test(M1$Number.of.children.5.and.under.in.household..de.jure.,M1$Type.of.place.of.residence)
# Mosaic plot
mosaicplot(table(M1$Number.of.children.5.and.under.in.household..de.jure.,M1$Type.of.place.of.residence),main="Mosaic plot for Type.of.place.of.residence and Number.of.children.5.and.under.in.household..de.jure.",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Number.of.children.5.and.under.in.household..de.jure.)
# Summary statistics for categorical Variable(Number.of.children.5.and.under.in.household..de.jure.)
summary(M1$Number.of.children.5.and.under.in.household..de.jure.)



#7) Educational attainment
# Frequency Table
table(M1$Educational.attainment)
# Proportion Table
prop.table(table(M1$Educational.attainment))*100
# Bar plot
barplot(table(M1$Educational.attainment),main="Educational.attainment",xlab="Educational.attainment",ylab="count",las=1,col=rainbow(M1$Educational.attainment))
legend("topright",legend=names(table(M1$Educational.attainment)),fill=rainbow(M1$Educational.attainment))
# Pie chart
pie(table(M1$Educational.attainment),main="Educational.attainment",col=rainbow(M1$Educational.attainment))
legend("topright",legend=names(table(M1$Educational.attainment)),fill=rainbow(M1$Educational.attainment))
#cross tabulaton (Contingency table)
table(M1$Educational.attainment,M1$Type.of.place.of.residence)
# Chi-square test of independence
chisq.test(M1$Educational.attainment,M1$Type.of.place.of.residence)
# Mosaic plot
mosaicplot(table(M1$Educational.attainment,M1$Type.of.place.of.residence),main="Mosaic plot for Type.of.place.of.residence and Educational.attainment",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Educational.attainment)
# Summary statistics for categorical Variable(Educational.attainment)
summary(M1$Educational.attainment)


#8) Wealth Index
# Frequency Table
table(M1$Wealth.index)
# Proportion Table
prop.table(table(M1$Wealth.index))*100
# Bar plot
barplot(table(M1$Wealth.index),main="Wealth.index",xlab="Wealth.index",ylab="count",las=1,col=rainbow(M1$Wealth.index))
legend("topright",legend=names(table(M1$Wealth.index)),fill=rainbow(M1$Wealth.index))
# Pie chart
pie(table(M1$Wealth.index),main="Wealth.index",col=rainbow(M1$Wealth.index))
legend("topright",legend=names(table(M1$Wealth.index)),fill=rainbow(M1$Wealth.index))
#cross tabulaton (Contingency table)
table(M1$Wealth.index,M1$Educational.attainment)
# Chi-square test of independence
chisq.test(M1$Wealth.index,M1$Educational.attainment)
# Mosaic plot
mosaicplot(table(M1$Educational.attainment,M1$Wealth.index),main="Mosaic plot for Wealth.index and Educational.attainment",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Wealth.index)
# Summary statistics for categorical Variable(Wealth.index)
summary(M1$Wealth.index)



#9) Total.children.ever.born
# Frequency Table
table(M1$Total.children.ever.born)
# Proportion Table
prop.table(table(M1$Total.children.ever.born))*100
# Bar plot
barplot(table(M1$Total.children.ever.born),main="Total.children.ever.born",xlab="Total.children.ever.born",ylab="count",las=1,col=rainbow(M1$Total.children.ever.born))
legend("topright",legend=names(table(M1$Total.children.ever.born)),fill=rainbow(M1$Total.children.ever.born))
# Pie chart
pie(table(M1$Total.children.ever.born),main="Total.children.ever.born",col=rainbow(M1$Total.children.ever.born))
legend("topright",legend=names(table(M1$Total.children.ever.born)),fill=rainbow(M1$Total.children.ever.born))
#cross tabulation (Contingency table)
table(M1$Total.children.ever.born,M1$Educational.attainment)
# Chi-square test of independence
chisq.test(M1$Total.children.ever.born,M1$Educational.attainment)
# Mosaic plot
mosaicplot(table(M1$Educational.attainment,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Educational.attainment",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Total.children.ever.born)
# Summary statistics for categorical Variable(Total.children.ever.born)
summary(M1$Total.children.ever.born)

##density plot
children_density=density(M1$Total.children.ever.born,na.rm=TRUE)
plot(children_density,main="Density plot of total children ever born",xlab="Number of children",ylab="Density",col="red",lwd=1)



#10) Number.of.living.children
# Frequency Table
table(M1$Number.of.living.children)
# Proportion Table
prop.table(table(M1$Number.of.living.children))*100
# Bar plot
barplot(table(M1$Number.of.living.children),main="Number.of.living.children",xlab="Number.of.living.children",ylab="count",las=1,col=rainbow(M1$Number.of.living.children))
legend("topright",legend=names(table(M1$Number.of.living.children)),fill=rainbow(M1$Number.of.living.children))
# Pie chart
pie(table(M1$Number.of.living.children),main="Number.of.living.children",col=rainbow(M1$Number.of.living.children))
legend("topright",legend=names(table(M1$Number.of.living.children)),fill=rainbow(M1$Number.of.living.children))
#cross tabulation (Contingency table)
table(M1$Number.of.living.children,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Number.of.living.children,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Number.of.living.children,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Educational.attainment",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Number.of.living.children)
# Summary statistics for categorical Variable(Number.of.living.children)
summary(M1$Number.of.living.children)


##density plot
livingchildren_density=density(M1$Number.of.living.children,na.rm=TRUE)
plot(livingchildren_density,main="Density plot of number of living children",xlab="Number of children",ylab="Density",col="green",lwd=1)




#11) Living.children...current.pregnancy
# Frequency Table
table(M1$Living.children...current.pregnancy)
# Proportion Table
prop.table(table(M1$Living.children...current.pregnancy))*100
# Bar plot
barplot(table(M1$Living.children...current.pregnancy),main="Living.children...current.pregnancy",xlab="Living.children...current.pregnancy",ylab="count",las=1,col=rainbow(M1$Living.children...current.pregnancy))
legend("topright",legend=names(table(M1$Living.children...current.pregnancy)),fill=rainbow(M1$Living.children...current.pregnancy))
# Pie chart
pie(table(M1$Living.children...current.pregnancy),main="Living.children...current.pregnancy",col=rainbow(M1$Living.children...current.pregnancy))
legend("topright",legend=names(table(M1$Living.children...current.pregnancy)),fill=rainbow(M1$Living.children...current.pregnancy))
#cross tabulaton (Contingency table)
table(M1$Living.children...current.pregnancy,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Living.children...current.pregnancy,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Living.children...current.pregnancy,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Living.children...current.pregnancy",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Living.children...current.pregnancy)
# Summary statistics for categorical Variable(Living.children...current.pregnancy)
summary(M1$Living.children...current.pregnancy)


#12) Currently.breastfeeding
# Frequency Table
table(M1$Currently.breastfeeding)
# Proportion Table
prop.table(table(M1$Currently.breastfeeding))*100
# Bar plot
barplot(table(M1$Currently.breastfeeding),main="Currently.breastfeeding",xlab="Currently.breastfeeding",ylab="count",las=1,col=rainbow(M1$Currently.breastfeeding))
legend("topright",legend=names(table(M1$Currently.breastfeeding)),fill=rainbow(M1$Currently.breastfeeding))
# Pie chart
pie(table(M1$Currently.breastfeeding),main="Currently.breastfeeding",col=rainbow(M1$Currently.breastfeeding))
legend("topright",legend=names(table(M1$Currently.breastfeeding)),fill=rainbow(M1$Currently.breastfeeding))
#cross tabulaton (Contingency table)
table(M1$Currently.breastfeeding,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Currently.breastfeeding,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Currently.breastfeeding,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Currently.breastfeeding",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Currently.breastfeeding)
# Summary statistics for categorical Variable(Currently.breastfeeding)
summary(M1$Currently.breastfeeding)



#13) Body.Mass.Index
# Frequency Table
table(M1$Body.Mass.Index)
# Proportion Table
prop.table(table(M1$Body.Mass.Index))*100
# Bar plot
barplot(table(M1$Body.Mass.Index),main="Body.Mass.Index",xlab="Body.Mass.Index",ylab="count",las=1,col=rainbow(M1$Body.Mass.Index))
legend("topright",legend=names(table(M1$Body.Mass.Index)),fill=rainbow(M1$Body.Mass.Index))
# Pie chart
pie(table(M1$Body.Mass.Index),main="Body.Mass.Index",col=rainbow(M1$Body.Mass.Index))
legend("topright",legend=names(table(M1$Body.Mass.Index)),fill=rainbow(M1$Body.Mass.Index))
#cross tabulaton (Contingency table)
table(M1$Body.Mass.Index,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Body.Mass.Index,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Body.Mass.Index,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Body.Mass.Index",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Body.Mass.Index)
# Summary statistics for categorical Variable(Body.Mass.Index)
summary(M1$Body.Mass.Index)

##density plot
bmi_density=density(M1$Body.Mass.Index,na.rm=TRUE)
plot(bmi_density,main="Density plot of BMI",xlab="Number of children",ylab="Density",col="green",lwd=1)




#14) Currently.pregnant..from.household.questionnaire.
# Frequency Table
table(M1$Currently.pregnant..from.household.questionnaire.)
# Proportion Table
prop.table(table(M1$Currently.pregnant..from.household.questionnaire.))*100
# Bar plot
barplot(table(M1$Currently.pregnant..from.household.questionnaire.),main="Currently.pregnant..from.household.questionnaire.",xlab="Currently.pregnant..from.household.questionnaire.",ylab="count",las=1,col=rainbow(M1$Currently.pregnant..from.household.questionnaire.))
legend("topright",legend=names(table(M1$Currently.pregnant..from.household.questionnaire.)),fill=rainbow(M1$Currently.pregnant..from.household.questionnaire.))
# Pie chart
pie(table(M1$Currently.pregnant..from.household.questionnaire.),main="Currently.pregnant..from.household.questionnaire.",col=rainbow(M1$Currently.pregnant..from.household.questionnaire.))
legend("topright",legend=names(table(M1$Currently.pregnant..from.household.questionnaire.)),fill=rainbow(M1$Currently.pregnant..from.household.questionnaire.))
#cross tabulation (Contingency table)
table(M1$Currently.pregnant..from.household.questionnaire.,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Currently.pregnant..from.household.questionnaire.,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Currently.pregnant..from.household.questionnaire.,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Currently.pregnant..from.household.questionnaire.",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Currently.pregnant..from.household.questionnaire.)
# Summary statistics for categorical Variable(Currently.pregnant..from.household.questionnaire.)
summary(M1$Currently.pregnant..from.household.questionnaire.)




#13) Anemia.level
# Frequency Table
table(M1$Anemia.level)
# Proportion Table
prop.table(table(M1$Anemia.level))*100
# Bar plot
barplot(table(M1$Anemia.level),main="Anemia.level",xlab="Severity Level",ylab="count",las=1,col=c("red", "orange", "yellow", "green"))
legend("topright",legend = c("Severe", "Moderate", "Mild", "Non-anemic"),fill=c("red", "orange", "yellow", "green"))
# Pie chart
pie(table(M1$Anemia.level),main="Anemia.level",col=c("red", "orange", "yellow", "green"))
legend("topright",legend = c("Severe", "Moderate", "Mild", "Non-anemic"),fill=c("red", "orange", "yellow", "green"))
#cross tabulation (Contingency table)
table(M1$Anemia.level,M1$Total.children.ever.born)
# Chi-square test of independence
chisq.test(M1$Anemia.level,M1$Total.children.ever.born)
# Mosaic plot
mosaicplot(table(M1$Anemia.level,M1$Total.children.ever.born),main="Mosaic plot for Total.children.ever.born and Anemia.level",shade=TRUE)
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Get the mode of the Gender column
getmode(M1$Anemia.level)
# Summary statistics for categorical Variable(Anemia.level)
summary(M1$Anemia.level)


#####----------------------------------------------------------------------------------------------------------------------------------------


##PLOT FOR ANEMIC vs NON-ANEMIC

##Remove Missing individuals data from Anemia levels
m1= M1[M1$Anemia.level!=9,];m1

m1$Anemia.level= ifelse(m1$Anemia.level==4, "Non-Anemic", "Anemic")

counts= table(m1$Anemia.level)

percent= prop.table(counts)*100

pie(percent, main="Percentage of People with and without Anemia", col=c("salmon", "skyblue"))


#### for Anemia prevalence by Type of Residence

m1$Type.of.place.of.residence= factor(m1$Type.of.place.of.residence, levels= c(1,2), labels= c("Urban", "Rural"))


table_data= table(m1$Type.of.place.of.residence, m1$Anemia.level)

percent_data= prop.table(table_data,1)*100

barplot(t(percent_data),beside= TRUE, col=c("salmon","skyblue"), legend= TRUE, main= "Anemia Prevalence by Place of Residence", xlab= "Place of Residence", ylab= "Percentage")


##### Age Groups############------------------------------------------------------------------------------------------------------------------

m1$Respondent.s.current.age= cut(m1$Respondent.s.current.age, breaks= c(0,5,15,25,40,60), labels= c("0-5", "6-15", "16-25", "26-40", "40+"), right= TRUE)

tab_data= table(m1$Respondent.s.current.age, m1$Anemia.level)

percent_data= prop.table(tab_data,1)*100

par(mar = c(4, 4, 4, 2))
barplot(t(percent_data), beside = TRUE, col = c("salmon", "skyblue"), names.arg = c("0-5", "6-15", "16-25", "26-40", "40+"), xlab = "Age Group", ylab = "Percentage", main = "Anemia Distribution by Age Group", legend.text = c("Anemic", "Non-Anemic"), args.legend = list(x = "topright"), xlim = c(1, 20))


#############################----------------------------------------------------------------------------------------------------------------------------------------------------------

par(mar=c(8,4,4,10))
tab= table(M1$Educational.attainment, M1$Anemia.level)
prop_tab=prop.table(tab,1)


prop_tab=t(prop_tab)
barplot(prop_tab,col=c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),xlab="Education Level",ylab="Proportion", main="Education vs Anemia",names.arg=c("No Edu", "Inc Prim", "Comp Prim", "Inc Sec", "Comp Sec", "Higher"), las=1)
legend("topright",legend = c("Severe", "Moderate", "Mild", "Non-anemic"),fill = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),bty = "n")

###########3---------------------------------------------------------------------



par(mar=c(8,4,4,10))
tab= table(M1$Type.of.place.of.residence, M1$Anemia.level)
prop_tab=prop.table(tab,1)


prop_tab=t(prop_tab)
barplot(prop_tab,col=c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),xlab="Type of Residence",ylab="Proportion", main="Residence vs Anemia",names.arg=c("Urban", "Rural"), las=1)

legend("topright",legend = c("Severe", "Moderate", "Mild", "Non-anemic"),fill = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),bty = "n")


###################################--------------------------------------------------------------------------------------------------------------------------------------


#1) Chi-Square test if independence
chisq.test(M1$State,M1$Type.of.place.of.residence)
chisq.test(M1$Type.of.place.of.residence,M1$Educational.attainment)
chisq.test(M1$Wealth.index,M1$Educational.attainment)
chisq.test(M1$Type.of.place.of.residence,M1$Educational.attainment)
chisq.test(M1$Type.of.place.of.residence,M1$Wealth.index)
chisq.test(M1$Wealth.index,M1$Type.of.toilet.facility)
chisq.test(M1$Anemia.level,M1$Educational.attainment)
chisq.test(M1$Type.of.place.of.residence,M1$Anemia.level)
chisq.test(M1$Type.of.place.of.residence,M1$Sex.of.child)
chisq.test(M1$Educational.attainment,M1$Sex.of.child)
chisq.test(M1$Anemia.level.1,M1$During.pregnancy..given.or.bought.iron.tablets.syrup)
chisq.test(M1$Anemia.level,M1$Taking.iron.pills..sprinkles.or.syrup)


######-----------------------------------------------------------------------------------------------------------------------------


#2) Cross tabulation (Contingency Table)
table(M1$State,M1$Type.of.place.of.residence)
table(M1$Type.of.place.of.residence,M1$Educational.attainment)
table(M1$Wealth.index,M1$Educational.attainment)
table(M1$Type.of.place.of.residence,M1$Educational.attainment)
table(M1$Type.of.place.of.residence,M1$Wealth.index)
table(M1$Wealth.index,M1$Type.of.toilet.facility)
table(M1$Anemia.level,M1$Educational.attainment)
table(M1$Type.of.place.of.residence,M1$Anemia.level)
table(M1$Type.of.place.of.residence,M1$Sex.of.child)
table(M1$Educational.attainment,M1$Sex.of.child)
table(M1$Anemia.level.1,M1$During.pregnancy..given.or.bought.iron.tablets.syrup)
table(M1$Anemia.level,M1$Taking.iron.pills..sprinkles.or.syrup)


####-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#3) Mosaic plots
mosaicplot(table(M1$State,M1$Type.of.place.of.residence),main="mosaic plot of state and place of residence",shade=TRUE)
mosaicplot(table(M1$Type.of.place.of.residence,M1$Educational.attainment),main="Mosaic plot of place of residence and educational attainment",shade=TRUE)
mosaicplot(table(M1$Wealth.index,M1$Educational.attainment),main="mosaic plot of wealth index and educational attainment",shade=TRUE)
mosaicplot(table(M1$Type.of.place.of.residence,M1$Educational.attainment),main="mosaic plot of place of residence and educational attainment",shade=TRUE)
mosaicplot(table(M1$Type.of.place.of.residence,M1$Wealth.index),main="mosaic plot of place of residence and wealth index",shade=TRUE)
mosaicplot(table(M1$Wealth.index,M1$Type.of.toilet.facility),main="mosaic plot of wealth indexx and type of toilet facility",shade=TRUE)
mosaicplot(table(M1$Anemia.level,M1$Educational.attainment),main="mosaic plot of anemia level and educational attainment",shade=TRUE)
mosaicplot(table(M1$Type.of.place.of.residence,M1$Anemia.level),main="mosaic plot of place of residence and anemia level",shade=TRUE)
mosaicplot(table(M1$Type.of.place.of.residence,M1$Sex.of.child),main="mosaic plot of place of residence and sex of child",shade = TRUE)
mosaicplot(table(M1$Educational.attainment,M1$Sex.of.child),main="education attainment and sex of child",shade = TRUE)
mosaicplot(table(M1$Anemia.level.1,M1$During.pregnancy..given.or.bought.iron.tablets.syrup),main="mosaic plot of anemia level.1 and during pregency bought iron tablets or syrup",shade=TRUE)
mosaicplot(table(M1$Anemia.level,M1$Taking.iron.pills..sprinkles.or.syrup),main="mosaic plot of anemia level.1 and taking iron pills sprinkles or syrup",shade=TRUE)

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Chi-Square test(State VS Other Variables)
# List of variables for ANOVA and Chi-Square tests
anova_vars <- c("Educational.attainment", "Wealth.index", "Type.of.place.of.residence",
                "Source.of.drinking.water", "Number.of.children.5.and.under.in.household..de.jure.",
                "Respondent.s.current.age", "Currently.breastfeeding","State")

# Perform Chi-Square test for each variable
for (var in anova_vars) {
  contingency_table <- table(M1$Anemia.level, M1[[var]])
  print(paste("Chi-Square test for Anemia Level in Women and", var))
  print(chisq.test(contingency_table))
  cat("\n")
}



#Chi-Square teste (Anemia Level VS Other Variables)
# List of variables for ANOVA and Chi-Square tests
anova_vars <- c("Educational.attainment", "Wealth.index", "Type.of.place.of.residence",
                "Source.of.drinking.water", "Number.of.children.5.and.under.in.household..de.jure.",
                "Respondent.s.current.age", "Currently.breastfeeding")

# Perform Chi-Square test for each variable
for (var in anova_vars) {
  contingency_table <- table(M1$Anemia.level, M1[[var]])
  print(paste("Chi-Square test for Anemia Level and", var))
  print(chisq.test(contingency_table))
  cat("\n")
}


#Chi-Square teste (Sex of child VS Other Variables)
# List of variables for ANOVA and Chi-Square tests
anova_vars <- c("Educational.attainment", "Wealth.index", "Type.of.place.of.residence",
                "Source.of.drinking.water", "Number.of.children.5.and.under.in.household..de.jure.",
                "Respondent.s.current.age", "Currently.breastfeeding")

# Perform Chi-Square test for each variable
for (var in anova_vars) {
  contingency_table <- table(M1$Sex.of.child, M1[[var]])
  print(paste("Chi-Square test for Sex of Child and", var))
  print(chisq.test(contingency_table))
  cat("\n")
}


##Chi-Square test for Education Attainment VS All variables
All_variables=c("Type.of.place.of.residence","State","Wealth.index","Total.children.ever.born","Respondent.s.current.age","Type.of.toilet.facility","Sex.of.child","Anemia.level")
for(var in All_variables){
  print(paste("Chi-Square test of Education attainment and",var))
  Chisq_table=(chisq.test(M1$Educational.attainment,M1[[var]]))
  print(Chisq_table)
  cat("\n")
}


##Chi-Square test for Anemina Level VS All variables
All_variables=c("Education.attainment","Type.of.place.of.residence","State","Wealth.index","Source.of.drinking.water","Total.children.ever.born","Respondent.s.current.age","Type.of.toilet.facility","Sex.of.child","Currently.breastfeeding","Currently.pregnant..from.household.questionnaire.")
for(var in All_variables){
  print(paste("Chi-Square test of Anemia Level and",var))
  Chisq_table=(chisq.test(M1$Anemia.level,M1[[var]]))
  print(Chisq_table)
  cat("\n")
}

##T-test for Anemina Level VS All variables
All_variables=c("Education.attainment","Type.of.place.of.residence","State","Wealth.index","Source.of.drinking.water","Total.children.ever.born","Respondent.s.current.age","Type.of.toilet.facility","Sex.of.child","Currently.breastfeeding","Currently.pregnant..from.household.questionnaire.")
for(var in All_variables){
  print(paste("T-test of Anemia Level and",var))
  Ttest_table=t.test(M1$Anemia.level,M1[[var]])
  print(Ttest_table)
  cat("\n")
}

##Chi-Square test for Education Attainment VS All variables
All_variables=c("Type.of.place.of.residence","State","Wealth.index","Total.children.ever.born","Respondent.s.current.age","Type.of.toilet.facility","Sex.of.child","Anemia.level")
for(var in All_variables){
  print(paste("T-test of Education attainment and",var))
  Ttest_table=t.test(M1$Educational.attainment,M1[[var]])
  print(Ttest_table)
  cat("\n")
}
Ttest_table


##Anova for Education Attainment and other variables
All_variables=c("Type.of.place.of.residence","State","Wealth.index","Total.children.ever.born","Respondent.s.current.age","Type.of.toilet.facility","Sex.of.child","Anemia.level")
for(var in All_variables){
  print(paste("ANOVA of Education attainment and",var))
  Anova_table=aov(M1$Educational.attainment~M1[[var]],data=M1)
  print(Anova_table)
  cat("\n")
}



#####################################################################################################

# List of independent variables for ANOVA
anova_vars <- c("Type.of.place.of.residence", 
                "Source.of.drinking.water", 
                "Number.of.children.5.and.under.in.household..de.jure.",
                "Wealth.index", 
                "Currently.breastfeeding",
                "Currently.pregnant..from.household.questionnaire.",
                "Sex.of.child")

# Store results
results <- list() 

# Loop through each variable for ANOVA
for (i in 1:length(anova_vars)) {
  # Set the dependent variable
  dependent_var <- anova_vars[i]
  
  # Remaining variables for independent variables
  remaining_vars <- anova_vars[-i]
  
  # Create formula for ANOVA
  formula <- as.formula(paste("M1$", dependent_var, "~", paste("M1$", remaining_vars, collapse = " + "), sep = ""))
  
  # Perform ANOVA
  anova_result <- aov(formula, data = M1)
  
  # Store the summary of the ANOVA result
  results[[dependent_var]] <- summary(anova_result)
  
  # Print the summary
  cat("\nANOVA result for", dependent_var, "with remaining variables:\n")
  print(summary(anova_result))
}


########################################################################################################################################################


barplot(table(M1$Anemia.level),main="Anemia Level in Women",xlab="Anemia Level",ylab="count",las=1,col=rainbow(M1$Anemia.level))
legend("topright",legend=names(table(M1$Anemia.level)),fill=rainbow(M1$Anemia.level))


#Boxplot of Anemia Level
boxplot(M1$Anemia.level,horizontal=TRUE,main="Boxplot of Anemia Level in Women")
barplot(table(M1$Anemia.level),xlab="Anemia Level in Women",ylab="count",main="Distribution of Anemia Level in Women",col=c(2,3,4,5,6))
legend("topright",legend=names(table(M1$Anemia.level)),fill=c(2,3,4,5,6))
pie(table(M1$Anemia.level),c("Severe", "Moderate", "Mild", "Not anemic", "Missing"),main="Anemia Level in Women",col=c(2,3,4,5,6))
legend("bottomright",legend=c("Severe", "Moderate", "Mild", "Not anemic", "Missing"),fill=c(2,3,4,5,6))





# List of categorical variables to test
chi_square_vars <- c("Respondent.s.current.age","Type.of.place.of.residence", "Source.of.drinking.water", 
                     "Type.of.toilet.facility", "Educational.attainment","Total.children.ever.born","Number.of.living.children" ,"Currently.breastfeeding",
                     "Wealth.index", "Currently.breastfeeding","State","Wealth.index","Body.Mass.Index","Covered.by.health.insurance","Sex.of.child")

# Initialize an empty data frame to store results
chi_square_results <- data.frame(
  Variable = character(),
  Chi_Square = numeric(),
  DF = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Perform Chi-Square test for each variable and store results
for (var in chi_square_vars) {
  # Create a contingency table between Anemia.level and the variable
  contingency_table <- table(M1$Anemia.level, M1[[var]])
  
  # Perform Chi-square test
  chi_test <- chisq.test(contingency_table)
  
  # Extract Chi-square statistic, degrees of freedom, and p-value
  chi_square_stat <- chi_test$statistic
  degrees_freedom <- chi_test$parameter
  p_value <- chi_test$p.value
  
  # Store results in the data frame
  chi_square_results <- rbind(chi_square_results, data.frame(
    Variable = var,
    Chi_Square = chi_square_stat,
    DF = degrees_freedom,
    P_Value = p_value
  ))
}

# View the Chi-square results table
print(chi_square_results)




##############---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##############---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

m1$Respondent.s.current.age= cut(m1$Respondent.s.current.age, breaks= c(0,5,15,25,40,60), labels= c("0-5", "6-15", "16-25", "26-40", "40+"), right= TRUE)

tab_data= table(m1$Respondent.s.current.age, m1$Anemia.level)

percent_data= prop.table(tab_data,1)*100

##par(mar = c(4, 4, 4, 2))
barplot(t(percent_data), col=c("salmon", "skyblue"), names.arg=c("0-5", "6-15", "16-25", "26-40", "40+"),   main="Anemia Distribution by Age Group", xlab="Age Group", ylab="Percentage", legend.text=c("Anemic", "Non-Anemic"), args.legend = list(x="topright"), xlim=c(0,8) )



