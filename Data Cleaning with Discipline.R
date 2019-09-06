library(tidyverse)
library(readxl)

getwd()

#Set working directory
setwd()

#Does size of school and grade level of school impact the discipline incident rate of schools?
#Also I want to export an aggregated dataset to load into Tableau
#Unlike the previous dataset with enrollment and spending by district where each year was separate and we
  #needed to append, we now have a dataset with all years already in there (2001-2018)

#1. Load in the data

Discipline<-read_excel("Building Discipline.xls")

#Remember that even though we have 198 obs in the dataset, that doesn't mean we havae 198 schools
#We have 198/17 years number of schools (also some schools may not have the full 17 years of data, some may have less)

#Let's look at the data
View(Discipline)
str(Discipline)

table(Discipline$School_Level)

#2. Recode school level to be numeric factor so that we can use it in a regression
#When you are recoding a character value, make sure to put quotes around the words

Discipline$School_Level_num<-recode(Discipline$School_Level, "Elem" = 1, "Middle"= 2, "High"=3)

str(Discipline$School_Level_num)

#3. Let's make a new variable so you also know how to do that (let's make an alcohol/drug incident variable)

View(Discipline)

#Let's add Discipline_Drug+Discipline_Alcohol
names(Discipline)

Discipline$DrugandAlcohol<-Discipline$DISCIPLINE_ALCOHOL+Discipline$DISCIPLINE_DRUG

View(Discipline)


#4. Aggregate by school name across all years (we want aggregate avg discipline rate, avg enrollment)
#We also want school level in our data, and I don't want that aggregate (that wouldn't make sense)
#So my options are to pull select only one year of data from my original dataset and then merge
  #school level in to my aggregate dataset after I make it
#OR, I know that school level is the same every year so I can use the "max" function or even "min" to pull just one
  #of the school levels

AggData<-Discipline %>%
  group_by(SCHOOL_NAME) %>%
  summarise(AvgDiscRate = mean(DISCIPLINE_INCIDENT_RATE, na.rm = TRUE), 
            AvgEnrollment = mean(ENROLLMENT_GRADES_K_12, na.rm = TRUE), SchoolLevel=max(School_Level_num))

#I want to round my enrollment
AggData$AvgEnrollment<-(round(AggData$AvgEnrollment,digits=0))


#5. Let's do a regression just to remember how to do it (normally you wouldn't want to aggregate 
  #everything and run the regression b/c you'll lose variation, you'd want to use all the years of data). 
#But we are doing this for demonstration purposes and b/c using all years of data requires more advance
  #analytical techniques. REMEMBER: You can't just use a linear regression using the data as it was originally,
  #with schools repeating in the data. That violates independence of observations. You must have only one line
  #per school. So to use all years of data, you would either have to restructure the data to wide (but still wouldn't
  #be good methodologically, or using different analysis techniques like longitudinal modeling, mixed, modeling,
  #time-series, multi-level modeling, etc. All far too advanced for this class))

names(AggData)

Model<-lm(AvgDiscRate~AvgEnrollment+factor(SchoolLevel), data=AggData)

summary(Model)

#Our model says that Enrollment is not associated with discipline rate
#And that Level 2 (middle school) IS associated with .54 higher rate than the level we left out (Elementary)
  #The t-score is greater than 1.96 and our p-value is less than .05
#However, high school (level 3) is not associated with discipline rate compared to elementary school
#Our overall model is poor (Adjusted R-square of .349) and F-statistic not significant

#This is simply for demonstration purposes
#Be warned about the following things:
#1. We aggregated our data so we lost a lot of variation
#2. Our sample size is ridculously low (we only have 14 schools)
#3. If you look at the data, some schools probably aren't reporting discipline (lots of 0 rates)--would want to look into this


#6. Let's export the aggregate dataset (in case we want to use this in Tableau or something)
#Again this is just for demostration purposes, you would probably just use all the years of data in Tableau b/c
#Tableau can aggregate for you

library(xlsx)

write.xlsx(AggData, file="AggDiscipline.xlsx") 

#You should be able to do row.names=FALSE but it wasn't working for me :)

#7. Last thing, about merging rules (b/c merging will happen a lot)
#In the previous video we merged two dataframes that both had the ID in the far left column and they were named
  #the same thing (DistrictName)
#However that is not always the case
#Here are rules depending on the dataset

#Merging when the datasets have the same ID

  #MergedData <- merge(dataA,dataB,by="ID")

#Merging when you need to merge on 2 or more IDs b/c one ID alone isn't unique (example District and School)
#Because School Name might be repeated in other districts

  #MergedData <- merge(dataA,dataB,by=c("SchoolID","DistrictID")) 

#Merging when the two datasets have different names for the IDs

  #MergedData<- merge(dataA, dataB, by.x = "dataAID", by.y = "dataBID"))

#8. And yesterday I mentioned the opposite of spread, it is gather (wide to long)

#While spread() (what we did in the previous video) takes two columns (key & value), 
  #and spreads into multiple columns: it makes "long" data wider, no repeating cases

#gather() takes multiple columns, and gathers them into key-value pairs: it makes "wide" data longer.

#So using the example from the previous video, if we wanted to go back to long from wide

# LongEnroll<-WideEnroll %>% gather(`ENROLL2017`, `ENROLL2018`, key = "YEAR", value = "ENROLLMENT")









