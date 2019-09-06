#Call the packages you know you are going to use
#Throughout the script if you call a function (such as mutate) and R throws an error saying it doesn't recognize
  #that function, it's very likely it's becauuse you forgot to load the required package for that function

library(tidyverse)
library(readxl)

getwd()


#Is size of school associated with Spending per ADA

#1. Set working directory

#Use drop down or use setwd()

#2. Load in 2018 sheet

Ed2018<-read_excel("Finance Data and Statistics Summary for All Districts.xls", skip=2, sheet="2018")

#3. Load in 2017 sheet

Ed2017<-read_excel("Finance Data and Statistics Summary for All Districts.xls", skip=2, sheet="2017")


#4. Let's check out the structure of this data before we move on

str(Ed2018)
str(Ed2017)

#I am going to make year in 2018 and 2017 factors so I can group by them
#In 2017 I want Enrollment and Spending to be numeric so I can do correlations

#5. Change variable structure

Ed2018$YEAR<-as.factor(Ed2018$YEAR)
Ed2017$YEAR<-as.factor(Ed2017$YEAR)

Ed2017$ENROLLMENT<-as.numeric(Ed2017$ENROLLMENT)

Ed2017$`CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`<-as.numeric(Ed2017$`CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`)

#6. Now I want to append these two years into one dataset so I can visualize both years

Education<-rbind(Ed2018,Ed2017)


#Can't because the number of columns don't match
#Check the dimensions

dim(Ed2017)
dim(Ed2018)

#Drop extra variable in 2018
#What is the extra variable?

names(Ed2018)
names(Ed2017)

Ed2018<-select(Ed2018,-"PK ENROLLMENT")

#Append now

Education<-rbind(Ed2018,Ed2017)

#7. Let's look at this data and see what we are dealing with.
#Let's make a scatterplot

ggplot(Education, aes(x=ENROLLMENT, y=`CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`, color=YEAR)) + geom_point()

#8. Oh wow, some crazy outliers. Let's drop those as well as the years where there is NA

Educationnew<-Education%>%filter(ENROLLMENT<25000 & `CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`<150000 & !is.na(YEAR))

#This is another way to do what we did above
#Educationnew<-subset(Education, ENROLLMENT<25000 & `CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`<150000 & !is.na(YEAR))

#9. Now let's try the plot again

ggplot(Educationnew, aes(x=ENROLLMENT, y=`CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`, color=YEAR)) + geom_point()

#It's not looking like there is much of a relationship between enrollment and spending.
#It also doesn't look like there is a lot of variation in enrollment, but there is a ton of variation in spending


#10. Next I want to actual get the correlation but I need to restructure data because I only want one line per building in order to do that
#Right now we have districts repeating in the data, I don't want that for correlation, regression, exploratory data analysis
#(you need unique rows)

#Let's first make the Enrollment Dataset
#First just get variables I want (make a smaller dataset)
#I want District Name, Year and Enrollment

EducationEnroll<-select(Educationnew,-1, -(5:17))

#Let's look at this data
View(EducationEnroll)

#This would get rid of any district with NA for district name but we don't have any
#EducationEnroll2<-EducationEnroll[complete.cases(EducationPop[ , 1:2]),]

#Now let's try to restructure using spread (from long to wide)

WideEnroll<-EducationEnroll%>%spread(YEAR, ENROLLMENT, fill=NA)%>%setNames( c("DISTRICTNAME", 
                                                                       "ENROLL2017", "ENROLL2018") )


#We get an error because there are duplicate districts and years in our dataset 
#(so for example, SLPS 2017 might appear twice in our data)

#Next only get unique districts (no duplicates) b/c restructure won't work with duplicates

EducationEnrollnodupes<-EducationEnroll %>% distinct(`DISTRICT NAME`, `YEAR`, .keep_all = TRUE)

#Now we can restructure and then change the variable names
#When you rename variables, start the name with characters, not numbers, R doesn't like variable names
  #that start in numbers

WideEnroll<-EducationEnrollnodupes%>%spread(YEAR, ENROLLMENT, fill=NA)%>%setNames( c("DISTRICTNAME", 
                                                                       "ENROLL2017", "ENROLL2018") )


#This gives us a dataset with Enrollment each year, but we also need spending each year so let's restructure again

#Make the dataset with just the variables we want again
#For this dataset I want District Name, Year and Current Expenditure per Average Daily Attendance

EducationSpend<-select(Educationnew,-1, -(4:9), -(11:17))

#Drop the duplicates

EducationSpendnodupes<-EducationSpend %>% distinct(`DISTRICT NAME`, `YEAR`, .keep_all = TRUE)

#Restructure

WideSpend<-EducationSpendnodupes%>%spread(YEAR, `CURRENT EXPENDITURE PER AVERAGE DAILY ATTENDANCE`, fill=NA)%>%setNames( c("DISTRICTNAME", 
                                                                       "SPEND2017", "SPEND2018") )
#Last I want to merge those two datasets together

Combined<-merge(WideEnroll, WideSpend)

str(Combined)

#11. Last we can finally run our correlation matrix to see if there is any relationship (next steps might be a regression)

#To run correlation table it has to have only numeric variables so I am going to pair my dataset down to only numeric vars
#So I will drop District Name

Combined2<-select(Combined,-1)
str(Combined2)

#I only want to use complete observations in my correlation matrix, so any district with NA in any variable is removed
cor(Combined2,use="complete.obs", method=c("pearson"))

#It's not looking like enrollment is very highly correlated with spending (i.e. bigger schools aren't 
  #necessarily correlated with higher spending)

#Let's do a t-test to get the significance of our correlation

library(ggpubr)

cor.test(Combined2$ENROLL2017,Combined2$SPEND2017)

#The correlation between enrollment and spending is not significant, absolute t-vale is less than 1.96
  #and p-value is greater than .05
#Note that this correlation value is a little different from the one above in the correlation matrix
#This is because we are no longer dropping districts who were missing data in any of our 4 variables so this 
  #correlation includes a few more districts

cor.test(Combined2$ENROLL2018,Combined2$SPEND2018)

#12. Let's just look at one last thing
#Let's group our districts into 4 bins of size (small, medium, large, and huge) and then get the average spending
  #per group
#For this let's just look at 2018 enrollment

#Let's look at the summary of each of our variables
#Note we are back to the dataset with District Name in it

summary(Combined)

#Let's create our groups
  
Combinednew<-mutate(Combined, ENROLLGROUP2018 = ifelse(`ENROLL2018` %in% 0:230, "Small",
                                     ifelse(`ENROLL2018` %in% 231:559, "Medium",
                                            ifelse( `ENROLL2018`%in% 560:1355, "Large",
                                                   ifelse(`ENROLL2018` %in% 1356:24955, "Huge", NA)))))

                                                 
#Now let's get the average spending per group

Combinednew %>%
  group_by(ENROLLGROUP2018) %>%
  summarise(AvgSpend = mean(SPEND2018, na.rm = TRUE))

#


