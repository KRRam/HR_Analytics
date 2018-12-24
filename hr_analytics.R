# in order to get most of their employees to stay. Also, they want to know
# which of these variables is most important and needs to be addressed rightaway.

# You are required to model the probability of attrition using a
# logistic regression.

#Read file from specified directory
#Read data files from specified directory

ESdata <- read.csv(file.choose(), stringsAsFactors = F)
MSdata <- read.csv(file.choose(), stringsAsFactors = F)
GenData <- read.csv(file.choose(), stringsAsFactors = F)

#Identify and install packages that are to be used
rqd_pkg <-
  c(
    "MASS",
    "car",
    "dplyr",
    "stringdist",
    "stringr",
    "forcats",
    "tidyr",
    "tidyverse",
    "lubridate",
    "ggplot2",
    "GGally",
    "reshape2",
    "caret",
    "e1071",
    "cowplot",
    "caTools"
  )
pload <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pload(rqd_pkg)

#Understanding Data

dim(ESdata)
# 4410    4
# Shows that ESdata has 4410 observations in 4 variables 

str(ESdata)
# 'data.frame':	4410 obs. of  4 variables:
# $ EmployeeID             : int  1 2 3 4 5 6 7 8 9 10 ...
# $ EnvironmentSatisfaction: int  3 3 2 4 4 3 1 1 2 2 ...
# $ JobSatisfaction        : int  4 2 2 4 1 2 3 2 4 1 ...
# $ WorkLifeBalance        : int  2 4 1 3 3 2 1 3 3 3 ...

View(ESdata)
lengths(lapply(ESdata, unique))
# EmployeeID              EnvironmentSatisfaction 
# 4410                       5 
# JobSatisfaction         WorkLifeBalance 
# 5                       5 

# shows that there are 4410 unique employee ids and 
# 5  values of the other 3 variables 

unique(ESdata$EnvironmentSatisfaction)
# 3  2  4  1 NA

unique(ESdata$JobSatisfaction)
# 4  2  1  3 NA

unique(ESdata$WorkLifeBalance)
# 2  4  1  3 NA

#Considering that this data set is an employee survey data it may 
# be likely that the NA values may be there by design rather than by error
# In such a case it may be useful to include NA values as another factor level

#NA Value check
colSums(is.na(ESdata))
# EmployeeID              EnvironmentSatisfaction 
# 0                       25 
# JobSatisfaction         WorkLifeBalance 
# 20                      38 

sum(is.na(ESdata))
# 83

#The below extracts row nos to a variable and 
# shows EmployeeId for those who did not respond
NArows<- c(unique (unlist (lapply (ESdata, function (x) which (is.na (x))))))
length(NArows)
# 83

# Lets convert the NA to 5 and add a column MVal to show 1 as a NA representing missing 
# value and 0 as no data change for reference

ESdata[is.na(ESdata)] <- 5
ESdata$Mval <- 0

ESdata <- ESdata %>%
  mutate(Mval=replace(Mval, (EnvironmentSatisfaction ==5 | JobSatisfaction == 5 | WorkLifeBalance == 5), 1)) %>%
  as.data.frame()

sum(ESdata$Mval)
# 83

#convert numeric variables to factor variables

ESdata[sapply(ESdata, is.numeric)] <-
  lapply(ESdata[sapply(ESdata, is.numeric)], as.factor)

sapply(ESdata, class)
# EmployeeID EnvironmentSatisfaction         JobSatisfaction 
# "factor"                "factor"                "factor" 
# WorkLifeBalance         Mval 
# "factor"                "factor" 

dim(MSdata)
# 4410    3
# Shows that MSdata has 4410 observations in 3 variables 

str(MSdata)
# 'data.frame':	4410 obs. of  3 variables:
# $ EmployeeID       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ JobInvolvement   : int  3 2 3 2 3 3 3 3 3 3 ...
# $ PerformanceRating: int  3 4 3 3 3 3 4 4 4 3 ...

lengths(lapply(MSdata, unique))
# EmployeeID    JobInvolvement PerformanceRating 
# 4410                 4                 2 

# shows that there are 4410 unique employee ids and 2 other    
# variables that have 4 and 2 unique values. 

colSums(is.na(MSdata))
# EmployeeID    JobInvolvement PerformanceRating 
# 0                 0                 0 
# Shows that MSdata has No observations with NA values

View(MSdata)

unique(MSdata$JobInvolvement)
# 3 2 1 4

unique(MSdata$PerformanceRating)
# 3 4

# The variables can be considered as factors. Also Performance rating has only 2 values 

table(MSdata$JobInvolvement,MSdata$PerformanceRating)
#     3    4
# 1  219   30
# 2  948  177
# 3 2199  405
# 4  366   66

# Given 4 integer values, we can combine Jobinvolvement and Performance rating
# to a combined measure as a categorical variable. We can combine the two 
# into 8 combinations because one of them-Performance rating has only 
# 2 levels "3" and "4"

MSdata$JIPR <- as.factor(10*as.numeric(MSdata$JobInvolvement)+ as.numeric(MSdata$PerformanceRating))
# This converts the table above into 8 combinations of the factors with levels 
# "3" and "4" converting to index values of 1 and 2 respectively

levels(MSdata$JIPR)
# "11" "12" "21" "22" "31" "32" "41" "42"

# converting numeric values to factors

MSdata[sapply(MSdata, is.numeric)] <-
  lapply(MSdata[sapply(MSdata, is.numeric)], as.factor)

setdiff(MSdata$EmployeeID,ESdata$EmployeeID) # Identical EmployeeID across these datasets
# character(0)
identical(MSdata$EmployeeID, ESdata$EmployeeID)
# [1] TRUE
# Shows that the EmployeeIDs are matched in MSdata and ESdata so they can be merged

ESdatanew <- merge(ESdata, MSdata[,-c(2,3)], by.x = "EmployeeID", sort = FALSE)

class(ESdatanew$EmployeeID)
#"factor"

ESdatanew$EmployeeID<- as.character(ESdatanew$EmployeeID) #EmployeeID as character
lapply(ESdatanew, class)
# $`EmployeeID`
# "character"
# 
# $EnvironmentSatisfaction 
# "factor"
# 
# $JobSatisfaction
# "factor"
# 
# $WorkLifeBalance
# "factor"
# 
# $Mval
# "factor"
# 
# $JIPR
# "factor"


dim(GenData)
# 4410   24

glimpse(GenData)
# Observations: 4,410
# Variables: 24
# $ Age                     <int> 51, 31, 32, 38, 32, 46, 28, 29, 31, 25, 45, 36, ...
# $ Attrition               <chr> "No", "Yes", "No", "No", "No", "No", "Yes", "No"...
# $ BusinessTravel          <chr> "Travel_Rarely", "Travel_Frequently", "Travel_Fr...
# $ Department              <chr> "Sales", "Research & Development", "Research & D...
# $ DistanceFromHome        <int> 6, 10, 17, 2, 10, 8, 11, 18, 1, 7, 17, 28, 14, 1...
# $ Education               <int> 2, 1, 4, 5, 1, 3, 2, 3, 3, 4, 2, 1, 4, 1, 3, 3, ...
# $ EducationField          <chr> "Life Sciences", "Life Sciences", "Other", "Life...
# $ EmployeeCount           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
# $ EmployeeID              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1...
# $ Gender                  <chr> "Female", "Female", "Male", "Male", "Male", "Fem...
# $ JobLevel                <int> 1, 1, 4, 3, 1, 4, 2, 2, 3, 4, 2, 1, 1, 1, 1, 2, ...
# $ JobRole                 <chr> "Healthcare Representative", "Research Scientist...
# $ MaritalStatus           <chr> "Married", "Single", "Married", "Married", "Sing...
# $ MonthlyIncome           <int> 131160, 41890, 193280, 83210, 23420, 40710, 5813...
# $ NumCompaniesWorked      <int> 1, 0, 1, 3, 4, 3, 2, 2, 0, 1, 0, 0, 0, 1, 1, 4, ...
# $ Over18                  <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"...
# $ PercentSalaryHike       <int> 11, 23, 15, 11, 12, 13, 20, 22, 21, 13, 13, 12, ...
# $ StandardHours           <int> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, ...
# $ StockOptionLevel        <int> 0, 1, 3, 3, 2, 0, 1, 3, 0, 1, 2, 2, 0, 2, 0, 0, ...
# $ TotalWorkingYears       <int> 1, 6, 5, 13, 9, 28, 5, 10, 10, 6, 21, 16, 37, 10...
# $ TrainingTimesLastYear   <int> 6, 3, 2, 5, 2, 5, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, ...
# $ YearsAtCompany          <int> 1, 5, 5, 8, 6, 7, 0, 0, 9, 6, 20, 15, 36, 10, 5,...
# $ YearsSinceLastPromotion <int> 0, 1, 0, 7, 0, 7, 0, 0, 7, 1, 4, 10, 4, 9, 0, 0,...
# $ YearsWithCurrManager    <int> 0, 4, 3, 5, 4, 7, 0, 0, 8, 5, 10, 11, 13, 9, 4, ...

lengths(lapply(GenData, unique))
# Age                     Attrition 
# 43                      2 
# BusinessTravel          Department 
# 3                       3 
# DistanceFromHome        Education 
# 29                      5 
# EducationField          EmployeeCount 
# 6                       1 
# EmployeeID              Gender 
# 4410                    2 
# JobLevel                JobRole 
# 5                       9 
# MaritalStatus           MonthlyIncome 
# 3                       1349 
# NumCompaniesWorked      Over18 
# 11                       1 
# PercentSalaryHike       StandardHours 
# 15                       1 
# StockOptionLevel        TotalWorkingYears 
# 4                       41 
# TrainingTimesLastYear   YearsAtCompany 
# 7                       37 
# YearsSinceLastPromotion YearsWithCurrManager 
# 16                      18 

colSums(is.na(GenData))
# Age                     Attrition 
# 0                       0 
# BusinessTravel          Department 
# 0                       0 
# DistanceFromHome        Education 
# 0                       0 
# EducationField          EmployeeCount 
# 0                       0 
# EmployeeID              Gender 
# 0                       0 
# JobLevel                JobRole 
# 0                       0 
# MaritalStatus           MonthlyIncome 
# 0                       0 
# NumCompaniesWorked      Over18 
# 19 #needs review        0 
# PercentSalaryHike       StandardHours 
# 0                       0 
# StockOptionLevel        TotalWorkingYears 
# 0                       9 #needs review
# TrainingTimesLastYear   YearsAtCompany 
# 0                       0 
# YearsSinceLastPromotion YearsWithCurrManager 
# 0                       0

# Shows that there are 3 variables - EmployeeCount, Over18 and 
# StandardHours with only one unique value and no NA values
# These columns can be removed without loss of information

GenDatanew <- GenData[,-c(8,16,18)]

# Shows that there are NA values in variables - NumCompaniesWorked, TotalWorkingYears and 
# Lets split the Gendata DF into complete and incomplete sets

GenDataCC <- GenDatanew[complete.cases(GenDatanew),]
GenDataNA <- GenDatanew[!complete.cases(GenDatanew),]
dim(GenDataNA)
# 28 21
# shows there are 28 observations with NA data

addmargins(table(GenDataNA$YearsAtCompany, GenDataNA$TotalWorkingYears),1) 

# Shows Years at Co in Rows and Tot Work years in Cols
#     0 1 4 5 6 7 8 9 10 18 20 28
# 0   1 0 0 0 0 0 0 0  0  0  0  0
# 1   0 1 0 1 0 0 0 0  1  0  0  0
# 3   0 0 0 0 0 1 0 0  1  0  0  0
# 4   0 0 1 0 1 0 0 0  0  0  0  0
# 5   0 0 0 1 2 1 0 0  0  0  0  1
# 6   0 0 0 0 0 0 0 0  0  0  0  0
# 7   0 0 0 0 0 0 1 1  0  0  0  0
# 8   0 0 0 0 0 0 0 0  0  0  0  0
# 9   0 0 0 0 0 0 0 1  1  0  0  0
# 10  0 0 0 0 0 0 0 0  0  1  0  0
# 19  0 0 0 0 0 0 0 0  0  0  1  0
# 20  0 0 0 0 0 0 0 0  0  0  0  0
# 21  0 0 0 0 0 0 0 0  0  0  0  0
# Sum 1 1 1 2 3 2 1 2  3  1  1  1

# In some cases Tot work years and No of years in Co are equal. 
# In such cases, the companies worked at can be 1

temp<- GenData[which(GenData$YearsAtCompany == GenData$TotalWorkingYears),]
# creates a subset to test our hypothesis
summary(temp$NumCompaniesWorked)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       1       1       1       1       1       1       5 
#shows our hypothesis is true, which means NA values in such cases can be 1

GenDataNA$NumCompaniesWorked[GenDataNA$YearsAtCompany == GenDataNA$TotalWorkingYears]<-1

# lets bind complete cases where NA values are imputed to the complete cases 
# dataset and remove the same from the GenDataNA dataset
GenDataCC <- rbind(GenDataCC,GenDataNA[complete.cases(GenDataNA),])
GenDataNA <- GenDataNA[!complete.cases(GenDataNA),]
sum(is.na(GenDataNA))/nrow(GenDataCC)
# 23

sum(is.na(GenDataNA))/(nrow(GenDataCC)+nrow(GenDataNA))
# [1] 0.00521542

# %NA is 0.5% which is insignificant and hence NA rows can be deleted

#Extract employee ids of NA rows being excluded 
NA_employeeid <- GenDataNA$EmployeeID

lapply(GenDataCC[,c(2:4,6:7,9:12,14,16,18)], unique)

# $`Attrition`
# [1] "No"  "Yes"
# 
# $BusinessTravel
# [1] "Travel_Rarely"     "Travel_Frequently" "Non-Travel"       
# 
# $Department
# [1] "Sales"          "Research & Development" "Human Resources"       
# 
# $Education
# [1] 2 1 4 5 3
# 
# $EducationField
# [1] "Life Sciences"    "Other"            "Medical"          "Marketing"       
# [5] "Technical Degree" "Human Resources" 
# 
# $Gender
# [1] "Female" "Male"  
# 
# $JobLevel
# [1] 1 4 3 2 5
# 
# $JobRole
# [1] "Healthcare Representative" "Research Scientist"       
# [3] "Sales Executive"           "Human Resources"          
# [5] "Research Director"         "Laboratory Technician"    
# [7] "Manufacturing Director"    "Sales Representative"     
# [9] "Manager"                  
# 
# $MaritalStatus
# [1] "Married"  "Single"   "Divorced"

# $NumCompaniesWorked
# [1] 1 0 3 4 2 7 9 5 6 8
# 
# $StockOptionLevel
# [1] 0 1 3 2
# 
# $TrainingTimesLastYear
# [1] 6 3 2 5 4 0 1

# The above variables are categorical so we can convert to factor

GenDataCC[,c(2:4,6:7,9:12,14,16,18)] <- lapply(GenDataCC[,c(2:4,6:7,9:12,14,16,18)], as.factor)
lapply(GenDataCC, class)

View(GenDataCC)
GenDataCC$EmployeeID <- as.character(GenDataCC$EmployeeID)
setdiff(GenDataCC$EmployeeID,as.numeric(ESdatanew$EmployeeID)) # Identical EmployeeID across these datasets
# character(0)

Master <- merge(ESdatanew,GenDataCC, by.x = "EmployeeID", sort = FALSE)

#Read Attendance data files from specified directory
Intim <- read.csv(file.choose(), stringsAsFactors = F)
Outim <- read.csv(file.choose(), stringsAsFactors = F)

dim(Intim)
# [1] 4410  262 - 4410 rows and 262 columns
View(Intim)
# Shows that the first column contains serial nos - perhaps EmployeeID 
# an the rest of the columns contain Dates with a character X

dim(Outim)
# [1] 4410  262 - 4410 rows and 262 columns
View(Outim)
# Shows that the first column contains serial nos - perhaps EmployeeID 
# an the rest of the columns contain Dates with a character X

setdiff(colnames(Intim),colnames(Outim))
# character(0)
identical(colnames(Intim),colnames(Outim))
# [1] TRUE
# Shows that the Intime and Outtimes are calculated for the same dates

Timein <- as.data.frame(Intim[,-1])
Timeout <- as.data.frame(Outim[,-1])

# Extracting column names
Names <- colnames(Intim[,c(2:ncol(Intim))])
Names <- sub('X', '', Names)

whrsdata <-
  as.data.frame(sapply(1:ncol(Timeout), function(i)
    round(
      difftime(
        time1 = Timeout[, i],
        time2 = Timein[, i],
        units = "hours"
      ), 2
    )))

colnames(whrsdata) <- Names

whrsdata$meanwhrs <- round(rowMeans(whrsdata, na.rm = TRUE),2) 

whrsdata$leave <- round(rowSums(is.na(whrsdata)),0)

whrsdata$wrkds <- rowSums(!is.na(whrsdata))-2

sapply(whrsdata[,c(262:264)], summary)
#         meanwhrs    leave    wrkds
# Min.     5.95000 13.00000 225.0000
# 1st Qu.  6.67000 20.00000 232.0000
# Median   7.41000 25.00000 236.0000
# Mean     7.70083 24.73469 236.2653
# 3rd Qu.  8.37000 29.00000 241.0000
# Max.    11.03000 36.00000 248.0000

# Shows aggregate for all employees attendance statistics such as
# mean work hours (meanwhrs), leave for period(leave), 
# and workdays for period (wrkds)

# Excluding whrsdata for NA employee ids
# since Employee ids and row numbers are sequential we can use the below
whrsdata_cc <- whrsdata[-c(NA_employeeid),]

# combining employee level attendance data with Master data

dim(whrsdata_cc)
# 4387  264
# our interest is only the last 3 columns in this data frame
dim(Master)
# 4387   26

Master <- cbind(Master, whrsdata_cc[,c(262:264)])
dim(Master)
# 4387   29

sapply(Master, class)

# EmployeeID    EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance                    Mval 
# "character"                "factor"                "factor"                "factor"                "factor" 

# JIPR                     Age               Attrition          BusinessTravel              Department 
# "factor"               "integer"                "factor"                "factor"                "factor" 

# DistanceFromHome               Education          EducationField                  Gender                JobLevel 
# "integer"                "factor"                "factor"                "factor"                "factor" 

# JobRole           MaritalStatus           MonthlyIncome      NumCompaniesWorked       PercentSalaryHike 
# "factor"                "factor"               "integer"                "factor"               "integer" 

# StockOptionLevel       TotalWorkingYears   TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion 
# "factor"               "integer"                "factor"               "integer"               "integer" 

# YearsWithCurrManager      meanwhrs                  leave                   wrkds 
# "integer"               "numeric"               "numeric"               "numeric" 


sum(Master$Attrition == "Yes")/nrow(Master)
# [1] 0.16093
# 16.1% Attrition rate .

# Creating segmented data sets for employee and attrition

Attitude_data <- Master[,c(8,1:6)]
lapply(Attitude_data,class)
# 
# $`Attrition`
# [1] "factor"
# 
# $EmployeeID
# [1] "character"
# 
# $EnvironmentSatisfaction
# [1] "factor"
# 
# $JobSatisfaction
# [1] "factor"
# 
# $WorkLifeBalance
# [1] "factor"
# 
# $Mval
# [1] "factor"
# 
# $JIPR
# [1] "factor"

Demographic_data <- Master[,c(8,1,7,12:14,17)]
lapply(Demographic_data,class)
# 
# $`Attrition`
# [1] "factor"
# 
# $EmployeeID
# [1] "character"
# 
# $Age
# [1] "integer"
# 
# $Education
# [1] "factor"
# 
# $EducationField
# [1] "factor"
# 
# $Gender
# [1] "factor"
# 
# $MaritalStatus
# [1] "factor"


Attendance_data <- Master[,c(8,1,27:29)]
lapply(Attendance_data,class)
# $Attrition      $`EmployeeID`     $meanwhrs
# "factor"        "character"    "numeric"

# $leave          $wrkds
# numeric"        "numeric"

Work_data <- Master[,c(8,1,9:11,15:16,18:26)]
lapply(Work_data,class)
# $`Attrition`
# [1] "factor"
# 
# $EmployeeID
# [1] "character"
# 
# $BusinessTravel
# [1] "factor"
# 
# $Department
# [1] "factor"
# 
# $DistanceFromHome
# [1] "integer"
# 
# $JobLevel
# [1] "factor"
# 
# $JobRole
# [1] "factor"
# 
# $MonthlyIncome
# [1] "integer"
# 
# $NumCompaniesWorked
# [1] "factor"
# 
# $PercentSalaryHike
# [1] "integer"
# 
# $StockOptionLevel
# [1] "factor"
# 
# $TotalWorkingYears
# [1] "integer"
# 
# $TrainingTimesLastYear
# [1] "factor"
# 
# $YearsAtCompany
# [1] "integer"
# 
# $YearsSinceLastPromotion
# [1] "integer"
# 
# $YearsWithCurrManager
# [1] "integer"


# Barcharts for categorical features with stacked information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

ggplot(Attitude_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions by Environment Satisfaction show minor variations

ggplot(Attitude_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions by Job Satisfaction show minor variations

ggplot(Attitude_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions for a rating of 3 on Worklife balance is highest
# Worklife balance could be an important variable

ggplot(Attitude_data, aes(x=Mval,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions for Mval=1 denoting data points with NA responses is insignificant

ggplot(Attitude_data, aes(x=JIPR,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions for the composite Jobinvolvement and Performance rating is
#higher where job involvement is 2/3 (medium to high) and performance rating is 3 i.e Excellent
#JIPR composite can be an important variable. 

ggplot(Demographic_data, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions increases with education level but peaks at Bachelors education level of 3 
# Education level can be an important factor. Bachelors in particular 

ggplot(Demographic_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are higher for Male gender

ggplot(Demographic_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are lowest for Divorced employees and highest for Single
# Marital status can be an important factor contributing to Attrition. 

ggplot(Demographic_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are higher for education field of Life Sciences and Medical
#Specific educational areas maybe linked to specific job roles and job levels

ggplot(Work_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are higher for employees who travel rarely

ggplot(Work_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest for Job levels 1 and 2

ggplot(Work_data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest for Lab Technicians, Research Scientists and Sales Executives

ggplot(Work_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest for Sales and R&D departments and ties in above.

ggplot(Work_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest for 0 and 1 StockOption Level.

ggplot(Work_data, aes(x=as.factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest where Years since promotion is 0,1 or 2.

ggplot(Work_data, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are higher where staff have worked for 1 Company.
#Either longstanding employees or fresh employees are contributors 
# and can be checked by looking at total work years

ggplot(Work_data, aes(x=as.factor(YearsAtCompany),fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest within 1 year of work at Company and tapers after 10 years at Company.

ggplot(Work_data, aes(x=as.factor(TotalWorkingYears),fill=Attrition))+ geom_bar()+bar_theme1
#Attrition proportions are highest for 1/6/10 year total workyear employees at Company.

ggplot(Work_data, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()+bar_theme1
# Attrition proportions are higher where no of training times is 2/3. 
# Perhaps training makes trained staff marketable.

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(Work_data, aes(TotalWorkingYears, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme 
          ,ncol = 1)
#Shows total working years has many outliers

plot_grid(ggplot(Attendance_data, aes(meanwhrs, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Attendance_data, aes(x="",y=meanwhrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
#Shows mean work hrs has few outliers and attrition peaks at about 8 work hours
plot_grid(ggplot(Attendance_data, aes(leave, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Attendance_data, aes(x="",y=leave))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

#Shows peaking of attrition proportion at about 20 days and between 25-30 days, no outliers
plot_grid(ggplot(Attendance_data, aes(wrkds, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Attendance_data, aes(x="",y=wrkds))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
#Workdays and leave show similar plots suggesting corelation/collinearity

plot_grid(ggplot(Work_data, aes(DistanceFromHome, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.5)+coord_flip()+box_theme, 
          ncol = 1)
#Shorter travel distances and attrition show some relationship, no outliers

plot_grid(ggplot(Work_data, aes(MonthlyIncome, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
# Monthly income has several outliers

plot_grid(ggplot(Work_data, aes(PercentSalaryHike, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

plot_grid(ggplot(Work_data, aes(YearsAtCompany, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
# years at company has several outliers

plot_grid(ggplot(Work_data, aes(YearsSinceLastPromotion, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
# Shows bulk of employees have not had promotions in the last 3 years and contribute the most to Attrition

plot_grid(ggplot(Work_data, aes(YearsWithCurrManager, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(Work_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)
#Early years contribute to Attrition

# Identify numeric variables in data set
Cornum <-
  c(names(which((
    sapply(Master, class)
  ) == 'numeric')), names(which((
    sapply(Master, class)
  ) == 'integer')))

Cornum
# [1] "meanwhrs"                "leave"                  
# [3] "wrkds"                   "Age"                    
# [5] "DistanceFromHome"        "MonthlyIncome"          
# [7] "PercentSalaryHike"       "TotalWorkingYears"      
# [9] "YearsAtCompany"          "YearsSinceLastPromotion"
# [11] "YearsWithCurrManager"

# Correlation between numeric variables

Master[, c(Cornum)] <-
  lapply(Master[, c(Cornum)], as.numeric) #takes a subset of numeric/integer vars and converts to numeric

ggpairs(Master[, c(Cornum)])
# Shows the following key correlations
# 1. leave and wrkds (-1) as expected
# 2. YearsAtCompany and YearsWithCurrManager(0.769)
# 3. Age and TotalWorkingYears (0.68)
# 4. YearsAtCompany and TotalWorkingYears (0.628)
# 5. YearsAtCompany and YearsSinceLastPromotion (0.619)

# Subset and scale numeric variables
# check if there are outliers

quantile(Master$EnvironmentSatisfaction,seq(0,1,0.01))
quantile(Master$JobSatisfaction,seq(0,1,0.01))
quantile(Master$WorkLifeBalance,seq(0,1,0.01))
quantile(Master$Mval,seq(0,1,0.01))
quantile(Master$meanwhrs,seq(0,1,0.01))
quantile(Master$leave,seq(0,1,0.01))
quantile(Master$wrkds,seq(0,1,0.01))
quantile(Master$Age,seq(0,1,0.01))
quantile(Master$DistanceFromHome,seq(0,1,0.01))
quantile(Master$MonthlyIncome,seq(0,1,0.01))
quantile(Master$PercentSalaryHike,seq(0,1,0.01))
quantile(Master$TotalWorkingYears,seq(0,1,0.01))
quantile(Master$YearsAtCompany,seq(0,1,0.01))
quantile(Master$YearsSinceLastPromotion,seq(0,1,0.01))
quantile(Master$YearsWithCurrManager,seq(0,1,0.01))

# cap the values for variables
Master$EnvironmentSatisfaction[which(Master$EnvironmentSatisfaction > 4)] <-4
Master$TotalWorkingYears[which(Master$TotalWorkingYears > 35)] <-35
Master$YearsAtCompany[which(Master$YearsAtCompany > 24)] <-24
Master$YearsWithCurrManager[which(Master$YearsWithCurrManager > 14)] <-14

Master_num <-
  Master[, c(Cornum)] #subset of numeric/integer vars

Master_num <- as.data.frame(scale(Master_num[,c(1:ncol(Master_num))]))
View(Master_num)

# Identify categoric variables in data set
Corfact <-
  c(names(which((
    sapply(Master, class)
  ) == 'factor')))

Corfact
lapply(Master[, c(Corfact)], levels)
# $`EnvironmentSatisfaction`
# [1] "1" "2" "3" "4" "5"
# 
# $JobSatisfaction
# [1] "1" "2" "3" "4" "5"
# 
# $WorkLifeBalance
# [1] "1" "2" "3" "4" "5"
# 
# $Mval
# [1] "0" "1"
# 
# $JIPR
# [1] "13" "14" "23" "24" "33" "34" "43" "44"
# 
# $Attrition
# [1] "No"  "Yes"
# 
# $BusinessTravel
# [1] "Non-Travel"        "Travel_Frequently" "Travel_Rarely"    
# 
# $Department
# [1] "Human Resources"        "Research & Development"
# [3] "Sales"                 
# 
# $Education
# [1] "1" "2" "3" "4" "5"
# 
# $EducationField
# [1] "Human Resources"  "Life Sciences"    "Marketing"       
# [4] "Medical"          "Other"            "Technical Degree"
# 
# $Gender
# [1] "Female" "Male"  
# 
# $JobLevel
# [1] "1" "2" "3" "4" "5"
# 
# $JobRole
# [1] "Healthcare Representative" "Human Resources"          
# [3] "Laboratory Technician"     "Manager"                  
# [5] "Manufacturing Director"    "Research Director"        
# [7] "Research Scientist"        "Sales Executive"          
# [9] "Sales Representative"     
# 
# $MaritalStatus
# [1] "Divorced" "Married"  "Single"  
# 
# $NumCompaniesWorked
# [1] "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
# 
# $StockOptionLevel
# [1] "0" "1" "2" "3"
# 
# $TrainingTimesLastYear
# [1] "0" "1" "2" "3" "4" "5" "6"

# Converting target variable to factor with numeric levels

Master$Attrition<- ifelse(Master$Attrition=="Yes",0,1)
Attrition <- sum(Master$Attrition)/nrow(Master)
Attrition

Master_fact <- Master[,c(Corfact)]
#Removing predicted variable "Attrition" from this

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Master_fact[,-6], 
                            function(x) data.frame(model.matrix(~x,data =Master_fact[,-6]))[,-1]))
# [,-1] is to exclude intercepts

# Modeldataset
Master_final <- Master[,-c(2:7,9:29)]           #isolating EmployeeID and Attrition
Master_final <- cbind(Master_final,Master_num)  #adding scaled numeric variables
Master_final <- cbind(Master_final,dummies)     #adding categorical variable dummies
Master_final <- Master_final[,-1]               #Retaining only Attrition as target
str(Master_final)
# Char2Num <- function(x) {as.factor(as.integer(x))} # Converts character factors to integer factors
# # TO CHECK IF THIS STEP IS NEEDED OR WE CAN CREATE DUMMY AS IS

# splitting the data between train and test
set.seed(100)

indices = sample.split(Master_final$Attrition, SplitRatio = 0.7)

train = Master_final[indices,]

test = Master_final[!(indices),]

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

#Null deviance: 2709.2  on 3070  degrees of freedom
#Residual deviance: 1961.7  on 2995  degrees of freedom
#AIC: 2113.

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

# removing JIPR.x34 due to high p-value = 0.151621

model_3 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                WorkLifeBalance.x5 + JIPR.x33 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2 + EducationField.xMarketing + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1 + 
                NumCompaniesWorked.x2 + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_3)
vif(model_3)

# removing EducationField.xMarketing due to high p-value = 0.123841
model_4 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                WorkLifeBalance.x5 + JIPR.x33 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1 + 
                NumCompaniesWorked.x2 + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_4)
vif(model_4)

# removing NumCompaniesWorked.x2

model_5 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                WorkLifeBalance.x5 + JIPR.x33 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
                 + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_5)
vif(model_5)

# removing WorkLifeBalance.x5 due to high p-value= 0.127785
model_6 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + JIPR.x33 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
              + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_6)
vif(model_6)

# removing JIPR.x33 due to high p-value = 0.097941
model_7 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_7)
vif(model_7)

# removing NumCompaniesWorked.x4 due to high p-value = 0.095322
model_8 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther + Gender + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_8)
vif(model_8)

# remove Gender due to high p-value 0.096765

model_9 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
              + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x2  + 
                EducationField.xOther +  JobLevel.x2 + JobLevel.x5 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
              + NumCompaniesWorked.x5 + 
                NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_9)
vif(model_9)

# removing EducationField.xOthe 0.089543

model_10 <-glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
               + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2  + 
                   JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
               + NumCompaniesWorked.x5 + 
                 NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                 NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_10)
vif(model_10)

# removing JobLevel.x2 due to high p-value 0.075119
model_11 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                  NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_11)

# removing NumCompaniesWorked.x8

model_12 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_12)

# removing MaritalStatus.xMarried for low p-value 

model_13 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  NumCompaniesWorked.x9 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)

summary(model_13)

# removing NumCompaniesWorked.x9 

model_14 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + NumCompaniesWorked.x1  
                + NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                   StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_14)

# removing NumCompaniesWorked.x1 

model_15 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)

summary(model_15)

# removing JobRole.xLaboratory.Technician 

model_16 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                   JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_16)

# removing JobRole.xResearch.Scientist 

model_17 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xResearch.Director + 
                   JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)

summary(model_17)

# removing JobRole.xSales.Executive

model_18 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xResearch.Director +
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_18)

# removing TrainingTimesLastYear.x1

model_19 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.x2  + 
                  JobLevel.x5 + 
                  JobRole.xResearch.Director +
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  StockOptionLevel.x1 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_19)

# removing StockOptionLevel.x1

model_20 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
               + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2  + 
                 JobLevel.x5 + 
                 JobRole.xResearch.Director +
                 MaritalStatus.xSingle +  
                 NumCompaniesWorked.x5 + 
                 NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                 TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_20)

# removing Education.x2 

model_21 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  JobRole.xResearch.Director +
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_21)

# removing NumCompaniesWorked.x6

model_22 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  JobRole.xResearch.Director +
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_22)

# removing JobRole.xResearch.Director

model_23 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_23)
vif(model_23)

# Removing BusinessTravel.xTravel_Rarely p-value = 0.029982

model_24 <- glm(formula = Attrition ~ meanwhrs + Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_24)

# Removing Age  p-value 0.023245
model_25 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_25)

# removing WorkLifeBalance.x4 p-value= 0.001410
model_26 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
               + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales +
                 JobLevel.x5 + 
                 MaritalStatus.xSingle +  
                 NumCompaniesWorked.x5 + 
                 NumCompaniesWorked.x7 +  
                 TrainingTimesLastYear.x6, family = "binomial", data = train)

summary(model_26)

# removing JobSatisfaction.x2 p-value=0.002714 
model_27 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)

summary(model_27)
vif(model_27)
# removing JobSatisfaction.x3 p-value=0.022412

model_28 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 
                + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_28)
vif(model_28)

# removing WorkLifeBalance.x4 p-value=0.002323

model_29 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_29)
vif(model_29)
# removing WorkLifeBalance.x2 due to insignificance 

model_30 <- glm(formula = Attrition ~ meanwhrs  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + 
                  MaritalStatus.xSingle +  
                  NumCompaniesWorked.x5 + 
                  NumCompaniesWorked.x7 +  
                  TrainingTimesLastYear.x6, family = "binomial", data = train)
summary(model_30)

########################################################################
# With 17 significant variables in the model

final_model<- model_30

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Accuracy    : 84%  
# Sensitivity : 97%
# Specificity : 18%

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Accuracy    : 84%  
# Sensitivity : 98%
# Specificity : 11%

#Sensitivity	of	a	model	is	the	proportion	of	yeses	(or	positives)	correctly	predicted	by	it	as	yeses	(or	positives)
#	specificity	is	equal	to	the	proportion	of	nos	(or	negatives)	correctly	predicted	by	the	model	as	nos	(or	negatives)
# For	any	given	model,	if	the	sensitivity	increases	on	changing	the	cutoff,	its	specificity	goes down

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Identify the cutoff value where all 3 (Sensitivity, Specificity, Accuracy)
#  merge at one point

(cutoff <- s[which(abs(OUT[,1] - OUT[,2]) < 0.080)])

#0.8

# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.8, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
sens
spec

## Summary
# Accuracy    : 77%   
# Sensitivity : 78%         
# Specificity : 72% 


##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

## Summary 
# KS Static - 51%  (KS Static > 40 % indicates a good model)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

ggplot(Attrition_decile,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Gain,2)),  
            nudge_x = -0.40, nudge_y = -0.40)

ggplot(Attrition_decile,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Cumlift,2)), 
            nudge_x = 0.10, nudge_y = 0.10)

#	the	gain	for	a	given	model,	is	78%	by	the	4th	decile,	
# what	it	basically	means,	is	that	if	you	sort	all	employee	
# according	to	probability,	then	among	the	top	40%	customers	of	this	sorted	list,	
# you	would	find	78%	of	all	customers	that	were	likely	to leave the company.	

# The	lift	just	tells	you	the	factor	by	which	your	model	is	outperforming	a	random	model.	
# model's	lift	is	equal	to	1.08	by	the	3rd	decile,	
# it	means	that model's	gain	by	the	end	of	the	3rd	decile	is	1.08	times	that	of	a	random	model's	gain	at	the	end	of	3	deciles.
# In	other	words,	the	model	catches	1.08	times	more attrition	than	a	random	model	would	have	caught.