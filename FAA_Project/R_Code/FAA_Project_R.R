library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)

setwd("C:\\Users\\jeeva\\Documents\\BANA\\STAT Computing\\FAA1_Project_R\\")

#Section 1 --- Loading data, preprocessing the same and 
#Reading Data with the help of xls
FAA1 <- read_excel("FAA1.xls")
FAA2 <- read_excel("FAA2.xls")

#Removing exact duplicates in FAA1 and FAA2
FAA1 <- FAA1[!duplicated(FAA1), ]
FAA2 <- FAA2[!duplicated(FAA2), ]


#Finding Summary Stats of FAA1 and FAA2. To just get brief on missing values and
#Variable types
summary(FAA1)
summary(FAA2)

#Merging FAA1 and FAA2 datasets
#Print colnames of FAA1
colnames(FAA1)

#Print colnames of FAA2
colnames(FAA2)

#Duration is Missing in FAA2
#Combining FAA1 and FAA2 without duration
FAA1_FAA2_Combined <- rbind(FAA1[,-2], FAA2)

#Removing Duplicates
FAA1_FAA2_Combined <- FAA1_FAA2_Combined[!duplicated(FAA1_FAA2_Combined),]

#Create a PK which is a combination of all columns for FAA1 other than duration
FAA1_FAA2_Combined$prim_key <- paste(FAA1_FAA2_Combined$aircraft, FAA1_FAA2_Combined$no_pasg, 
                                     FAA1_FAA2_Combined$speed_ground, 
                                     FAA1_FAA2_Combined$speed_air, 
                                     FAA1_FAA2_Combined$height, FAA1_FAA2_Combined$pitch, 
                                     FAA1_FAA2_Combined$distance, 
                                     sep = "__")

#Create a PK which is a combination of all columns for FAA1 other than duration
FAA1$prim_key <- paste(FAA1$aircraft, FAA1$no_pasg, FAA1$speed_ground, 
                       FAA1$speed_air, FAA1$height, FAA1$pitch, FAA1$distance, 
                       sep = "__")

#Merge FAA1_FAA2_Combined with FAA1 to extract duration column
FAA1_FAA2_Combined <- merge(x = FAA1_FAA2_Combined, y = FAA1[,c("prim_key", "duration")],
                            by.x = "prim_key", by.y = "prim_key", all.x = TRUE)

#Print Dimension of final data set obtained
dim(FAA1_FAA2_Combined)

#Section 2: Completeness check of data

#Extract base summary stats
summary(FAA1_FAA2_Combined)

#Convert aircraft to categorical variable
FAA1_FAA2_Combined$aircraft <- as.factor(FAA1_FAA2_Combined$aircraft)
summary(FAA1_FAA2_Combined$aircraft)

#Checking for Missing Values
missing_percent <- as.data.frame(colSums(is.na(FAA1_FAA2_Combined)) 
                                 / nrow(FAA1_FAA2_Combined))
colnames(missing_percent) <- c("miss_percent")
View(missing_percent)
#Speed_Air has 75% Missing values and duration has 5% Missing values

#Checking for Abnormalities in data
#Duration has to be greater than 40 mins. Else Abnormal
nrow(FAA1_FAA2_Combined[which(FAA1_FAA2_Combined$duration <= 40),])
#Ans 5

#Ground speed needs to be in the range of 30 - 140 MPH. Else abnormal
nrow(FAA1_FAA2_Combined[which(FAA1_FAA2_Combined$speed_ground < 30 | 
                                FAA1_FAA2_Combined$speed_ground > 140 ),])
#Ans 3

# Height needs to  greater than 6 meters. Else abnormal
nrow(FAA1_FAA2_Combined[which(FAA1_FAA2_Combined$height <= 6),])
#Ans 10

# Distance needs to be less than 6000 feet. Else abnormal
nrow(FAA1_FAA2_Combined[which(FAA1_FAA2_Combined$distance >= 6000 ),])
#Ans 2


#Handling Missing Values
#For Speed_Air, there is no need to treat for missing values

#For Duration, we will replace missing values with median
median_dur <- median(FAA1_FAA2_Combined$duration, na.rm = TRUE)

FAA1_FAA2_Combined$duration_new <- ifelse(is.na(FAA1_FAA2_Combined$duration), 
                                          median_dur, 
                                          FAA1_FAA2_Combined$duration)
#Here we will be deleting abnormal values

index <- which(FAA1_FAA2_Combined$duration <= 40 |
           FAA1_FAA2_Combined$speed_ground < 30 | 
           FAA1_FAA2_Combined$speed_ground > 140 |
           FAA1_FAA2_Combined$height <= 6 | 
           FAA1_FAA2_Combined$distance >= 6000)

FAA1_FAA2_Combined2 <- FAA1_FAA2_Combined[-index,]

#Plotting histogram, qq plot and testing for Normality of each varaible

#Let's define a function which does all of it

dist_function <- function(x)
{
  par(mfrow = c(1,2))
  hist_plot <- hist(x)
  qqnorm(x, pch = 1, frame = FALSE)
  qqline(x, col = "steelblue", lwd = 2)
  shapiro.test(x)
}

#Plotting for all variables
dist_function(FAA1_FAA2_Combined2$no_pasg)
dist_function(FAA1_FAA2_Combined2$speed_ground)
dist_function(FAA1_FAA2_Combined2$height)
dist_function(FAA1_FAA2_Combined2$pitch)
dist_function(FAA1_FAA2_Combined2$distance)
dist_function(FAA1_FAA2_Combined2$duration_new)

#Doing Bi-Variate Analysis 

bivariate_function <- function(x)
{
  plot(x, FAA1_FAA2_Combined2$distance)
}

bivariate_function(FAA1_FAA2_Combined2$no_pasg)
bivariate_function(FAA1_FAA2_Combined2$speed_ground)
bivariate_function(FAA1_FAA2_Combined2$height)
bivariate_function(FAA1_FAA2_Combined2$pitch)
bivariate_function(FAA1_FAA2_Combined2$duration_new)

cor(FAA1_FAA2_Combined2$distance, FAA1_FAA2_Combined2[,-c(1,2,5,9)])

#Creating data frame with only required varaibles 
FAA1_FAA2_Model_Df <- FAA1_FAA2_Combined2[,-c(1,2,5,9)]

#Iter 1 - With all varaibles 

iter1 <- lm(distance ~ ., data = FAA1_FAA2_Model_Df)
summary(iter1)

#Remove Duration 
iter2 <- lm(distance ~ .-(duration_new), data = FAA1_FAA2_Model_Df)
summary(iter2)

#Remove No_Pasg
iter3 <- lm(distance ~ speed_ground + height + pitch, data = FAA1_FAA2_Model_Df)
summary(iter3)

#Remove Pitch
iter4 <- lm(distance ~ speed_ground + height, data = FAA1_FAA2_Model_Df)
summary(iter4)

#Remove Height
iter5 <- lm(distance ~ speed_ground, data = FAA1_FAA2_Model_Df)
summary(iter5)

#Choose Iter4 to be final iteration
summary(iter4)


