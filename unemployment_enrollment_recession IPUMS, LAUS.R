##### LOAD AND PREP DATA
library('ipumsr')
library(zoo)
library(ggplot2)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("cps_00004.xml")
IPUMSdata <- read_ipums_micro(ddi)
IPUMSdata$MONTH <- formatC(IPUMSdata$MONTH,flag = 0,width = 2)

# Create date
IPUMSdate <- paste(IPUMSdata$YEAR,"-",IPUMSdata$MONTH,sep="")
IPUMSdate <- as.Date(as.yearmon(IPUMSdate))
IPUMSdata['date'] <- IPUMSdate

# Extract years that have CPS data, 18 and over, in universe of laborforce
IPUMSdata <- IPUMSdata[IPUMSdata$YEAR >= 2004,]
IPUMSdata <- IPUMSdata[IPUMSdata$YEAR <= 2013,]
IPUMSdata <- IPUMSdata[IPUMSdata$AGE >= 18,]
IPUMSdata <- IPUMSdata[IPUMSdata$LABFORCE != "0",]

# Create label for pre/post recession
# 0 := pre-recession (before 2009)
# 1 := recession (2009 and after)
Time_Period <- ifelse(IPUMSdata$YEAR < 2009, 0, 1)
IPUMSdata['time_period'] <- Time_Period

# Create Age Group variable
Age_Group <- ifelse(IPUMSdata$AGE <= 24,0,ifelse(IPUMSdata$AGE <= 44,1,ifelse(IPUMSdata$AGE <= 64,2,3)))
IPUMSdata['AGE1'] <- Age_Group

# Create Race group variable
Race_Group <- ifelse(IPUMSdata$RACE<=100,0,ifelse(IPUMSdata$RACE<=200,1,ifelse(IPUMSdata$RACE<=300,2,ifelse(IPUMSdata$RACE<=652,3,4))))
IPUMSdata['RACE1'] <- Race_Group

# Create Hispanic group variable
Hispanic_Group <- ifelse(IPUMSdata$HISPAN <= 0,0,1)
IPUMSdata['HISPAN1'] <- Hispanic_Group

# Create Citizen group variable
Citizen_Group <- ifelse(IPUMSdata$CITIZEN <= 1,0,1)
IPUMSdata['CITIZEN1'] <- Citizen_Group

# Create Metro group variable
Metro_Group <- ifelse(IPUMSdata$METRO <= 1,1,0)
IPUMSdata['METRO1'] <- Metro_Group

# Create Educational Attainment group variable
EdAttain_Group <- ifelse(IPUMSdata$EDUC99 <= 9,0,ifelse(IPUMSdata$EDUC99 <= 10,1,ifelse(IPUMSdata$EDUC99 <= 11,2,ifelse(IPUMSdata$EDUC99 <= 14,3,ifelse(IPUMSdata$EDUC99 <= 15,4,ifelse(IPUMSdata$EDUC99 <= 16,5,6))))))
IPUMSdata['EDUC991'] <- EdAttain_Group

#Create Class of Worker group variable
ClassWorker_Group <- ifelse(IPUMSdata$CLASSWKR <= 14,0,ifelse(IPUMSdata$CLASSWKR <= 23,1,ifelse(IPUMSdata$CLASSWKR <= 28,2,3)))
IPUMSdata['CLASSWKR1'] <- ClassWorker_Group




summary(IPUMSdata)




##### GROUP ANALYSIS #####

### ----------------------------------
### SEX

# Create contingency tables
tbl1 <- table(IPUMSdata$SEX,IPUMSdata$LABFORCE)
tbl1
tbl1.0 <- subset(IPUMSdata, time_period == 0)
tbl1.0 <- table(tbl1.0$SEX,tbl1.0$LABFORCE)
tbl1.0
tbl1.1 <- subset(IPUMSdata, time_period == 1)
tbl1.1 <- table(tbl1.1$SEX,tbl1.1$LABFORCE)
tbl1.1

# Basic barplots
barplot(tbl1,beside = TRUE)
barplot(tbl1.0,beside = TRUE)
barplot(tbl1.1,beside = TRUE)

# Perform chi-squared test of independence
model1 <- chisq.test(tbl1)
model1
model1.0 <- chisq.test(tbl1.0)
model1.0
model1.1 <- chisq.test(tbl1.1)
model1.1


### ----------------------------------
### AGE

# Create contingency tables
tbl2 <- table(IPUMSdata$AGE1,IPUMSdata$LABFORCE)
tbl2
tbl2.0 <- subset(IPUMSdata, time_period == 0)
tbl2.0 <- table(tbl2.0$AGE1,tbl2.0$LABFORCE)
tbl2.0
tbl2.1 <- subset(IPUMSdata, time_period == 1)
tbl2.1 <- table(tbl2.1$AGE1,tbl2.1$LABFORCE)
tbl2.1

# Basic barplots
barplot(tbl2,beside = TRUE)
barplot(tbl2.0,beside = TRUE)
barplot(tbl2.1,beside = TRUE)

# Perform chi-squared test of independence
model2 <- chisq.test(tbl2)
model2
model2.0 <- chisq.test(tbl2.0)
model2.0
model2.1 <- chisq.test(tbl2.1)
model2.1


### ----------------------------------
### RACE

# Create contingency tables
tbl3 <- table(IPUMSdata$RACE1,IPUMSdata$LABFORCE)
tbl3
tbl3.0 <- subset(IPUMSdata, time_period == 0)
tbl3.0 <- table(tbl3.0$RACE1,tbl3.0$LABFORCE)
tbl3.0
tbl3.1 <- subset(IPUMSdata, time_period == 1)
tbl3.1 <- table(tbl3.1$RACE1,tbl3.1$LABFORCE)
tbl3.1

# Basic barplots
barplot(tbl3,beside = TRUE)
barplot(tbl3.0,beside = TRUE)
barplot(tbl3.1,beside = TRUE)

# Perform chi-squared test of independence
model3 <- chisq.test(tbl3)
model3
model3.0 <- chisq.test(tbl3.0)
model3.0
model3.1 <- chisq.test(tbl3.1)
model3.1


### ----------------------------------
### HISPANIC STATUS

# Create contingency tables
tbl4 <- table(IPUMSdata$HISPAN1,IPUMSdata$LABFORCE)
tbl4
tbl4.0 <- subset(IPUMSdata, time_period == 0)
tbl4.0 <- table(tbl4.0$HISPAN1,tbl4.0$LABFORCE)
tbl4.0
tbl4.1 <- subset(IPUMSdata, time_period == 1)
tbl4.1 <- table(tbl4.1$HISPAN1,tbl4.1$LABFORCE)
tbl4.1

# Basic barplots
barplot(tbl4,beside = TRUE)
barplot(tbl4.0,beside = TRUE)
barplot(tbl4.1,beside = TRUE)

# Perform chi-squared test of independence
model4 <- chisq.test(tbl4)
model4
model4.0 <- chisq.test(tbl4.0)
model4.0
model4.1 <- chisq.test(tbl4.1)
model4.1


### ----------------------------------
### CITIZENSHIP STATUS

# Create contingency tables
tbl5 <- table(IPUMSdata$CITIZEN1,IPUMSdata$LABFORCE)
tbl5
tbl5.0 <- subset(IPUMSdata, time_period == 0)
tbl5.0 <- table(tbl5.0$CITIZEN1,tbl5.0$LABFORCE)
tbl5.0
tbl5.1 <- subset(IPUMSdata, time_period == 1)
tbl5.1 <- table(tbl5.1$CITIZEN1,tbl5.1$LABFORCE)
tbl5.1

# Basic barplots
barplot(tbl5,beside = TRUE)
barplot(tbl5.0,beside = TRUE)
barplot(tbl5.1,beside = TRUE)

# Perform chi-squared test of independence
model5 <- chisq.test(tbl5)
model5
model5.0 <- chisq.test(tbl5.0)
model5.0
model5.1 <- chisq.test(tbl5.1)
model5.1


### ----------------------------------
### METRO

# Create contingency tables
tbl6 <- table(IPUMSdata$METRO1,IPUMSdata$LABFORCE)
tbl6
tbl6.0 <- subset(IPUMSdata, time_period == 0)
tbl6.0 <- table(tbl6.0$METRO1,tbl6.0$LABFORCE)
tbl6.0
tbl6.1 <- subset(IPUMSdata, time_period == 1)
tbl6.1 <- table(tbl6.1$METRO1,tbl6.1$LABFORCE)
tbl6.1

# Basic barplots
barplot(tbl6,beside = TRUE)
barplot(tbl6.0,beside = TRUE)
barplot(tbl6.1,beside = TRUE)

# Perform chi-squared test of independence
model6 <- chisq.test(tbl6)
model6
model6.0 <- chisq.test(tbl6.0)
model6.0
model6.1 <- chisq.test(tbl6.1)
model6.1


### ----------------------------------
### MARITAL STATUS

# Create contingency tables
tbl7 <- table(IPUMSdata$MARST,IPUMSdata$LABFORCE)
tbl7
tbl7.0 <- subset(IPUMSdata, time_period == 0)
tbl7.0 <- table(tbl7.0$MARST,tbl7.0$LABFORCE)
tbl7.0
tbl7.1 <- subset(IPUMSdata, time_period == 1)
tbl7.1 <- table(tbl7.1$MARST,tbl7.1$LABFORCE)
tbl7.1

# Basic barplots
barplot(tbl7,beside = TRUE)
barplot(tbl7.0,beside = TRUE)
barplot(tbl7.1,beside = TRUE)

# Perform chi-squared test of independence
model7 <- chisq.test(tbl7)
model7
model7.0 <- chisq.test(tbl7.0)
model7.0
model7.1 <- chisq.test(tbl7.1)
model7.1


### ----------------------------------
### VETERAN STATUS

# Create contingency tables
tbl8 <- table(IPUMSdata$VETSTAT,IPUMSdata$LABFORCE)
tbl8
tbl8.0 <- subset(IPUMSdata, time_period == 0)
tbl8.0 <- table(tbl8.0$VETSTAT,tbl8.0$LABFORCE)
tbl8.0
tbl8.1 <- subset(IPUMSdata, time_period == 1)
tbl8.1 <- table(tbl8.1$VETSTAT,tbl8.1$LABFORCE)
tbl8.1

# Basic barplots
barplot(tbl8,beside = TRUE)
barplot(tbl8.0,beside = TRUE)
barplot(tbl8.1,beside = TRUE)

# Perform chi-squared test of independence
model8 <- chisq.test(tbl8)
model8
model8.0 <- chisq.test(tbl8.0)
model8.0
model8.1 <- chisq.test(tbl8.1)
model8.1


### ----------------------------------
### DISABILITY STATUS

# Create contingency tables
tbl9 <- table(IPUMSdata$DIFFANY,IPUMSdata$LABFORCE)
tbl9
tbl9.0 <- subset(IPUMSdata, time_period == 0)
tbl9.0 <- table(tbl9.0$DIFFANY,tbl9.0$LABFORCE)
tbl9.0
tbl9.1 <- subset(IPUMSdata, time_period == 1)
tbl9.1 <- table(tbl9.1$DIFFANY,tbl9.1$LABFORCE)
tbl9.1

# Basic barplots
barplot(tbl9,beside = TRUE)
barplot(tbl9.0,beside = TRUE)
barplot(tbl9.1,beside = TRUE)

# Perform chi-squared test of independence
model9 <- chisq.test(tbl9)
model9
model9.0 <- chisq.test(tbl9.0)
model9.0
model9.1 <- chisq.test(tbl9.1)
model9.1


### ----------------------------------
### EDUCATIONAL ATTAINMENT

# Create contingency tables
tbl10 <- table(IPUMSdata$EDUC991,IPUMSdata$LABFORCE)
tbl10
tbl10.0 <- subset(IPUMSdata, time_period == 0)
tbl10.0 <- table(tbl10.0$EDUC991,tbl10.0$LABFORCE)
tbl10.0
tbl10.1 <- subset(IPUMSdata, time_period == 1)
tbl10.1 <- table(tbl10.1$EDUC991,tbl10.1$LABFORCE)
tbl10.1

# Basic barplots
barplot(tbl10,beside = TRUE)
barplot(tbl10.0,beside = TRUE)
barplot(tbl10.1,beside = TRUE)

# Perform chi-squared test of independence
model10 <- chisq.test(tbl10)
model10
model10.0 <- chisq.test(tbl10.0)
model10.0
model10.1 <- chisq.test(tbl10.1)
model10.1


### ----------------------------------
### COLLEGE ATTENDANCE

# Create contingency tables
tbl11temp <- subset(IPUMSdata, SCHLCOLL >= 3)
tbl11 <- table(tbl11temp$SCHLCOLL,tbl11temp$LABFORCE)
tbl11
tbl11.0 <- subset(tbl11temp, time_period == 0)
tbl11.0 <- table(tbl11.0$SCHLCOLL,tbl11.0$LABFORCE)
tbl11.0
tbl11.1 <- subset(tbl11temp, time_period == 1)
tbl11.1 <- table(tbl11.1$SCHLCOLL,tbl11.1$LABFORCE)
tbl11.1
rm(tbl11temp)

# Basic barplots
barplot(tbl11,beside = TRUE)
barplot(tbl11.0,beside = TRUE)
barplot(tbl11.1,beside = TRUE)

# Perform chi-squared test of independence
model11 <- chisq.test(tbl11)
model11
model11.0 <- chisq.test(tbl11.0)
model11.0
model11.1 <- chisq.test(tbl11.1)
model11.1


### ----------------------------------
### CLASS OF WORKER

# Create contingency tables
tbl12temp <- subset(IPUMSdata, CLASSWKR != 0 & CLASSWKR != 29)
tbl12 <- table(tbl12temp$CLASSWKR1,tbl12temp$LABFORCE)
tbl12
tbl12.0 <- subset(tbl12temp, time_period == 0)
tbl12.0 <- table(tbl12.0$CLASSWKR1,tbl12.0$LABFORCE)
tbl12.0
tbl12.1 <- subset(tbl12temp, time_period == 1)
tbl12.1 <- table(tbl12.1$CLASSWKR1,tbl12.1$LABFORCE)
tbl12.1
rm(tbl12temp)

# Basic barplots
barplot(tbl12,beside = TRUE)
barplot(tbl12.0,beside = TRUE)
barplot(tbl12.1,beside = TRUE)

# Perform chi-squared test of independence
model12 <- chisq.test(tbl12)
model12
model12.0 <- chisq.test(tbl12.0)
model12.0
model12.1 <- chisq.test(tbl12.1)
model12.1


### ----------------------------------
### REASON FOR UNEMPLOYMENT

# Create contingency tables
tbl13temp <- subset(IPUMSdata, WHYUNEMP != 0)
tbl13 <- table(tbl13temp$WHYUNEMP,tbl13temp$LABFORCE)
tbl13
tbl13.0 <- subset(tbl13temp, time_period == 0)
tbl13.0 <- table(tbl13.0$WHYUNEMP,tbl13.0$LABFORCE)
tbl13.0
tbl13.1 <- subset(tbl13temp, time_period == 1)
tbl13.1 <- table(tbl13.1$WHYUNEMP,tbl13.1$LABFORCE)
tbl13.1
rm(tbl13temp)

# Basic barplots
barplot(tbl13,beside = TRUE)
barplot(tbl13.0,beside = TRUE)
barplot(tbl13.1,beside = TRUE)

# Perform chi-squared test of independence
model13 <- chisq.test(tbl13)
model13
model13.0 <- chisq.test(tbl13.0)
model13.0
model13.1 <- chisq.test(tbl13.1)
model13.1


### ----------------------------------
### FULL OR PART TIME STATUS

# Create contingency tables
tbl14temp <- subset(IPUMSdata, WKSTAT != 99)
tbl14 <- table(tbl14temp$WKSTAT,tbl14temp$LABFORCE)
tbl14
tbl14.0 <- subset(tbl14temp, time_period == 0)
tbl14.0 <- table(tbl14.0$WKSTAT,tbl14.0$LABFORCE)
tbl14.0
tbl14.1 <- subset(tbl14temp, time_period == 1)
tbl14.1 <- table(tbl14.1$WKSTAT,tbl14.1$LABFORCE)
tbl14.1
rm(tbl14temp)

# Basic barplots
barplot(tbl14,beside = TRUE)
barplot(tbl14.0,beside = TRUE)
barplot(tbl14.1,beside = TRUE)

# Perform chi-squared test of independence
model14 <- chisq.test(tbl14)
model14
model14.0 <- chisq.test(tbl14.0)
model14.0
model14.1 <- chisq.test(tbl14.1)
model14.1


### ----------------------------------
### EMPLOYMENT STATUS

# Create contingency tables
tbl15 <- table(IPUMSdata$EMPSTAT,IPUMSdata$LABFORCE)
tbl15
tbl15.0 <- subset(IPUMSdata, time_period == 0)
tbl15.0 <- table(tbl15.0$EMPSTAT,tbl15.0$LABFORCE)
tbl15.0
tbl15.1 <- subset(IPUMSdata, time_period == 1)
tbl15.1 <- table(tbl15.1$EMPSTAT,tbl15.1$LABFORCE)
tbl15.1

# Basic barplots
barplot(tbl15,beside = TRUE)
barplot(tbl15.0,beside = TRUE)
barplot(tbl15.1,beside = TRUE)

# Perform chi-squared test of independence
model15 <- chisq.test(tbl15)
model15
model15.0 <- chisq.test(tbl15.0)
model15.0
model15.1 <- chisq.test(tbl15.1)
model15.1


### ----------------------------------
### REPEAT CHI-SQUARED TESTS BUT FIX ON LABEL AND RUN FOR LABOR FORCE BY RECESSION PERIOD

# Sex
tbl16.0 <- subset(IPUMSdata, SEX == 1)
tbl16.0 <- table(tbl16.0$time_period,tbl16.0$LABFORCE)
model16.0 <- chisq.test(tbl16.0)
model16.0
tbl16.1 <- subset(IPUMSdata, SEX == 2)
tbl16.1 <- table(tbl16.1$time_period,tbl16.1$LABFORCE)
model16.1 <- chisq.test(tbl16.1)
model16.1

# Age Group
tbl17.0 <- subset(IPUMSdata, AGE1 == 0)
tbl17.0 <- table(tbl17.0$time_period,tbl17.0$LABFORCE)
model17.0 <- chisq.test(tbl17.0)
model17.0
tbl17.1 <- subset(IPUMSdata, AGE1 == 1)
tbl17.1 <- table(tbl17.1$time_period,tbl17.1$LABFORCE)
model17.1 <- chisq.test(tbl17.1)
model17.1
tbl17.2 <- subset(IPUMSdata, AGE1 == 2)
tbl17.2 <- table(tbl17.2$time_period,tbl17.2$LABFORCE)
model17.2 <- chisq.test(tbl17.2)
model17.2
tbl17.3 <- subset(IPUMSdata, AGE1 == 3)
tbl17.3 <- table(tbl17.3$time_period,tbl17.3$LABFORCE)
model17.3 <- chisq.test(tbl17.3)
model17.3

#Race Group
tbl18.0 <- subset(IPUMSdata, RACE1 == 0)
tbl18.0 <- table(tbl18.0$time_period,tbl18.0$LABFORCE)
model18.0 <- chisq.test(tbl18.0)
model18.0
tbl18.1 <- subset(IPUMSdata, RACE1 == 1)
tbl18.1 <- table(tbl18.1$time_period,tbl18.1$LABFORCE)
model18.1 <- chisq.test(tbl18.1)
model18.1
tbl18.2 <- subset(IPUMSdata, RACE1 == 2)
tbl18.2 <- table(tbl18.2$time_period,tbl18.2$LABFORCE)
model18.2 <- chisq.test(tbl18.2)
model18.2
tbl18.3 <- subset(IPUMSdata, RACE1 == 3)
tbl18.3 <- table(tbl18.3$time_period,tbl18.3$LABFORCE)
model18.3 <- chisq.test(tbl18.3)
model18.3
tbl18.4 <- subset(IPUMSdata, RACE1 == 4)
tbl18.4 <- table(tbl18.4$time_period,tbl18.4$LABFORCE)
tbl18.4
model18.4 <- chisq.test(tbl18.4)
model18.4

# Hispanic
tbl19.0 <- subset(IPUMSdata, HISPAN1 == 0)
tbl19.0 <- table(tbl19.0$time_period,tbl19.0$LABFORCE)
model19.0 <- chisq.test(tbl19.0)
model19.0
tbl19.1 <- subset(IPUMSdata, HISPAN1 == 1)
tbl19.1 <- table(tbl19.1$time_period,tbl19.1$LABFORCE)
model19.1 <- chisq.test(tbl19.1)
model19.1

# Citizen
tbl20.0 <- subset(IPUMSdata, CITIZEN1 == 0)
tbl20.0 <- table(tbl20.0$time_period,tbl20.0$LABFORCE)
model20.0 <- chisq.test(tbl20.0)
model20.0
tbl20.1 <- subset(IPUMSdata, CITIZEN1 == 1)
tbl20.1 <- table(tbl20.1$time_period,tbl20.1$LABFORCE)
model20.1 <- chisq.test(tbl20.1)
model20.1

# Metro
tbl21.0 <- subset(IPUMSdata, METRO1 == 0)
tbl21.0 <- table(tbl21.0$time_period,tbl21.0$LABFORCE)
model21.0 <- chisq.test(tbl21.0)
model21.0
tbl21.1 <- subset(IPUMSdata, METRO1 == 1)
tbl21.1 <- table(tbl21.1$time_period,tbl21.1$LABFORCE)
model21.1 <- chisq.test(tbl21.1)
model21.1

# Marital Status
tbl22.0 <- subset(IPUMSdata, MARST == 1)
tbl22.0 <- table(tbl22.0$time_period,tbl22.0$LABFORCE)
model22.0 <- chisq.test(tbl22.0)
model22.0
tbl22.1 <- subset(IPUMSdata, MARST == 2)
tbl22.1 <- table(tbl22.1$time_period,tbl22.1$LABFORCE)
model22.1 <- chisq.test(tbl22.1)
model22.1
tbl22.2 <- subset(IPUMSdata, MARST == 3)
tbl22.2 <- table(tbl22.2$time_period,tbl22.2$LABFORCE)
model22.2 <- chisq.test(tbl22.2)
model22.2
tbl22.3 <- subset(IPUMSdata, MARST == 4)
tbl22.3 <- table(tbl22.3$time_period,tbl22.3$LABFORCE)
model22.3 <- chisq.test(tbl22.3)
model22.3
tbl22.4 <- subset(IPUMSdata, MARST == 5)
tbl22.4 <- table(tbl22.4$time_period,tbl22.4$LABFORCE)
model22.4 <- chisq.test(tbl22.4)
model22.4
tbl22.5 <- subset(IPUMSdata, MARST == 6)
tbl22.5 <- table(tbl22.5$time_period,tbl22.5$LABFORCE)
model22.5 <- chisq.test(tbl22.5)
model22.5

# Veteran Status
tbl23.0 <- subset(IPUMSdata, VETSTAT == 1)
tbl23.0 <- table(tbl23.0$time_period,tbl23.0$LABFORCE)
model23.0 <- chisq.test(tbl23.0)
model23.0
tbl23.1 <- subset(IPUMSdata, VETSTAT == 2)
tbl23.1 <- table(tbl23.1$time_period,tbl23.1$LABFORCE)
model23.1 <- chisq.test(tbl23.1)
model23.1

# Disability Status
tbl24.0 <- subset(IPUMSdata, DIFFANY == 1)
tbl24.0 <- table(tbl24.0$time_period,tbl24.0$LABFORCE)
model24.0 <- chisq.test(tbl24.0)
model24.0
tbl24.1 <- subset(IPUMSdata, DIFFANY == 2)
tbl24.1 <- table(tbl24.1$time_period,tbl24.1$LABFORCE)
model24.1 <- chisq.test(tbl24.1)
model24.1

# Educational Attainment Status
tbl25.0 <- subset(IPUMSdata, EDUC991 == 0)
tbl25.0 <- table(tbl25.0$time_period,tbl25.0$LABFORCE)
model25.0 <- chisq.test(tbl25.0)
model25.0
tbl25.1 <- subset(IPUMSdata, EDUC991 == 1)
tbl25.1 <- table(tbl25.1$time_period,tbl25.1$LABFORCE)
model25.1 <- chisq.test(tbl25.1)
model25.1
tbl25.2 <- subset(IPUMSdata, EDUC991 == 2)
tbl25.2 <- table(tbl25.2$time_period,tbl25.2$LABFORCE)
model25.2 <- chisq.test(tbl25.2)
model25.2
tbl25.3 <- subset(IPUMSdata, EDUC991 == 3)
tbl25.3 <- table(tbl25.3$time_period,tbl25.3$LABFORCE)
model25.3 <- chisq.test(tbl25.3)
model25.3
tbl25.4 <- subset(IPUMSdata, EDUC991 == 4)
tbl25.4 <- table(tbl25.4$time_period,tbl25.4$LABFORCE)
model25.4 <- chisq.test(tbl25.4)
model25.4
tbl25.5 <- subset(IPUMSdata, EDUC991 == 5)
tbl25.5 <- table(tbl25.5$time_period,tbl25.5$LABFORCE)
model25.5 <- chisq.test(tbl25.5)
model25.5
tbl25.6 <- subset(IPUMSdata, EDUC991 == 6)
tbl25.6 <- table(tbl25.6$time_period,tbl25.6$LABFORCE)
model25.6 <- chisq.test(tbl25.6)
model25.6

# College Attendance
temptbl17 <- subset(IPUMSdata, SCHLCOLL >= 3)
tbl17.0 <- subset(IPUMSdata, SCHLCOLL == 3)
tbl17.0 <- table(tbl17.0$time_period,tbl17.0$LABFORCE)
model17.0 <- chisq.test(tbl17.0)
model17.0
tbl17.1 <- subset(IPUMSdata, SCHLCOLL == 4)
tbl17.1 <- table(tbl17.1$time_period,tbl17.1$LABFORCE)
model17.1 <- chisq.test(tbl17.1)
model17.1
tbl17.2 <- subset(IPUMSdata, SCHLCOLL == 5)
tbl17.2 <- table(tbl17.2$time_period,tbl17.2$LABFORCE)
model17.2 <- chisq.test(tbl17.2)
model17.2
rm(temptbl17)


### ----------------------------------

# END OF ANALYSIS

### ----------------------------------












##### CAN DELETE EVERYTHING BELOW

# Grouped Bar Plot EXAMPLE
#barplot(tbl1, main="Recession Period by Sex",
#        xlab="Pre (0), Current (1), and Post (2) Recession Period", col=c("darkblue","red"),
#        legend = rownames(tbl1), beside=TRUE)

# Grouped Bar Plot using 2 categorical variables EXAMPLE
#library(reshape2)
#library(plyr)

#df1 <- mutate(as.data.frame.matrix(table(IPUMSdata$LABFORCE,IPUMSdata$SEX)), LABFORCE = rownames(as.data.frame.matrix##(table(IPUMSdata$LABFORCE,IPUMSdata$SEX))))
#df1 <- IPUMSdata[c("time_period", "LABFORCE", "SEX")]
#df1 <- melt(IPUMSdata, id.vars = c("time_period", "LABFORCE"), measure.vars = c("SEX"))
#table(c(IPUMSdata$time_period, IPUMSdata$LABFORCE),count(IPUMSdata$SEX))

#p <- ggplot(data = df1, aes(x = LABFORCE, y = value, group = variable, fill = variable))
#p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge")
#p <- p + facet_grid(. ~ time_period)
#p <- p + theme_bw()
#p <- p + theme(axis.text.x = element_text(angle = 90))
#p



# School or college attendance
# Separate your dependent variable according to your grouping (categorical) variable
#tp0<-IPUMSdata$SCHLCOLL[IPUMSdata$time_period==0] #pull out the school/college attendance values that correspond to pre-recession group
#tp1<-IPUMSdata$SCHLCOLL[IPUMSdata$time_period==1] #pull out the school/college attendance values that correspond to recession group
# newModel<-t.test(y-values for group 1, y-values for group 2, var.equal=TRUE/FALSE, paired = FALSE/TRUE)
#ttest1<-t.test(tp0,tp1, var.equal=T, alternative="two.sided")# Assume equal variance
#ttest2<-t.test(tp0,tp1, data=IPUMSdata, var.equal=T, alternative="less") # One-sided
#ttest3<-t.test(tp0,tp1, data=IPUMSdata, var.equal=T, alternative="greater") # One-sided
#ttest4<-t.test(tp0,tp1, data=IPUMSdata, var.equal=F) # Approximation for non-equal variances
#ttest5<-t.test(tp0[1:14],tp1, paired=T) #paired t-test
# Print out results of the t-test (or check contents of the variable)
#ttest1
#ttest2
#ttest3
#ttest4
#ttest5

### PLOTS
# Basic scatterplot ##
#p=ggplot(IPUMSdata,aes(x=factor(time_period), y=factor(SEX)))+
#  geom_point(color="blue")+
#  theme(legend.position="none")
#show(p)
# Basic t-test plot ##
#p=ggplot(IPUMSdata,aes(x=factor(time_period),y=factor(SEX)))+
#  geom_point(aes(color=factor(time_period)))
#show(p)

##### PREDICTIVE ANALYSIS
# train <- df[1:450,]
# test <- df[451:525,]
# model <- glm(time_period ~.,family=binomial(link='logit'),data=train)
# summary(model)

##### LOGISTICAL REGRESSION










rm(list=(ls()))

