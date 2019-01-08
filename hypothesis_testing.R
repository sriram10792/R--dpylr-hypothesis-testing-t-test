setwd('H:\\USF fall 2018\\Managerial analytics\\exam files')
install.packages("readxl")



library(readxl)

battery=read_excel("q3_data.xlsx",sheet="Sheet1")

attach(battery)

summary(battery)
#Sonoran

results=t.test(Sonoran,mu=77,alternative = c("two.sided"))
names(results)
results

#Minnesotan

results2=t.test(Minnesotan,mu=77,alternative = c("two.sided"))
names(results2)
results2



#Question 3B

q3_results=t.test(Sonoran,Minnesotan,alternative=c("two.sided"),mu=0,paired=FALSE)
q3_results



#Question 4
attach(housing)

housing=read_excel("q4_data.xlsx",sheet="Real Estate")
q4_results=t.test(Selling,alternative=c("greater"),mu=400000,paired=FALSE)
q4_results


#question 4c

mean_days=mean(Days)
std_days=sd(Days)
count_days=nrow(housing)
count_days

z_score=2.33

error=z_score*std_days/sqrt(count_days)

upper=mean_days+error
lower=mean_days-error

upper
lower


#question 4d
housing$concession=housing$Asking-housing$Selling
housing$concession
attach(housing)
q4d_results=t.test(housing$concession,alternative=c("greater"),mu=120000,paired=FALSE)
q4d_results


#question 5d


photo=read_excel("q5.xlsx",sheet="Sheet1")
attach(photo)

q5d_results=t.test(HarrySteveMorgan,alternative=c("greater"),mu=3.75,paired=FALSE)
q5d_results


#Question 5e

mean_fred=mean(FredCDobbs)
std_fred=sd(FredCDobbs)
count_fred=195

z_score=1.44

error=z_score*std_fred/sqrt(count_fred)

upper=mean_fred+error
lower=mean_fred-error

upper
lower


#Question 5f

mean_charlie=mean(CharlieAllnut)
mean_charlie=6.532846715
std_charlie=sd(CharlieAllnut)
std_charlie=0.538121664
count_charlie=137
#z_score=1.44
error=z_score*std_charlie/sqrt(count_charlie)
upper=6.593648
lower=6.472044
#upper=mean_charlie+error
#lower=mean_charlie-error
error=mean_charlie-lower
#zscore=error*sqrt(count_charlie)/std_charlie
zscore=error*sqrt(count_charlie)/std_charlie
zscore



#Question 5g
photos=read.csv('q5g_data.csv')
attach(photos)
colnames(photos)=c("rick","charlie","fred","harry")

#Rick and charlie
q5g1_results=t.test(photos$rick,photos$charlie,alternative=c("two.sided"),mu=0,paired=FALSE)
q5g1_results


#Rick and Fred
q5g2_results=t.test(photos$fred,photos$harry,alternative=c("two.sided"),paired=TRUE)
q5g2_results




#Question 6 

prison=read_xlsx("q6.xlsx",sheet="Sheet1")

results6=t.test(prison$Months,mu=109,alternative = c("two.sided"))
results6



#Question 7

tvtime=read_xlsx("q7.xlsx",sheet="Television")
attach(tvtime)

#2008 average time is 9 hrs 45 mins which is 585 mins

colnames(tvtime)=c("time")
results7=t.test(tvtime$time,mu=585,alternative = c("less"))
results7





#Question 8
tvhabit=read_xlsx("q8_data.xlsx",sheet="Sheet1")
attach(tvhabit)
#colnames(photos)=c("rick","charlie","fred","harry")

#Detroid Sandiego Miami Buffalo
q8_results=t.test(Miami,Buffalo,alternative=c("two.sided"),mu=0,paired=TRUE)
q8_results


#Paired test


#Question 9
total=1481
friendly=ceiling(0.4*1481)
friendly

#Confidence interval for this friendly range

mean_=mean(Days)
std_days=sd(Days)
count_days=nrow(housing)
count_days

z_score=2.33

error=z_score*std_days/sqrt(count_days)

upper=mean_days+error
lower=mean_days-error

upper
lower



#Question 9B

age18=276
age18_ally=ceiling(.16*276)

age_45=548
age45_ally=ceiling(.10*548)


q9b_results=t.test(age18_ally,age45_ally,alternative=c("two.sided"),mu=0,paired=FALSE)
q9b_results



#Question 1C
q1=read_xlsx("q1_data.xlsx",sheet="Sheet1")
attach(q1)
q1c_results=t.test(French,Garden,alternative=c("two.sided"),mu=0,paired=FALSE)
q1c_results
