setwd('C:\\Users\\Ramesh Narayanan\\Downloads')
data=read.csv('Bankruptcy.csv')
head(data)

hist(data$ROA)

#It looks like a normal distribution with left skewed data

#Calculate the mean
mean_ROA=mean(data$ROA)

t.test(data$ROA)

size=46

std_ROA=sd(data$ROA)
zscore=1.96

margin_error=zscore*std_ROA/sqrt(size)

highest_ROA=mean_ROA+margin_error
print(highest_ROA)

lowest_ROA=mean_ROA-margin_error
print(lowest_ROA)


#Question 2
head(data)

bankrupt_data=ifelse(data$Status='Bankrupt')

bankrupt_data=subset(data,data$Status=="Bankrupt")
solvent_data=subset(data,data$Status=="Solvent")


#Reindexing the bankrupt dataframe
rownames(bankrupt_data) <- 1:nrow(bankrupt_data)
bankrupt_data


#Reindexing the solvent dataframe
rownames(solvent_data) <- 1:nrow(solvent_data)
solvent_data


#Calculate the confidence scores of the two datasets

t.test(bankrupt_data$ROA)

t.test(solvent_data$ROA)
