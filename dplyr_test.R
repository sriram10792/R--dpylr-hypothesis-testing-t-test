library(dplyr)

annual_data2 <- data.frame(
  person=c(1, 1, 1, 2, 2),
  year=c(2010, 2011, 2012, 2010, 2012),
  y=c(1, 2, 3, 1, 3)
)



typeof(annual_data2)
attributes(annual_data)


annual_data=read.csv(file.choose())


data2=annual_data[,0:3]
head(data2)
annual_data=data2

colnames()
year=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
y=annual_data$Traveltimetowork
colnames(annual_data)=c("person","year","y")
head(annual_data)
annual_data=annual_data[,0:3]



expand_data <- function(x) {
  years <- min(x$year):max(x$year)
  quarters <- 1:12
  grid <- expand.grid(quarter=quarters, year=years)
  x$quarter <- 1
  merged <- grid %>% left_join(x, by=c('year', 'quarter'))
  merged$person <- x$person[1]
  return(merged)
}

interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$y
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$yhat <- interpolation$y
  return(data)
}

expand_and_interpolate <- function(x) interpolate_data(expand_data(x))

quarterly_data <- annual_data %>% group_by(person) %>% do(expand_and_interpolate(.))

print(as.data.frame(quarterly_data))





ger <- data.frame(DATE= as.Date(c("1991-01-01", "1991-04-01", "1991-07-01", "1991-10-01", "1992-01-01" )),VALUE= c(470780, 468834, 466332, 472949, 480359))
head(ger)
DateSeq <- seq(ger$DATE[1],tail(ger$DATE,1),by="1 month")
gerMonthly <- data.frame(DATE=DateSeq, Interp.Value=spline(ger, method="natural", xout=DateSeq)$y)
merge(ger, gerMonthly, by='DATE', all.y = T)


tail(ger)
tail(gerMonthly)


library(tempdisagg)
demo(tempdisagg)

data(swisspharma)

data2=swisspharma
head(swisspharma)

