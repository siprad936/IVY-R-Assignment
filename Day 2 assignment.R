df1 <- read.csv("C:/Users/TE213958/Desktop/I/R/2/Forest Fires/forestfires.csv")
str(df1)

##1.Compute the square of each data point in the X column 
##and store the result in a new column called "X_square"

df2<-df1
df2$X_Square<-df1$X^2
print(df2)
df1 <- transform(df1, X_square = X^2)

##2.Compute the sum, mean, median, standard deviation of the following columns - 
##a.	FMCC  b.	DMC  c.	DC

library(pastecs)
stat.desc(df1)
summary(df1)


##3.	Create another column called "Month", which has full values of month, 
##i.e "aug" becomes "August", "sep" becomes "September" and so on
?month.name
?month.abb
?state.abb
df3<-df1
df3$Month<-month.name[match(df3$month,month.abb)]
str(df3)

##4.	Create another Column Day_Num where day will be from 1 to 7 
##- 1 being Sunday, 2 being Monday, 3 being Tuesday and so on
?as.Date
df4<-df1
df4$Day_Num<-(format(df4$day, format="%d"))

  
##5.	Find the correlation coefficient X and Y 
?cor
df1
cor(df1$X,df1$Y)

##6.	Find the total rain,wind  for each month
##7.	Find the mean rain,wind  for each month
##8.	Find the number of records present for each month
library(dplyr)
?dplyr
?summarise
df1
summarise(group_by(df1, month), sum_rain=sum(rain),sum_wind=sum(wind),mean_rain=mean(rain),mean_wind=mean(wind),count=n())

##9.Find the number of records for each month-day combo
x<-summarise(group_by(df1, month),count=n())
y<-summarise(group_by(df1, day),count=n())


  