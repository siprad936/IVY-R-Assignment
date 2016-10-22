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
df1$Month_Full <-
sapply(df1$month,function(x)
{
if(x=="jan")
  {
   x <- as.factor("January")
}
  if(x=="feb")
  {
    x <- as.factor("February")
  }
  if(x=="mar")
  {
    x <- as.factor("March")
  }
  if(x=="apr")
  {
    x <- as.factor("April")
  }
  if(x=="may")
  {
    x <- as.factor("May")
  }
  if(x=="jun")
  {
    x <- as.factor("June")
  }
  if(x=="jul")
  {
    x <- as.factor("July")
  }
  if(x=="aug")
  {
    x <- as.factor("August")
  }
  if(x=="sep")
  {
    x <- as.factor("September")
  }
  if(x=="oct")
  {
    x <- as.factor("October")
  }
  if(x=="nov")
  {
    x <- as.factor("November")
  }
  if(x=="dec")
  {
    x <- as.factor("December")
  }
 return( x )
})

df3<-df1
df3$Month<-month.name[match(df3$month,month.abb)]
str(df3)

##4.	Create another Column Day_Num where day will be from 1 to 7 
##- 1 being Sunday, 2 being Monday, 3 being Tuesday and so on
?as.Date
df4<-df1
df4$Day_Num<-(format(df4$day, format="%d"))

df1$Day_Num <-
  sapply(df1$day,function(x)
  {
    if(x=="sun")
    {
      x <- as.factor("1")
    }
    if(x=="mon")
    {
      x <- as.factor("2")
    }
    if(x=="tue")
    {
      x <- as.factor("3")
    }
    if(x=="wed")
    {
      x <- as.factor("4")
    }
    if(x=="thu")
    {
      x <- as.factor("5")
    }
    if(x=="fri")
    {
      x <- as.factor("6")
    }
    if(x=="sat")
    {
      x <- as.factor("7")
    }
    
    return( x )
  })

  
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


  