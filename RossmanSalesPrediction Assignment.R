setwd("C:/Users/TE213958/Desktop/I/R/4/")

train <- read.csv("Assignment/train.csv", header = T)
test <- read.csv("Assignment/test.csv", header = T)
store <- read.csv("Assignment/store.csv")

?merge
train <- merge(train,store)
test <- merge(test,store)



munge_data <- function(dt){
  

    # replacing NA's by the mean value  
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
  dt$Open[is.na(dt$Open)] = round(mean(dt$Open, na.rm = T))
  
  # converting to numeric
  dt$StateHoliday = as.numeric(dt$StateHoliday)
  dt$StoreType = as.numeric(dt$StoreType)
  dt$Assortment = as.numeric(dt$Assortment)
  dt$PromoInterval = as.numeric(dt$PromoInterval)
  
  
  # seperating out the elements of the date column for the dt set
  dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
  dt$month <- as.integer(format(dt$Date, "%m"))
  dt$year <- as.integer(format(dt$Date, "%y"))
  dt$day <- as.integer(format(dt$Date, "%d"))
  
  # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
  dt$Date = NULL
  dt$Customers = NULL
  dt$CompetitionOpenSinceYear = NULL
  
  return(dt)
}


train = munge_data(train)
test = munge_data(test)


cor(train)


?ggplot
ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point() + 
  labs(title = "Customers and Sales")


ggplot(train[Sales != 0], aes(x = factor(DayOfWeek), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) + 
  labs(title = "Sales by Day and Year") +
  facet_wrap(~ Year)
names(train)
# Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
fit = lm(Sales ~ DayOfWeek+Open+StateHoliday+SchoolHoliday+StoreType+Assortment+CompetitionDistance+CompetitionOpenSinceMonth
         +Promo+Promo2SinceWeek+PromoInterval+year+month, data = train)
step <- stepAIC(fit, direction="both")
step$anova # display results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics



fit = lm(Sales ~ DayOfWeek+Open+StateHoliday+SchoolHoliday+StoreType+Assortment+CompetitionDistance+CompetitionOpenSinceMonth
         +Promo+Promo2SinceWeek+PromoInterval+year+month, data = train)
summary(fit)
summary(mod)
y = predict(fit, newdata = test)

submissionRoss <- data.frame(Id=test$Id, Sales=y)

write.csv(submissionRoss, "lm.csv")
