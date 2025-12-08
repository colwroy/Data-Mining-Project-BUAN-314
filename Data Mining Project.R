library(dplyr)
library(ggplot2)
library(sqldf)
toyota <- read.csv('https://raw.githubusercontent.com/colwroy/Data-Mining-Project-BUAN-314/refs/heads/main/toyota.csv')

2/sqrt(6738)

summary(toyota)
toyota %>% filter(engineSize<1)
#some vehicles have zero L displacement, imputing 1 L as replacement value
toyota_enginefix <- toyota %>% mutate(engineSize=ifelse(engineSize<1,1,engineSize))
toyota_enginefix %>% filter(engineSize<1)

#Prius cars and others all had greatly inflated mpg values, 
#researched values are all is closer to 50-60 mpg. any values above 60 are imputed as 60
#for closer accuracy to real values
toyota_mpgfix <- toyota_enginefix %>% mutate(mpg=ifelse(mpg>60,60,mpg))

#multiple toyota hilux entries listed values equalt to 2.8, were replaced with 28mpg
#which is more accurate to projected numbers online for the model
toyota_mpgfix2 <- toyota_mpgfix %>% mutate(mpg=(ifelse(mpg<7,28,mpg)))

#create key
toyota_key <- toyota_mpgfix2 %>% mutate(keyval = row_number())
#export data frame
write.csv(toyota_key, file = 'data=toyota_key.csv')
getwd()
#split cleaned data into two separate tables, toyota_carspecs and toyota_extinfo
#tax info is ommitted because there were many '0' values and the column will
#not be used, removing these would omit over 100 observations 
Q1 <- "SELECT model, transmission, fueltype, engineSize, mpg, keyval
       FROM toyota_key"
toyota_carspecs <- sqldf(Q1)

Q2 <- "SELECT year, price, mileage, keyval
       FROM toyota_key"
toyota_extinfo <- sqldf(Q2)

#three plots to observe relationships between price and other variables
ggplot(toyota_key,aes(engineSize,price))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')

ggplot(toyota_key,aes(mileage,price))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(toyota_key,aes(year,price))+
  geom_point(position='jitter')+
  geom_smooth()

ggplot(toyota_key,aes(mpg,price,color=engineSize))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')

#box plot of fuel, transmission, and model categorical variables
ggplot(toyota_key,aes(price,fuelType))+geom_boxplot()
ggplot(toyota_key,aes(price,transmission))+geom_boxplot()
ggplot(toyota_key,aes(price,model))+geom_boxplot(fill='yellow')

#histograms to see the distributions of each of these variables
ggplot(toyota_key,aes(year))+geom_histogram(bins=22,fill='green',color='black')
ggplot(toyota_key,aes(price))+geom_density(color='blue')
ggplot(toyota_key,aes(mpg))+geom_histogram(bins=16,fill='brown',color='black')
ggplot(toyota_key,aes(mileage))+geom_density(color='purple')

#bar charts to see the distributions of the categorical variables
ggplot(toyota_key, aes(fuelType,fill=fuelType))+
  geom_bar()
ggplot(toyota_key, aes(transmission,fill=transmission))+
  geom_bar()



