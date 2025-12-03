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
toyota %>% filter(mpg>60)
toyota_mpgfix <- toyota_enginefix %>% mutate(mpg=ifelse(mpg>60,60,mpg))
toyota_mpgfix %>% filter(mpg>60)

#multiple toyota hilux entries listed values equalt to 2.8, were replaced with 28mpg
#which is more accurate to projected numbers online for the model
toyota_mpgfix2 <- toyota_mpgfix %>% mutate(mpg=(ifelse(mpg<7,28,mpg)))
summary (toyota_mpgfix2)
toyota_mpgfix2 %>% filter(tax <1 )

#create key
toyota_key <- toyota_mpgfix2 %>% mutate(keyval = row_number())

#split cleaned data into two separate tables, toyota_carspecs and toyota_extinfo
#tax info is ommitted because there were many '0' values and the column will
#not be used, removing these would omit over 100 observations 
Q1 <- "SELECT model, transmission, fueltype, engineSize, mpg, keyval
       FROM toyota_key"
toyota_carspecs <- sqldf(Q1)

Q2 <- "SELECT year, price, mileage, keyval
       FROM toyota_key"
toyota_extinfo <- sqldf(Q2)

ggplot(toyota_key, aes(engineSize,price,color=transmission))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')+facet_wrap('transmission')

ggplot(toyota_key, aes(engineSize,price,color=fuelType))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')+facet_wrap('fuelType')

ggplot(toyota_key, aes(fuelType,fill=fuelType))+geom_bar()
