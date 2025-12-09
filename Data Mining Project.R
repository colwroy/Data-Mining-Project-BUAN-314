library(dplyr)
library(ggplot2)
library(sqldf)
toyota <- read.csv('https://raw.githubusercontent.com/colwroy/Data-Mining-Project-BUAN-314/refs/heads/main/toyota.csv')

2/sqrt(6738)

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

#=============================================================================
#=============================================================================

#scatter plots to observe relationships between price and other variables
ggplot(toyota_key,aes(engineSize,price))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')
Q3 <- "SELECT toyota_carspecs.engineSize, 
        AVG(toyota_extinfo.price) AS avg_price_by_engine, 
        COUNT(toyota_carspecs.engineSize) AS count
       FROM toyota_carspecs
       LEFT JOIN toyota_extinfo
       ON toyota_carspecs.keyval = toyota_extinfo.keyval
       GROUP BY engineSize
       ORDER BY engineSize DESC"
toyota_sizenprice <- sqldf(Q3)

ggplot(toyota_key,aes(mileage,price))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(toyota_key,aes(year,price))+
  geom_point(position='jitter')+
  geom_smooth()

ggplot(toyota_key,aes(mpg,price,color=engineSize))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')
Q8 <- "SELECT COUNT(toyota_carspecs.mpg) count_of_MPG_val, toyota_carspecs.mpg
       FROM toyota_carspecs
       GROUP BY toyota_carspecs.mpg
       ORDER BY engineSize DESC"
toyota_mpg <- sqldf(Q8)

#box plot of fuel, transmission, and model categorical variables
ggplot(toyota_key,aes(price,fuelType))+geom_boxplot(fill='blue')
ggplot(toyota_key,aes(price,transmission))+geom_boxplot(fill='red')
ggplot(toyota_key,aes(price,model))+geom_boxplot(fill='yellow')
Q4 <- "SELECT toyota_carspecs.model, AVG(toyota_extinfo.price)
       FROM toyota_carspecs
       LEFT JOIN toyota_extinfo
       ON toyota_carspecs.keyval = toyota_extinfo.keyval
       GROUP BY model"
toyota_modelprice <- sqldf(Q4)

Q5 <- "SELECT toyota_carspecs.model, MAX(toyota_extinfo.price)
       FROM toyota_carspecs
       LEFT JOIN toyota_extinfo
       ON toyota_carspecs.keyval = toyota_extinfo.keyval
       GROUP BY model"
toyota_modelmax <- sqldf(Q5)

#histograms to see the distributions of each of these variables
ggplot(toyota_key,aes(year))+geom_histogram(bins=22,fill='green',color='black')
Q6 <- "SELECT toyota_extinfo.year, COUNT(toyota_extinfo.year)
       FROM toyota_extinfo
       GROUP BY year
       ORDER BY year"
toyota_yearcount <- sqldf(Q6)

ggplot(toyota_key,aes(mpg))+geom_histogram(bins=16,fill='brown',color='black')

ggplot(toyota_key,aes(price))+geom_density(color='blue')
Q7 <- "SELECT toyota_extinfo.price, toyota_carspecs.model
       FROM toyota_extinfo
       LEFT JOIN toyota_carspecs
       ON toyota_carspecs.keyval = toyota_extinfo.keyval
       WHERE price >= 20000"
toyota_above20k <- sqldf(Q7)
ggplot(toyota_key,aes(mileage))+geom_density(color='purple')

#bar charts to see the distributions of the categorical variables
ggplot(toyota_key, aes(fuelType,fill=fuelType))+
  geom_bar()+theme(legend.position = "none")
Q9 <- "SELECT toyota_carspecs.fuelType, 
        COUNT(toyota_carspecs.model), 
        toyota_carspecs.model
       FROM toyota_carspecs
       LEFT JOIN toyota_extinfo
       ON toyota_carspecs.keyval = toyota_extinfo.keyval
       GROUP BY toyota_carspecs.fuelType, toyota_carspecs.model"
toyota_fuelType <- sqldf(Q9)

ggplot(toyota_key, aes(transmission,fill=transmission))+
  geom_bar()+theme(legend.position = "none")

#==============================================================================
#==============================================================================

##LET'S TAKE A LOOK AT THE NUMERIC VARIABLES AND HOW THEY ARE RELATED:
pairs(toyota_key[,c("year", "price","mileage","mpg", "engineSize")])


##LET'S TAKE A LOOK AT THE CORRELATION MATRIX:
cor(toyota_key[,c("year", "price","mileage","mpg", "engineSize")])

##BUILDING A LINEAR MODEL TO QUANTIFY THE RELATIONSHIP BETWEEN PRICE AND MILEAGE##
M1<-lm(price~engineSize,toyota_key)  #model: Sales = B_0+B_1(AdSpend)+e

##MODEL DIAGNOSTICS##
summary(M1) #produces the summary output of the model
confint(M1) #returns upper and lower bounds from the 95% confidence interval for each model parameter


##VISUALIZING OUR RESULTS##
plot(toyota_key$price~toyota_key$engineSize) #scatter plot of price vs. mileage again
abline(M1$coefficients[1], M1$coefficients[2], col='blue', lwd=2) #add regression line to plot

##PLOTTING FITTED (PREDICTED) VALUES
plot(M1$fitted.values~toyota_key$price, add=TRUE)
abline(M1$coefficients[1], M1$coefficients[2], col='blue', lwd=2) #add regression line to plot

##RESIDUAL ANALYSIS##
plot(M1$residuals)
abline(0,0,col='black')
hist(M1$residuals)
summary(M1$residuals)





##BUILD A MULTIVARIATE MODEL TO PREDICT SALES CONTROLLING FOR BOTH ADSPEND AND PROMO##
M3<-lm(price~mpg+engineSize,toyota_key) #model: Sales = B_0+B_1(AdSpend)+B_2(Promo)+e
summary(M3)  #returns summary output for model M3



################ engine + years + mileage
lm(price~engineSize+,toyota_key)




########################
### SQL QUERIES ########
########################
##AVERAGE PRICE BY FUEL TYPE####
sqldf("
  SELECT fuelType, AVG(price) AS avg_price
  FROM toyota_key
  GROUP BY fuelType
  ORDER BY avg_price DESC
")

######################################
####MPG vs. engine size averages######
######################################
sqldf("
  SELECT engineSize, AVG(mpg) AS avg_mpg
  FROM toyota_key
  GROUP BY engineSize
  ORDER BY engineSize
")


#######################################
####PRICE BY TRANSIMISISON#############
########################################
sqldf("
   SELECT transmission, AVG(price) AS avg_price
   FROM  toyota_key
   GROUP BY transmission
   ORDER BY avg_price DESC
     
")


###############################################
###MILEAGE VS. PRICE GROUPED BY ENGINE SIZE ###
###############################################
sqldf("
   SELECT engineSize,
          AVG(mileage) AS avg_mileage,
          AVG(price) AS avg_price
   FROM   toyota_key
   GROUP BY engineSize
   ORDER BY avg_mileage DESC
   
")

###############################
##
################################
###############################
##TRANSMISSION TYPE BY MODEL###
###############################      
sqldf("
  SELECT model,
         transmission,
       COUNT(*) AS count_transmissions
  FROM  toyota_carspecs
  GROUP BY model, transmission
  ORDER BY model, transmission    

