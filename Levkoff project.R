############################################################
## 0. SETUP
############################################################

# Install packages once if you have not already
# install.packages("tidyverse")
# install.packages("sqldf")

library(tidyverse)
library(sqldf)

############################################################
## 1. LOAD RAW DATA
############################################################

# Set working directory if needed
# setwd("path/to/your/folder")

toyota_raw <- read.csv("https://raw.githubusercontent.com/colwroy/Data-Mining-Project-BUAN-314/refs/heads/main/toyota.csv", stringsAsFactors = FALSE)

# Quick check
str(toyota_raw)
head(toyota_raw)

############################################################
## 2. BASIC CLEANING AND FEATURE ENGINEERING
############################################################

# Clean column names and trim spaces from character columns
toyota <- toyota_raw %>%
  rename_with(~ trimws(.x)) %>%     # remove stray spaces in names
  mutate(across(where(is.character), ~ trimws(.x)))

# Create Age (assuming current year is 2025)
toyota <- toyota %>%
  mutate(
    Age = 2025 - year,
    # Transmission: Automatic = 1, Manual or Semi Auto = 0
    Automatic = if_else(grepl("Auto", transmission, ignore.case = TRUE), 1, 0),
    # convert miles to kilometers
    KM = mileage * 1.60934
  )

# Check summary
summary(toyota)

############################################################
## 3. REMOVE OUTLIERS
############################################################

# Simple rule based trimming
# You can tweak thresholds if you like
toyota_clean <- toyota %>%
  filter(
    price > 2000,
    price < 60000,
    mileage >= 0,
    mileage < 200000
  )

summary(toyota_clean$price)
summary(toyota_clean$mileage)

############################################################
## 4. CREATE DOORS VARIABLE (BASED ON MODEL)
############################################################

three_door_models <- c("Aygo", "GT86", "Supra")
five_door_models <- c(
  "Yaris", "Auris", "Avensis", "Prius", "Corolla",
  "Verso", "C-HR", "PROACE VERSO", "RAV4",
  "Land Cruiser", "Hilux", "Camry"
)

toyota_clean <- toyota_clean %>%
  mutate(
    Doors = case_when(
      model %in% three_door_models ~ 3L,
      model %in% five_door_models ~ 5L,
      TRUE ~ 5L      # default assumption
    )
  )

table(toyota_clean$Doors)

############################################################
## 5. CREATE CarID AND SPLIT INTO TWO TABLES
############################################################

toyota_clean <- toyota_clean %>%
  arrange(model, year, price) %>%
  mutate(CarID = row_number())

# CarSpecs table (features)
CarSpecs <- toyota_clean %>%
  select(
    CarID,
    model,
    year,
    Age,
    mileage,
    KM,
    engineSize,
    fuelType,
    transmission,
    Automatic,
    Doors
  )

# Pricing table (price and tax)
Pricing <- toyota_clean %>%
  select(
    CarID,
    price,
    tax
    # If you had other guarantee fields you would include them here
    # MfrGuarantee,
    # BOVAGGuarantee
  )

# Joined table for most analysis
Cars <- CarSpecs %>%
  inner_join(Pricing, by = "CarID")

head(CarSpecs)
head(Pricing)
head(Cars)

############################################################
## 6. VISUALIZATIONS (USING ggplot2)
############################################################

# Visualization 1: Price vs Age (Scatter with smooth line)
ggplot(Cars, aes(x = Age, y = price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Price vs Age of Used Toyota Vehicles",
    x = "Age (years)",
    y = "Price"
  )

# Visualization 2: Histogram of Price Distribution
ggplot(Cars, aes(x = price)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Used Toyota Prices",
    x = "Price",
    y = "Count"
  )

# Visualization 3: Boxplot of Price by Fuel Type
ggplot(Cars, aes(x = fuelType, y = price)) +
  geom_boxplot() +
  labs(
    title = "Price by Fuel Type",
    x = "Fuel Type",
    y = "Price"
  )

# Visualization 4: Average Price by Doors (Bar chart)
avg_price_doors <- Cars %>%
  group_by(Doors) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

ggplot(avg_price_doors, aes(x = factor(Doors), y = avg_price)) +
  geom_col() +
  labs(
    title = "Average Price by Number of Doors",
    x = "Number of Doors",
    y = "Average Price"
  )

# Visualization 5: Bubble Chart Price vs KM, size = engineSize
ggplot(Cars, aes(x = KM, y = price, size = engineSize)) +
  geom_point(alpha = 0.4) +
  scale_size_continuous(name = "Engine Size") +
  labs(
    title = "Price vs Kilometers Driven",
    x = "Kilometers (KM)",
    y = "Price"
  )

# Visualization 6: Faceted Histogram of Price by Transmission Type
Cars <- Cars %>%
  mutate(
    TransCategory = if_else(Automatic == 1, "Automatic", "Manual or Semi Auto")
  )

ggplot(Cars, aes(x = price)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ TransCategory) +
  labs(
    title = "Price Distribution by Transmission Type",
    x = "Price",
    y = "Count"
  )

############################################################
## 7. SQL QUERIES USING sqldf
############################################################

# For sqldf, we work on data frames available in the global environment.
# Make sure CarSpecs, Pricing, and Cars exist.

# Query 1: Top 15 most expensive cars
query1 <- "
SELECT c.model, c.year, p.price, c.mileage, c.fuelType, c.transmission
FROM Cars c
JOIN Pricing p ON c.CarID = p.CarID
ORDER BY p.price DESC
LIMIT 15
"
q1_result <- sqldf(query1)
q1_result

# Query 2: Average price by FuelType
query2 <- "
SELECT fuelType, AVG(price) AS avg_price
FROM Cars
GROUP BY fuelType
ORDER BY avg_price DESC
"
q2_result <- sqldf(query2)
q2_result

# Query 3: Average mileage by model
query3 <- "
SELECT model, AVG(mileage) AS avg_mileage
FROM Cars
GROUP BY model
ORDER BY avg_mileage
"
q3_result <- sqldf(query3)
head(q3_result, 10)

# Query 4: Cars with highest engine size (proxy for horsepower)
query4 <- "
SELECT model, year, engineSize, price
FROM Cars
ORDER BY engineSize DESC, price DESC
LIMIT 10
"
q4_result <- sqldf(query4)
q4_result

# Query 5: Average engine size by number of doors
query5 <- "
SELECT Doors, AVG(engineSize) AS avg_engineSize
FROM Cars
GROUP BY Doors
ORDER BY Doors
"
q5_result <- sqldf(query5)
q5_result

# Query 6: Price quartiles (min, Q1, median, Q3, max)
# sqldf itself does not have percentile functions, so we show summary using R
price_summary <- summary(Cars$price)
price_summary

# Query 7: Cars older than 10 years with mileage < 50000
query7 <- "
SELECT model, year, Age, mileage, price
FROM Cars
WHERE Age > 10 AND mileage < 50000
ORDER BY price DESC
"
q7_result <- sqldf(query7)
head(q7_result, 20)

# Query 8: Cheapest automatic cars
query8 <- "
SELECT model, year, mileage, price
FROM Cars
WHERE Automatic = 1
ORDER BY price ASC
LIMIT 10
"
q8_result <- sqldf(query8)
q8_result

# Query 9: Most common models (used as a proxy for colors if not present)
query9 <- "
SELECT model, COUNT(*) AS count_model
FROM Cars
GROUP BY model
ORDER BY count_model DESC
"
q9_result <- sqldf(query9)
head(q9_result, 10)

# Query 10: Average tax by FuelType
query10 <- "
SELECT fuelType, AVG(tax) AS avg_tax
FROM Cars
GROUP BY fuelType
ORDER BY avg_tax DESC
"
q10_result <- sqldf(query10)
q10_result

# Query 11: Correlation helper style summary by model
query11 <- "
SELECT model,
       AVG(price) AS avg_price,
       AVG(Age) AS avg_age,
       AVG(mileage) AS avg_mileage,
       AVG(engineSize) AS avg_engineSize
FROM Cars
GROUP BY model
ORDER BY avg_price DESC
"
q11_result <- sqldf(query11)
head(q11_result, 10)

# Query 12: Total inventory value
query12 <- "
SELECT SUM(price) AS TotalInventoryValue
FROM Cars
"
q12_result <- sqldf(query12)
q12_result

############################################################
## 8. CORRELATION AND REGRESSION MODEL
############################################################

# Select numeric variables for correlation matrix
numeric_vars <- Cars %>%
  select(price, Age, KM, mileage, tax, engineSize, Automatic)

cor_matrix <- cor(numeric_vars)
cor_matrix

# Regression model: Price ~ Age + KM + engineSize + Automatic
price_model <- lm(price ~ Age + KM + engineSize + Automatic, data = Cars)
summary(price_model)

# You can also save model residuals or predictions if needed
Cars$pred_price <- predict(price_model, newdata = Cars)
Cars$residual <- Cars$price - Cars$pred_price

# Quick check of predicted vs actual
head(Cars %>% select(model, year, price, pred_price, residual))
``

