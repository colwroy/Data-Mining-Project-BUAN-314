############################################################
## 0. SETUP
############################################################

# This script analyzes a used Toyota car dataset.
# It:
#  - Cleans and prepares the raw data
#  - Creates engineered features (Age, Doors, Automatic flag)
#  - Splits the data into CarSpecs and Pricing tables
#  - Builds visualizations in ggplot2
#  - Runs SQL-style summaries using sqldf
#  - Fits a regression model to predict price

# Packages:
#  - tidyverse: data wrangling (dplyr), plotting (ggplot2), etc.
#  - sqldf: lets us run SQL queries directly on data frames in R.
# install.packages("tidyverse")
# install.packages("sqldf")

library(tidyverse)
library(sqldf)

############################################################
## 1. LOAD RAW DATA
############################################################

# If you were working locally, you could set your working directory:
# setwd("path/to/your/folder")

# Here we load the Toyota dataset directly from GitHub.
# This keeps the project reproducible—anyone running the script
# will pull the same dataset from the same URL.
toyota_raw <- read.csv(
  "https://raw.githubusercontent.com/colwroy/Data-Mining-Project-BUAN-314/refs/heads/main/toyota.csv",
  stringsAsFactors = FALSE
)

# Quick structure and preview checks:
#  - str() shows variable types and column names
#  - head() shows the first few rows to confirm the data looks right
str(toyota_raw)
head(toyota_raw)

############################################################
## 2. BASIC CLEANING AND FEATURE ENGINEERING
############################################################

# STEP 1: Clean column names and string values.
#  - Some datasets have extra spaces in names or categories.
#  - We trim whitespace from:
#    * column names
#    * all character columns (model, fuelType, transmission, etc.)
toyota <- toyota_raw %>%
  rename_with(~ trimws(.x)) %>%                 # remove stray spaces in column names
  mutate(across(where(is.character), trimws))   # trim spaces in all character fields

# STEP 2: Create new variables that will be useful in analysis.
#  - Age: more intuitive than raw "year" when studying depreciation.
#  - Automatic: a binary indicator for transmission group:
#       Automatic / Semi-Auto  -> 1
#       Manual                 -> 0
#    This groups Automatic and Semi-Auto together, since they
#    behave more similarly than Manual does.
toyota <- toyota %>%
  mutate(
    # Age of the car in years, assuming "current year" is 2025
    Age = 2025 - year,
    
    # Automatic flag:
    #  1 = Automatic or Semi-Auto
    #  0 = Manual (or anything else defaulted to Manual-like)
    Automatic = case_when(
      transmission %in% c("Automatic", "Semi-Auto") ~ 1L,
      transmission == "Manual" ~ 0L,
      TRUE ~ 0L  # fallback if there are weird/unknown labels
    )
  )

# Quick summary to verify ranges and catch obvious issues
summary(toyota)

############################################################
## 3. REMOVE OUTLIERS
############################################################

# Outliers can distort visualizations and regression results.
# Here we remove:
#  - Unrealistic prices (below 2k or above 60k)
#  - Unrealistic mileages (negative or over 200k miles)
#  - ONE specific outlier: a 1998 car priced at 19,990
#    which is skewing the Price vs Age scatterplot.
toyota_clean <- toyota %>%
  filter(
    price > 2000,       # keep cars priced above $2,000
    price < 60000,      # and below $60,000
    mileage >= 0,       # mileage must be non-negative
    mileage < 200000    # and under 200,000 miles
  ) %>%
  # Explicitly drop the single outlier row that is 1998 and 19,990.
  # This line targets ONLY that combination (year == 1998 AND price == 19990).
  filter(!(year == 1998 & price == 19990))

# Check the trimmed distributions of price and mileage
summary(toyota_clean$price)
summary(toyota_clean$mileage)

############################################################
## 4. CREATE DOORS VARIABLE (2-door vs 4-door)
############################################################

# The original dataset does NOT contain a Doors column.
# Number of doors is, however, an intuitive feature:
#  - 2-door cars are often sportier, less practical, possibly priced differently.
#  - 4-door cars are more family/utility oriented.
# We approximate the number of doors based on the model name.

# Models typically sold as 2-door (sports / coupes):
two_door_models <- c("GT86", "Supra")

# Models typically sold as 4-door (sedans, hatchbacks, SUVs, trucks):
four_door_models <- c(
  "Aygo", "Yaris", "Auris", "Avensis", "Prius",
  "Corolla", "Verso", "C-HR", "PROACE VERSO",
  "RAV4", "Land Cruiser", "Hilux", "Camry"
)

# Create the Doors variable:
#  - 2 if model is in two_door_models
#  - 4 if model is in four_door_models
#  - default to 4 otherwise (safe assumption for unknowns)
toyota_clean <- toyota_clean %>%
  mutate(
    Doors = case_when(
      model %in% two_door_models ~ 2L,
      model %in% four_door_models ~ 4L,
      TRUE ~ 4L
    )
  )

# Inspect distribution of the new Doors variable
table(toyota_clean$Doors)

############################################################
## 5. CREATE CarID AND SPLIT INTO TWO TABLES
############################################################

# We mimic the structure of the UberEats example project:
#  - Create a unique CarID for each row
#  - Split into:
#      CarSpecs  = vehicle characteristics (features)
#      Pricing   = price-related fields (targets / outcomes)

toyota_clean <- toyota_clean %>%
  arrange(model, year, price) %>%   # consistent ordering
  mutate(CarID = row_number())      # unique identifier per car

# CarSpecs table: features describing each car.
# Note: We keep mileage in miles and do NOT create a KM variable.
CarSpecs <- toyota_clean %>%
  select(
    CarID,
    model,
    year,
    Age,
    mileage,        # mileage in miles (no KM)
    engineSize,
    fuelType,
    transmission,
    Automatic,
    Doors
  )

# Pricing table: outcome / monetary variables.
Pricing <- toyota_clean %>%
  select(
    CarID,
    price,
    tax
    # If the dataset had other price-related columns (e.g., guarantees),
    # they would be added here as well.
  )

# Combined Cars table: used for most plots, SQL, and modeling.
# This is analogous to joining Main and Fare tables in the example report.
Cars <- CarSpecs %>%
  inner_join(Pricing, by = "CarID")

# Quick sanity checks on each table
head(CarSpecs)
head(Pricing)
head(Cars)

############################################################
## 6. VISUALIZATIONS (USING ggplot2)
############################################################
# These plots support the descriptive goal:
# "Identify which car features most strongly influence price."

# Visualization 1: Price vs Age (Scatter with smooth line)
# This shows how car price tends to decrease as Age increases
# (classic depreciation pattern).
ggplot(Cars, aes(x = Age, y = price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Price vs Age of Used Toyota Vehicles",
    x = "Age (years)",
    y = "Price"
  )

# Visualization 2: Histogram of Price Distribution
# This shows the overall shape of price values (center, spread, skewness).
ggplot(Cars, aes(x = price)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Used Toyota Prices",
    x = "Price",
    y = "Count"
  )

# Visualization 3: Boxplot of Price by Fuel Type
# This compares medians and variability of price across fuel types
# (e.g., petrol vs diesel vs hybrid).
ggplot(Cars, aes(x = fuelType, y = price)) +
  geom_boxplot() +
  labs(
    title = "Price by Fuel Type",
    x = "Fuel Type",
    y = "Price"
  )

# Visualization 4: Average Price by Doors (Bar chart)
# This compares average prices for 2-door vs 4-door cars,
# highlighting how body style relates to price.
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

# Visualization 5: Bubble Chart Price vs Mileage, size = engineSize
# This shows how price varies with mileage (in miles) and engine size.
#  - x-axis: mileage (higher mileage usually lowers price)
#  - y-axis: price
#  - bubble size: engineSize (larger engines often more expensive)
ggplot(Cars, aes(x = mileage, y = price, size = engineSize)) +
  geom_point(alpha = 0.4) +
  scale_size_continuous(name = "Engine Size") +
  labs(
    title = "Price vs Miles Driven",
    x = "Mileage (miles)",
    y = "Price"
  )

# Before Visualization 6, we define a transmission category that
# groups Automatic + Semi-Auto together vs Manual.
# This is easier to interpret than three separate categories.
Cars <- Cars %>%
  mutate(
    TransCategory = if_else(
      transmission %in% c("Automatic", "Semi-Auto"),
      "Automatic / Semi-Auto",
      "Manual"
    )
  )

# Visualization 6: Faceted Histogram of Price by Transmission Category
# Compares the price distributions for:
#  - Automatic / Semi-Auto
#  - Manual
# This helps see whether automatics tend to sell at higher prices.
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
# This section uses SQL to summarize and explore the data,
# similar to the structure in the UberEats example report.
# Each query supports specific descriptive questions.

# Query 1: Top 15 most expensive cars
# Question: Which models show up at the very top of the price range?
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
# Question: How does average price differ across fuel types?
query2 <- "
SELECT fuelType, AVG(price) AS avg_price
FROM Cars
GROUP BY fuelType
ORDER BY avg_price DESC
"
q2_result <- sqldf(query2)
q2_result

# Query 3: Average mileage by model
# Question: Which models tend to be driven more (higher average mileage)?
query3 <- "
SELECT model, AVG(mileage) AS avg_mileage
FROM Cars
GROUP BY model
ORDER BY avg_mileage
"
q3_result <- sqldf(query3)
head(q3_result, 10)

# Query 4: Cars with highest engine size (proxy for horsepower)
# Question: Which models represent the high-performance end of the lineup?
query4 <- "
SELECT model, year, engineSize, price
FROM Cars
ORDER BY engineSize DESC, price DESC
LIMIT 10
"
q4_result <- sqldf(query4)
q4_result

# Query 5: Average engine size by number of doors
# Question: Do 2-door cars tend to have larger engines than 4-door cars?
query5 <- "
SELECT Doors, AVG(engineSize) AS avg_engineSize
FROM Cars
GROUP BY Doors
ORDER BY Doors
"
q5_result <- sqldf(query5)
q5_result

# Query 6: Price summary (min, Q1, median, Q3, max)
# We use R's summary() as a quick "quartile report" for prices.
price_summary <- summary(Cars$price)
price_summary

# Query 7: Cars older than 10 years with mileage < 50000
# Question: Are there older cars that still have relatively low mileage?
# These might represent "good deals" in the used market.
query7 <- "
SELECT model, year, Age, mileage, price
FROM Cars
WHERE Age > 10 AND mileage < 50000
ORDER BY price DESC
"
q7_result <- sqldf(query7)
head(q7_result, 20)

# Query 8: Cheapest automatic / semi-auto cars
# Question: What are the most affordable options if a buyer insists on an automatic?
query8 <- "
SELECT model, year, mileage, price
FROM Cars
WHERE Automatic = 1
ORDER BY price ASC
LIMIT 10
"
q8_result <- sqldf(query8)
q8_result

# Query 9: Most common models
# Question: Which Toyota models appear most often in this dataset?
query9 <- "
SELECT model, COUNT(*) AS count_model
FROM Cars
GROUP BY model
ORDER BY count_model DESC
"
q9_result <- sqldf(query9)
head(q9_result, 10)

# Query 10: Average tax by FuelType
# Question: How do ownership costs (tax) differ by fuel type?
query10 <- "
SELECT fuelType, AVG(tax) AS avg_tax
FROM Cars
GROUP BY fuelType
ORDER BY avg_tax DESC
"
q10_result <- sqldf(query10)
q10_result

# Query 11: Summary by model (price, age, mileage, engine size)
# Question: Which models stand out as more expensive, newer/older,
#           more/less driven, or having larger engines?
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
# Question: What is the total dollar value of all used Toyotas in this dataset?
query12 <- "
SELECT SUM(price) AS TotalInventoryValue
FROM Cars
"
q12_result <- sqldf(query12)
q12_result

############################################################
## 8. CORRELATION AND REGRESSION MODEL
############################################################
# This section supports the predictive goal:
# "Predict car price from age, mileage, engine size, and transmission type."

# STEP 1: Select numeric variables for correlation analysis.
# We include:
#  - price: target variable
#  - Age: car age in years
#  - mileage: odometer reading in miles
#  - tax: yearly tax cost
#  - engineSize: engine displacement
#  - Automatic: 1 = Automatic/Semi-Auto, 0 = Manual
numeric_vars <- Cars %>%
  select(price, Age, mileage, tax, engineSize, Automatic)

# Correlation matrix helps show:
#  - which predictors are strongly related to price
#  - which predictors are strongly related to each other
cor_matrix <- cor(numeric_vars)
cor_matrix

# STEP 2: Build the regression model.
# Formula:
#  price ~ Age + mileage + engineSize + Automatic
# Interpretation:
#  - Age: holding others constant, older cars should have lower prices.
#  - mileage: more miles driven should reduce price.
#  - engineSize: larger engines often command higher prices.
#  - Automatic: automatic/semi-auto (1) often has a price premium vs manual (0).
price_model <- lm(price ~ Age + mileage + engineSize + Automatic, data = Cars)

# Model summary output includes:
#  - Coefficients for each predictor
#  - p-values (statistical significance)
#  - R-squared (how much variance in price is explained by the model)
summary(price_model)

# STEP 3: Add predictions and residuals back into the Cars table.
#  - pred_price: fitted values from the model
#  - residual: actual price - predicted price
# These can be used for diagnostic plots or identifying under/overpriced cars.
Cars$pred_price <- predict(price_model, newdata = Cars)
Cars$residual <- Cars$price - Cars$pred_price

# Quick check of actual vs predicted prices for a few cars
head(Cars %>% select(model, year, price, pred_price, residual))