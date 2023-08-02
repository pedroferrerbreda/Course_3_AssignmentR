## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

# Week 4 assignment: EDA using R

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)

# Import the data set.
sales = read.csv(file.choose(), header=T)

# Print the data frame.
head(sales)
summary(sales)
# the summary shows two rows have missing values so we'll drop them
sales <- sales[complete.cases(sales), ]
# now there are no missing values
summary(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales2)

# View the descriptive statistics.
summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, EU_Sales, data=sales2)
qplot(NA_Sales, Global_Sales, data=sales2)
qplot(EU_Sales, Global_Sales, data=sales2)

## 2b) Histograms
# Create histograms.
qplot(EU_Sales, data=sales2)
qplot(NA_Sales, data=sales2)
qplot(Global_Sales, data=sales2)

## 2c) Boxplots
# Create boxplots.
qplot(EU_Sales, data=sales2, geom='boxplot')
qplot(NA_Sales, data=sales2, geom='boxplot')
qplot(Global_Sales, data=sales2, geom='boxplot')
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# Both scatterplots show a positive relationship of EU and NA sales with global sales with very similarly sized y axis, suggesting that EU and NA sales may be of similar importance. The graph shows some clear outliers on the top right corner. This suggests there are some significant best-selling products.
# All three histograms show a very right skewed distribution for sales. Most observations are below 20k for global sales, and for both EU and NA, most observations are below 10k.
# Boxplots provide a similar insight to histograms in this case.
###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales2)

# Check output: Determine the min, max, and mean values.
min(sales2$EU_Sales)
min(sales2$NA_Sales)
min(sales2$Global_Sales)
max(sales2$EU_Sales)
max(sales2$NA_Sales)
max(sales2$Global_Sales)
mean(sales2$EU_Sales)
mean(sales2$NA_Sales)
mean(sales2$Global_Sales)
# View the descriptive statistics.
summary(sales2$EU_Sales)
summary(sales2$NA_Sales)
summary(sales2$Global_Sales)
###############################################################################

# 2. Determine the impact on sales per product_id.
library(dplyr)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_grouped <- sales2 %>% 
  group_by(Product) %>% 
  summarize(sum_global = sum(Global_Sales, na.rm = TRUE),
            sum_EU = sum(EU_Sales, na.rm = TRUE),
            sum_NA = sum(NA_Sales, na.rm = TRUE))



# View the data frame.
head(sales_grouped)

# check for missing values
sum(is.na(sales_grouped))

# Explore the data frame.
summary(sales_grouped$sum_EU)
summary(sales_grouped$sum_NA)
summary(sales_grouped$sum_global)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, sum_EU, data=sales_grouped)
qplot(Product, sum_NA, data=sales_grouped)
qplot(Product, sum_global, data=sales_grouped)

# Create histograms.
qplot(sum_EU, data=sales_grouped)
qplot(sum_NA, data=sales_grouped)
qplot(sum_global, data=sales_grouped)

# Create boxplots.
qplot(sum_EU, data=sales_grouped, geom='boxplot')
qplot(sum_NA, data=sales_grouped, geom='boxplot')
qplot(sum_global, data=sales_grouped, geom='boxplot')


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_grouped$sum_EU)
qqline(sales_grouped$sum_EU, col='red')
qqnorm(sales_grouped$sum_NA)
qqline(sales_grouped$sum_NA, col='red')
qqnorm(sales_grouped$sum_global)
qqline(sales_grouped$sum_global, col='red')


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((sales_grouped$sum_EU))
shapiro.test((sales_grouped$sum_NA))
shapiro.test((sales_grouped$sum_global))

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_grouped$sum_EU)
skewness(sales_grouped$sum_NA)
skewness(sales_grouped$sum_global)


## 3d) Determine correlation
# Determine correlation.
cor(sales_grouped$sum_EU, sales_grouped$sum_NA)
cor(sales_grouped$sum_NA, sales_grouped$sum_global)
cor(sales_grouped$sum_EU, sales_grouped$sum_global)



###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
qplot(sum_EU, data=sales_grouped, geom='boxplot')
qplot(sum_NA, data=sales_grouped, geom='boxplot')
qplot(sum_global, data=sales_grouped, geom='boxplot')
# explain

###############################################################################

# 5. Create a DataFrame without outliers
sales2_no_out <- subset(sales2, EU_Sales <= 15 & NA_Sales <= 15)
summary(sales2_no_out)
###############################################################################

# 6. Observations and insights
# Your observations and insights here...

#	All three qqplots show a considerable upward departure from the line representing a normal distribution of data beyond 1 standard deviation (or Z score). This represents a considerable right skewness of the data with a relatively heavy tail or extreme outliers.
# The p value is below 0.05 for all the data sets. Consequently, all the data is unlikely to have a normal distribution.
# This is likely to impact linear regression results.
# sales data positively skewed. Right tailed. We expect distribution with mostly extreme values on the positive side.
# All datasets are all leptokurtic, i.e. kurtosis is greater than 3.  Has a wider/flatter shape with fatter tails, resulting in a greater chance of extreme positive or negative events. 
# That is, compared to data with a normal distribution.
# The correlation between NA and EU sales is moderate and positive.
# The correlation of EU and NA sales on their own with global sales is strong and positive. This is to be expected since both are components of global sales.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
sales_grouped

# Determine a summary of the data frame.
summary(sales_grouped)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
library(psych)
sales_grouped$Product <- NULL
head(sales_grouped)
corPlot(sales_grouped, cex=2)

# Create a linear regression model on the original data.
model1 <- lm(Global_Sales ~ EU_Sales, data = sales)
summary(model1)

model2 <- lm(Global_Sales ~ NA_Sales, data = sales)
summary(model2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_grouped$sum_EU, sales_grouped$sum_global)
coefficients(model1)
abline(coefficients(model1))

plot(sales_grouped$sum_NA, sales_grouped$sum_global)
coefficients(model2)
abline(coefficients(model2))
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
# Multiple linear regression model.

MRmodel <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales2)
summary(MRmodel)
plot(resid(MRmodel))
qplot(MRmodel$residuals, data=MRmodel)

MRmodel2 <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales2_no_out)
summary(MRmodel2)
plot(resid(MRmodel2))
qplot(MRmodel2$residuals, data=MRmodel2)

###############################################################################

# 4. Predictions based on given values
# create dataframe with given values
given_sales <- data.frame(EU_Sales = as.numeric(c("23.80", "1.56", "0.65", "0.97", "0.52")),
                          NA_Sales = as.numeric(c("34.02", "3.93", "2.73", "2.26", "22.08")))

# create a predict model and view results
predictTest = predict(MRmodel, newdata=given_sales, interval='confidence')
predictTest


predictTest2 = predict(MRmodel2, newdata=given_sales, interval='confidence')
predictTest2

# Compare with observed values for a number of records.

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The R^2 and adjusted R^2 are higher than for the models using European and North American sales on their own. Furthermore, the T value is higher for both coefficients in the multiple regression model than in the models with a single variable. Given there are similar P values for both models, the multiple regression model has been selected since it better explains variations in global sales.
# In general, the model appears to predict higher global sales values than the observed values when dealing with smaller values, but predicted accurately the best selling WII game’s global sales. This inaccuracy of the model when predicting smaller values may be due to the inclusion of the best sellers in the model. 
#	As a result, we have provided an additional model excluding such extreme outliers to compare values with. However, we are not going to prescind of the original model since it is still useful when dealing with best-selling products which make up a sizeable portion of Turtle Games’ revenue.
# furthermore, models including outliers are still inaccurate.

###############################################################################
###############################################################################




