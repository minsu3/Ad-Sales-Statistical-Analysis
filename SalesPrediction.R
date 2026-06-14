## Task 1 Data Import and Initial Inspection
setwd('/Users/minsukim/Documents/StatsData')
advertising <- read.csv("/Users/minsukim/Documents/StatsData/advertising.csv", header = TRUE)
# Summary of the advertising data
summary(advertising)
attach(advertising)

sd(TV)
sd(Radio)
sd(Newspaper)
sd(Sales)

# Visualization with Histogram and Boxplot
par(mar = c(5.1, 4.1, 4.1, 2.1)) # default margin settings

hist(TV, main = "TV Advertising Spend", col="blue", xlab = "Budget in Thousands", breaks = 30)
hist(Radio, main = "Radio Advertising Spend", col="red", xlab = "Budget in Thousands", breaks = 30)
hist(Newspaper, main = "Newspaper Advertising Spend", col="green", xlab = "Budget in Thousands", breaks = 30)
hist(Sales, main = "Sales", col="purple", xlab = "Budget in Thousands", breaks = 30)

boxplot(TV, main = "TV", ylab = "Budget in Thousands", col = "blue")
boxplot(Radio, main = "Radio", ylab = "Budget in Thousands", col = "red")
boxplot(Newspaper, main = "Newspaper", ylab = "Budget in Thousands", col = "green")
boxplot(Sales, main = "Sales", ylab = "Budget in Thousands", col = "purple")

# TV ad spending ranges from $700 to $296,400 which is the largest where the mean is $147,040
# Radio ad spending ranges from $0 to $49,600 with a mean of $23,264
# Newspaper ad spending ranges from $300 to $114,000 with a mean of $30,550

# Typical sales ad spending ranges from $1,600 to $27,000 with a mean of $15,130
# The median of sales is $16,000 and 1st quartile of $11,000 and 3 quartile of $19,050
# The standard deviation of 5.28 indicates that coefficient of variation of 35% which is of moderate variability. 

## Task 2 Exploratory Data Analysis
# Creating a correlation matrix
cor(advertising)
# Pearson correlation coefficient
cor(TV, Sales)
cor(Radio, Sales)
cor(Newspaper, Sales)
# Test of associations
cor.test(TV, Sales)
cor.test(Radio, Sales)
cor.test(Newspaper, Sales)

# Plotting TV, Radio, Newspaper vs Sales
plot(TV, Sales)
plot(Radio, Sales)
plot(Newspaper, Sales)

# The strongest advertising channel associated with sales is TV as the Pearson correlation coefficient is 0.901 which is close to the value of 1
# Newspaper with the correlation coefficient of 0.158 shows the weakest association with sales

## Task 3 Simple Linear Regression
# Fitting the model
SalesTVmodel <- lm(Sales ~ TV, data = advertising)

# report estimated regression coefficients (intercept and slope)
summary(SalesTVmodel)
# Intercept is 6.9748 and Slope is 0.0555
# Standard Error for Intercept is 0.322553 and Slope is 0.001896
# t-statistics for Intercept is 21.62 and Slope is 29.26 
# p-values  for Intercept is < 0.001 and Slope is < 0.001
# R-squared is 0.8112 

# We can interpret that every $1000 of spending on TV advertisement will increase by 55.5, because the slope regression coefficient is 0.0555 (multiplied by 1000)
# Since the slope is positive, we can interpret that more spending in TV advertisement will increase sales
# Additionally, the p-value is less than 0.001, this association is significant and convey confident

# Scatterplot with the regression line
plot(TV, Sales, main = "Linear Regression: Sales and TV", xlab = "TV Advertising", ylab = "Sales", pch  = 16)
abline(SalesTVmodel)

## Task 4 
# Fit a multiple linear regression model with response as Sales and predictors as TV, Radio, and Newspaper simultaneously
MultipleRegressionModel <- lm(Sales ~ TV + Radio + Newspaper, data = advertising)

#Report  Estimated coefficients, standard errors, p-values, and R-squared / adjusted R-squared
summary(MultipleRegressionModel)
# the 5% level in p-value is the threshold level for it to be significant, therefore TV and Radio with the p-value of < 0.001 are significant

newspaper_model <- lm(Sales ~ Newspaper, data = advertising)
summary(newspaper_model)
# compared to the simple regression without other variables, the coefficient becomes 0.038 with a p-value of 0.0255, which is below the threshold of 0.05; therefore, it becomes significant
# Including TV and Radio all together, the coefficient the coefficient is 0.0003 and p-value of 0.954 which in a multiple regression is not significant
cor(advertising[, c("TV", "Radio", "Newspaper")])
# shown in the inter-predictor correlation matrix, the coefficient of 0.34 between Radio and Newspaper shows a meaningful impact on Newspaper contribution to sales
# This indicates that the more money spent on Newspaper tend to have more spending on Radio as well, holding other variables constant 

## Task 5
# Residuals vs fitted values plot and Q-Q residuals plot 
plot(MultipleRegressionModel)
Standardized_Residuals <- rstandard(MultipleRegressionModel)

# value beyond 3 indicates that it is an outlier due to distance from fitted line
outliers <- which(abs(Standardized_Residuals) > 3) 
outliers

# The Residuals vs Fitted plot displays a clear linearity trend in the zero. The mean of residuals is practically at zero
# The homoscedasticity can be clearly spotted in this plot vertical spread of dots stay roughly the same width throughout and not fan out
# In the Q-Q residuals plot, the points standardized residuals and theoretical quantiles are in a diagonal line, which indicates normality and homoscedasticity is met 
# We can see that the outliers are 131, 151 and 34, however they are within the threshold so they will not significantly affect the regression

## Task 6
# compare simple regression (TV) and multiple regression model (all predictors) in R-squared and adjusted R-squared 
simple_regression <- lm(Sales ~ TV)
summary(simple_regression)
multiple_regression <- lm(Sales ~ TV + Radio + Newspaper)
summary(multiple_regression)
# With a simple regression with TV, the R-squared is 0.8122 and with Radio and Newspaper, it goes up to 0.9026
# The adjusted R-squared also goes up from 0.8112 to 0.9011 
# Standard residual error with TV only is 2.296 and with Radio and Newspaper it becomes 1.662

# From these measures, we can understand that adding Radio and Newspaper are useful to the model of predicting to actual sales figures
# Adding two measure reduces the standard residual error to 1.57 closer to the actual sales value
# Newspaper p-value is 0.954 which is insignificant to improvement to the prediction, but nonetheless slightly

# extracting coefficients from multiple regression model
coefficients <- coef(multiple_regression)
coefficients
# entire extra budget is allocated to TV
allocated_TV <- coefficients["TV"] * 10000
allocated_TV

# entire extra budget is allocated to Radio
allocated_Radio <- coefficients["Radio"] * 10000
allocated_Radio

# We can see that the budget allocated to TV yields 544.4568 units sold and Radio yields 1070.012 units sold
# The Radio coefficient is higher than TV so it is recommended to allocate the budget to Radio
# This is with the assumption that all other variables are held constant and the model coefficients are stable
# There are no interaction effects between TV and Radio working together

## Task 7
# Split sales at the median, then run a t-test for each predictor against the two groups
Sales_split2 <- ifelse(Sales > median(Sales), "High", "Low")
# Statistical tests to assess differences in central tendencies of the predictor variables with sales-split2
t.test(TV ~ Sales_split2)
t.test(Radio ~ Sales_split2)
t.test(Newspaper ~ Sales_split2)
# Visualising results
boxplot(TV ~ Sales_split2)
boxplot(Radio ~ Sales_split2)
boxplot(Newspaper ~ Sales_split2)

# Null hypothesis: The mean of each individual predictor is equal between the high and low sales group
# Alternative hypothesis: The mean of each individual predictor is different between the low and high 
# Assumptions: The observations are independent of each other
# Assumptions: the data within each group is normally distributed 
# Assumptions: variances are homogeneous across the two groups

# Creating split into 4 groups 
advertising$Sales_split4 <- cut(advertising$Sales,breaks = quantile(advertising$Sales,probs = seq(0,1,0.25)), include.lowest = TRUE, labels = c("Low","Medium","High","Extreme"))
# ANOVA testing
anova_tv <- aov(TV ~ Sales_split4, data = advertising)
anova_radio <- aov(Radio ~ Sales_split4, data = advertising)
anova_newspaper <- aov(Newspaper ~ Sales_split4, data = advertising)

# Running summaries for each variables to assess differences in central tendencies of the interval predictor variables with respect to Sales-split4
summary(anova_tv)
summary(anova_radio)
summary(anova_newspaper)

# Nonparametric equivalent Kruskal-Wallis testing
kruskal.test(TV ~ Sales_split4, data = advertising)
kruskal.test(Radio ~ Sales_split4, data = advertising)
kruskal.test(Newspaper ~ Sales_split4, data = advertising)

# Post-hoc testing with Tukey HSD
TukeyHSD(anova_tv)
TukeyHSD(anova_radio)
TukeyHSD(anova_newspaper)

## Task 8
# PCA Analysis

# Scale the data is true to put all predictors on equal footing
scaled_data <- scale(advertising[, c("TV","Radio","Newspaper","Sales")])
pca <- prcomp(scaled_data)
summary(pca)

# shows how much each variable contributes to each component
pca$rotation 

# creating a biplot to visualize the projection of the variables relates to the two components 
biplot(pca, main = "PCA Biplot", col  = c("blue", "red"))

# The biplot shows that TV leans more towards the PC2 while Radio and Newspaper loan on to PC1
# This indicates that TV operates independently of the other two predictors
# Radio and Newspaper both leading towards PC1 suggest they are correlated with each other, which indicates that more is spent in one in spent more on the other

# Hierchical Cluster model to identify the underlying factors that contribute to sales
distance_matrix <- dist(scaled_data)
hc_model <- hclust(distance_matrix)
# cluster dendrogram  
plot(hc_model, main = "Hierarchical Clustering of Advertising", xlab = "Markets", ylab = "Distance", cex  = 0.5)
# The plot shows the larger of the two groups containing the left two thirds of the markets (x-axis)
# The markets merge together at lower distances (y-axis) which indicates that they are quite similar to each other
# This suggests that these left two markets are moderate to low spend across all advertising channels with consistent sales figures
# The right group splits in higher distance, around 6, and these markets are more diverse from each other
# The diverse market in this group suggests a higher spend in advertising


