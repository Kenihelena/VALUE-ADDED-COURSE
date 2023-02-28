library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
# Load CSV files
cars = read.csv('DS Engineering project/cars_multi.csv', header=TRUE)
prices = read.csv('DS Engineering project/cars_price.csv', header=TRUE) 

# Merge the two dataframes together using the ID field
cars = cars %>%
  left_join(prices, by = "ID")
str(cars)
summary(cars)
# Cylinders came in as an integer, when it should be a multi-valued discrete, 
# otherwise known as a "factor" in R. 
cars$cylinders = cars$cylinders %>%
  factor(labels = sort(unique(cars$cylinders)))

# Horsepower was imported as a factor, but it should be a continuous numerical 
# variable.
cars$horsepower = as.numeric(levels(cars$horsepower))[cars$horsepower]

# I will change he model (year) column from an integer to a categorical factor.
model_years = sort(unique(cars$model))
cars$model = cars$model %>%
  factor(labels = model_years)

# I am converting the origin column from an integer to a descriptive categorical variable. By looking at the data and cross-referencing it with my knowledge of which brands are based in which countries, I can discern that the origins in this dataset are 
# 1 = United States ("USA") 
# 2 = Europe
# 3 = Japan
origins <- c('USA', 'Europe', 'Japan')
cars$origin <- factor(cars$origin, labels = origins)
# Miles Per Gallon
qplot(cars$mpg, xlab = 'Miles Per Gallon', ylab = 'Count', binwidth = 2, 
      main='Frequency Histogram: Miles per Gallon')
# Number of Cylinders
qplot(cars$cylinders, xlab = 'Cylinders', ylab = 'Count', 
      main='Frequency Histogram: Number of Cylinders')
table(cars$cylinders)
# Based on the relatively tiny counts of three- and five-cylinder cars (4 and 3, respectively), I am removing those completely because they end up being a distraction in later plots
cars = cars[!cars$cylinders %in% c(3, 5),]
qplot(cars$cylinders, ylab = 'Count', xlab = 'Cylinders')
# Displacement
qplot(cars$displacement, xlab = 'Displacement', ylab = 'Count', binwidth = 20,
      main='Frequency Histogram: Displacement')
# Horsepower
qplot(cars$horsepower, xlab = 'Horsepower', ylab = 'Count', binwidth = 10,
      main='Frequency Histogram: Horsepower')
# Weight
qplot(cars$weight, xlab = 'Weight', ylab = 'Count', binwidth = 200,
      main='Frequency Histogram: Weight')
# Acceleration
qplot(cars$acceleration, xlab = 'Acceleration', ylab = 'Count', binwidth = 1,
      main='Frequency Histogram: Acceleration')
# Model Year
qplot(cars$model, xlab = 'Model Year', ylab = 'Count', 
      main='Frequency Histogram: Model Year')
cor(cars[ , c('weight', 'displacement', 'horsepower', 'acceleration')], 
    use='complete')
# Origin
qplot(cars$origin, xlab = 'Origin', ylab = 'Count', main='Frequency Histogram: Origin')
table(cars$origin)
# Price
qplot(cars$price, xlab = 'Price', ylab = 'Count', main='Frequency Histogram: Price')
summary(cars$price)
ggplot(data = cars, aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('MPG') +
  ylab('Weight') +
  ggtitle('MPG vs. Weight: Entire Sample')
fit = lm(mpg ~ weight, data=cars)
summary(fit)
ggplot(data = cars, aes(x = model, y = mpg)) +
  geom_boxplot() +
  xlab('Model Year') +
  ylab('MPG') +
  ggtitle('MPG Comparison by Model Year')
ggplot(data = cars, aes(x = origin, y = mpg)) +
  geom_boxplot() +
  xlab('Region of Origin') +
  ylab('MPG') +
  ggtitle('MPG Comparison by Region of Origin')
ggplot(data = cars, aes(x = origin, y = weight)) +
  geom_boxplot() +
  xlab('Region of Origin') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Region of Origin') 
ggplot(data = cars, aes(x = cylinders, y = mpg)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('MPG') +
  ggtitle('MPG Comparison by Number of Cylinders') 
ggplot(data = cars, aes(x = cylinders, y = weight)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Number of Cylinders') 
ggplot(data = cars, aes(x = cylinders, fill = origin)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  xlab('Weight') +
  ylab('Price') +
  ggtitle('Price vs. Weight')
ggplot(data = cars, aes(x = acceleration, y = price)) +
  geom_point() +
  xlab('Acceleration') +
  ylab('Price') +
  ggtitle('Price vs. Acceleration')
fit = lm(price ~ acceleration, data=cars)
summary(fit)
ggplot(data = cars, aes(x = cylinders, y = price)) +
  geom_boxplot() +
  xlab('Cylinders') +
  ylab('Price') +
  ggtitle('Price vs. Number of Cylinders')
ggplot(data = cars, aes(x = model, y = price)) +
  geom_boxplot() +
  xlab('Year') +
  ylab('Price') +
  ggtitle('Price over Time')
ggplot(data = cars, aes(x = origin, y = price)) +
  geom_boxplot() +
  xlab('Origin') +
  ylab('Price') +
  ggtitle('Prices by Region')
ggplot(data = cars, aes(x = mpg, y = price)) +
  geom_point() +
  xlab('MPG') +
  ylab('Price') +
  ggtitle('Prices by MPG')
ggplot(data = cars, aes(x = model, fill = cylinders)) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('Each Region of Origin\'s Product Mix Over Time')
ggplot(data = cars, aes(x = model, y = weight)) +
  geom_boxplot() +
  facet_wrap(~ origin) +
  xlab('Model Year') +
  ylab('Weight') +
  ggtitle('Weight Distributions Over Time by Region of Origin')
ggplot(data = cars, aes(x = model, y = mpg)) +
  geom_boxplot() +
  facet_wrap(~ origin) +
  xlab('Model Year') +
  ylab('MPG') +
  ggtitle('Evolution of MPG Over Time by Region of Origin: All Cars')
ggplot(data = subset(cars, cylinders==4), aes(x = model, y = mpg)) +
  geom_boxplot() +
  facet_wrap(~ origin) +
  xlab('Model Year') +
  ylab('MPG') +
  ggtitle('Evolution of MPG Over Time by Region of Origin: 4-Cylinder Cars Only') 
ggplot(data = subset(cars, cylinders==4), aes(x = model, y = mpg, group = 1)) +
  geom_point() +
  facet_wrap(~ origin) +
  geom_smooth() +
  xlab('Model Year') +
  ylab('MPG') +
  ggtitle('Evolution of MPG Over Time by Region of Origin: 4-Cylinder Cars Only')
ggplot(data = subset(cars, cylinders==4), aes(x = model, y = weight, group = 1)) +
  geom_point() +
  facet_wrap(~ origin) +
  geom_smooth() +
  xlab('Model Year') +
  ylab('Weight') +
  ggtitle('Evolution of Weight Over Time by Region of Origion: 4-Cylinder Cars Only')
ggplot(data = subset(cars, cylinders==4), aes(x = origin, y = mpg)) +
  geom_boxplot() +
  facet_wrap(~ model, ncol = 7) +
  ggtitle('MPG Comparison by Region of Origin: 4-Cylinder Cars Only') +
  xlab('Origin') + 
  ylab('MPG') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
