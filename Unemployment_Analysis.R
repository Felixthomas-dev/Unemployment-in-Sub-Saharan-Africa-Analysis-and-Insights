#Install and load tidyverse package
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(ggplot2)


#Load dataset
data <- read.csv("~/Felix Data/Unemployment_Data.csv")

#Now lets carry out some simple exploration of the dataset.
head(data) #displays the first few rows of the dataset.
tail(data) #displays the last few rows of the dataset.
colnames(data) #display the column names of the dataset.
str(data) #displays the structure of the dataset including the data type of variables.
dim(data) #displays the dimension of the dataset, here we have 960 rows and 38 columns.
glimpse(data) #displays the column of the dataset with some portion of our data.
view(data) #displays the entire dataset.

#Lets check for missing data and drop them.
sum(is.na(data))
data <- data %>% drop_na()
sum(is.na(data)) #Great Job!
dim(data) #Dataset new dimension is 894 rows and 38 columns.

"We have observed that the Date_code and Country_code columns will not be useful for this analysis
let us get rid of them"
data <- data[-c(2,4)]
view(data)
str(data)

#Now lets do some Exploratory Data Analysis (EDA)
#Lets check for outliers. Lets use the unemployment column as sample.
Filtered_country <- data %>%
  filter(Country == "Ghana") 
ggplot(Filtered_country, aes(x = Country, y = Unemployment))+ 
  geom_boxplot(fill = "red") + theme(panel.background = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA))

# Visualization of Population Across Countries
# Sort the dataset by Population in descending order
sorted_data <- data %>% arrange(desc(Population))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2016) #year ranges from 1997 to 2016
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries using ggplot
ggplot(top_10, aes(x = reorder(Country, -Population), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Countries by Population - 2016",
       x = "Country", y = "Population") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #Rotate x-axis labels for better readability
  scale_y_continuous(labels = scales::label_number_si()) + theme(panel.background = element_blank())

# Visualization of Employment Rates Across Countries
# Sort the dataset by Employment in descending order
sorted_data <- data %>% arrange(desc(Employment))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2016) #year ranges from 1997 to 2016
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries using ggplot
ggplot(top_10, aes(x = reorder(Country, -Employment), y = Employment)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Top 10 Countries by Employment - 2016",
       x = "Country", y = "Employment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_y_continuous(labels = scales::label_number_si())+ theme(panel.background = element_blank()) 


# Visualization of Unemployment Rates Across Countries
# Sort the dataset by Unemployment in descending order
sorted_data <- data %>% arrange(desc(Unemployment))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2016) #year ranges from 1997 to 2016
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries using ggplot
ggplot(top_10, aes(x = reorder(Country, -Unemployment), y = Unemployment)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Countries by Unemployment - 2016",
       x = "Country", y = "Unemployment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_y_continuous(labels = scales:: percent_format(scale = 1)) + theme(panel.background = element_blank())+
  coord_cartesian(xlim = c(), ylim = c(0,25))

# Visualization for GDP Trends
Filtered_country <- data %>%
  filter(Country == "Nigeria") 
ggplot(Filtered_country, aes(x = Year, y = Annual_GDP_growth, color = Country)) +
  geom_line() +
  labs(title = "Nigeria GDP Trend 1997 to 2016",
       x = "Year", y = "Annual GDP Growth") +
  scale_y_continuous(labels = scales:: percent_format(scale = 1))+ theme(panel.background = element_blank())

# Visualization for Urban Vs Rural Population Trend
Filtered_country <- data %>%
  filter(Country == "South Africa") 
ggplot(Filtered_country, aes(x = Year, y = Fertility_rate)) +
  geom_line(colour = "darkblue") +
  labs(title = "South Africa Fertility Rate Trend",
       x = "Year ", y = "Fertility Rate") +
  scale_y_continuous(labels = scales:: percent_format(scale = 1))+ 
  theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill = NA))


#STATSITICAL ANALYSIS
"Lets start by checking the normality of the data. This can be achieved by carrying out the following
hypothesis tests; Shapiro-Wilk Test (Statistical Test), Q-Q plot (Visual Method) and
Kolmogorov-Smirnov Test (Statistical Test). Lets install and load datarium and qqplotr"

install.packages("datarium")
install.packages("qqplotr")
library(datarium)
library(qqplotr)

#Shapiro Test
#Test for individual indicators. Lets try Population and Annual GDP Growth.
shapiro.test(data$Population)
shapiro.test(data$Annual_GDP_growth)

"Since the p-value is less than the significance level (0.05), we reject the null hypothesis,
indicating that the data is not normally distributed."

# Perform the Q-Q test
qqnorm(data$Population)
qqline(data$Population, col = 2)#Since the points does not closely follow the reference line, it does not suggest normality.
qqnorm(data$Annual_GDP_growth)
qqline(data$Annual_GDP_growth, col = 2) #This doesn't look so bad.


# Perform the Kolmogorov-Smirnov test
ks.test(data$Population, "pnorm")
ks.test(data$Annual_GDP_growth, "pnorm")
"Since the p-value is less than the significance level (0.05), we reject the null hypothesis, 
indicating that the data is not normally distributed."

"We observed that the data is non-normal, to correct this, we take the log of the data"
#First we remove non numeric and non integer column(s). In this case, we remove the country column.
data1 <- data[-c(2)]
view(data1)
str(data1)
#Next we take the log of the data to normalize it.
data_norm <- log(data1)
qqnorm(data_norm$Population)
qqline(data_norm$Population, col = 2)
qqnorm(data_norm$Unemployment)
qqline(data_norm$Unemployment, col = 2)
#Perfect!, we can apply this steps to every non-normal indicator.

summary(data) #Summarise Statistics of the entire dataset

#Lets calculate the  Mean, Median, Mode, Standard deviation, Skewness and Kurtosis of the data.
# Calculate mean, median, and standard deviation
mean_values <- apply(data1, 2, mean)
median_values <- apply(data1, 2, median)
std_dev_values <- apply(data1, 2, sd)

# Now lets Print the results. We can do this using two methods
print(mean_values)
print(median_values)
print(std_dev_values) 
#OR
cat("Mean:\n", mean_values, "\n\n")
cat("Median:\n", median_values, "\n\n")
cat("Standard Deviation:\n", std_dev_values, "\n\n")

# For Mode lets create a function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the user function. Lets do this for a few columns.
result_population <- getmode(data1$Population)
result_population_growth <- getmode(data1$Population_growth)
result_unemployment <- getmode(data1$Unemployment)
result_gdp <- getmode(data1$Annual_GDP_growth)
result_fertility <- getmode(data1$Fertility_rate)
print(result_population)
print(result_population_growth)
print(result_unemployment)
print(result_gdp)
print(result_fertility)

#For Skewness and Kurtosis, we Install and load the e1071 package
install.packages("e1071")
library(e1071)

# Calculate skewness and kurtosis
skewness_value_population <- skewness(data1$Population)
skewness_value_Population_growth <- skewness(data1$Population_growth)
skewness_value_unemployment <- skewness(data1$Unemployment)
skewness_value_gdp <- skewness(data1$Annual_GDP_growth)
skewness_value_fertility <- skewness(data1$Fertility_rate)

kurtosis_value_population <- kurtosis(data1$Population)
kurtosis_value_population_growth <- kurtosis(data1$Population_growth)
kurtosis_value_unemployment <- kurtosis(data1$Unemployment)
kurtosis_value_gdp <- kurtosis(data1$Annual_GDP_growth)
kurtosis_value_fertility <- kurtosis(data1$Fertility_rate)

# Print the results
print(skewness_value_population)
print(skewness_value_Population_growth)
print(skewness_value_unemployment)
print(skewness_value_gdp)
print(skewness_value_fertility)

print(kurtosis_value_population)
print(kurtosis_value_population_growth)
print(kurtosis_value_unemployment)
print(kurtosis_value_gdp)
print(kurtosis_value_fertility)


#CORRELATION ANALYSIS
"Now lets carry out correlation analysis for our different indicators. First we select some
indicators of interest"
corr_data1 <- data[c("Population","Urban_population","Rural_population","Population_growth",
                     "Unemployment","Annual_GDP_growth","Fertility_rate","Employment_agric",
                     "Employment_industry","Employment_service","Selfemployed","Unemployment_male",
                     "Unemployment_female")]
corr_data2 <- data[c("Population_male","Population_female","Population_15_64_male","Population_15_64_female",
                     "Population_above64_male","Population_above64_female","Unemployment_male",
                     "Unemployment_female","Wage_salary_workers_male","Wage_salary_workers_female", 
                     "Selfemployed_male","Selfemployed_female","Employment_agric_male","Employment_agric_female",
                     "Employment_industry_male","Employment_industry_female","Employment_service_male",
                     "Employment_service_female")]
corr_data3 <- data[c("Population","Population_male","Population_female","Unemployment")]
corr_data4 <- data[c("Selfemployed","Employment","Unemployment","Annual_GDP_growth")]
#Pearson Correlation:
# Calculate Pearson correlation
correlation_matrix_p1 <- cor(corr_data1, method = "pearson")
correlation_matrix_p2 <- cor(corr_data2, method = "pearson")
correlation_matrix_p3 <- cor(corr_data3, method = "pearson")
correlation_matrix_p4 <- cor(corr_data4, method = "pearson")

# Print the correlation matrix
print(correlation_matrix_p1)
print(correlation_matrix_p2)
print(correlation_matrix_p3)
print(correlation_matrix_p4)

#Spearman Correlation:
# Calculate Spearman correlation
correlation_matrix_s1 <- cor(corr_data1, method = "spearman")
correlation_matrix_s2 <- cor(corr_data2, method = "spearman")
correlation_matrix_s3 <- cor(corr_data3, method = "spearman")
correlation_matrix_s4 <- cor(corr_data4, method = "spearman")

# Print the correlation matrix
print(correlation_matrix_s1)
print(correlation_matrix_s2)
print(correlation_matrix_s3)
print(correlation_matrix_s4)
"Interpretation:
  The correlation coefficient ranges from -1 to 1.
A correlation of 1 indicates a perfect positive correlation.
A correlation of -1 indicates a perfect negative correlation.
A correlation of 0 indicates no correlation."

"Visualization:
We can visualize the correlation matrix using different correlation plots for better
interpretation. lets see a few."
# Lets install and load the corrplot package
install.packages("corrplot")
library(corrplot)

corrplot(correlation_matrix_p1, type = "upper", order = "hclust")
corrplot(correlation_matrix_p2, type = "upper", order = "hclust")
corrplot(correlation_matrix_p3, type = "upper", order = "hclust")
corrplot(correlation_matrix_p4, type = "upper", order = "hclust")
corrplot(correlation_matrix_s1, type = "upper", order = "hclust")
corrplot(correlation_matrix_s2, type = "upper", order = "hclust")
corrplot(correlation_matrix_s3, type = "upper", order = "hclust")
corrplot(correlation_matrix_s4, type = "upper", order = "hclust")

# Alternatively, Lets install and load the GGally package
install.packages("GGally")
library(GGally)

GGally:: ggcorr(corr_data1, method = c("everything", "pearson"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data2, method = c("everything", "pearson"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data3, method = c("everything", "pearson"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data4, method = c("everything", "pearson"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data1, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data2, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data3, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE)
GGally:: ggcorr(corr_data4, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE)


#HYPOTHESIS TEST
#1. Chi-squared Test
#Objective 1: To determine the structural reasons behind unemployment in Sub-Saharan Africa.
#Chi-squared test for independence (Lets check the relationship between Population and unemployment).
chisq.test(data$Population,data$Unemployment)
#Since P-value = 0.2765, i.e.P >0.05, we do not have enough evidence to reject the Null Hypothesis.
"Null Hypothesis(H0): There is no significant relationship between Population and unemployment in 
Sub-Saharan Africa.
Alternative Hypothesis(H1): There is a significant relationship between Population and unemployment in
Sub-Saharan Africa."

#Lets try out another variable.
chisq.test(data$Selfemployed,data$Unemployment)
#Since P-value = 2.98e-05, i.e.P <0.05, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no significant relationship between Selfemployment and unemployment in 
Sub-Saharan Africa.
Alternative Hypothesis(H1): There is a significant relationship between Selfemployment and unemployment in
Sub-Saharan Africa."

#2. T-test
#Objective 2: To investigate gender disparities in unemployment in Sub-Saharan Africa.
t.test(data$Unemployment_male, data$Unemployment_female)
#Since P-value = 1.428e-12, i.e. P <0.05, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no significant difference in unemployment rates between genders 
in Sub-Saharan Africa.
Alternative Hypothesis(H1): There is a significant difference in unemployment rates between genders
in Sub-Saharan Africa."
t.test(data$Population_male, data$Population_female)
#Since P-value = 0.8399, i.e.P >0.05, we do not have enough evidence to reject the Null Hypothesis.
"Null Hypothesis(H0): There is no significant difference in unemployment rates between genders 
in Sub-Saharan Africa.
Alternative Hypothesis(H1): There is a significant difference in unemployment rates between genders
in Sub-Saharan Africa."

#3. ANOVA
#Objective 3: To examine the dynamics of youth(working age) unemployment in Sub-Saharan Africa.
anova_result <- aov(Unemployment ~ Population_15_64, data = data)
summary(anova_result)
#Since P-value = 0.000498, i.e.P <0.05, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no significant difference in unemployment rates between youth and 
other age groups in Sub-Saharan Africa.
Alternative Hypothesis(H1): There is a significant difference in unemployment rates between youth
and other age groups in Sub-Saharan Africa."

#4. Wilcoxon rank sum test/Mann-Whitney U test
#Objective 4: To investigate the impact of selfemployment in the economy of Sub-Saharan Africa.
wilcox.test(data$Selfemployed, data$Unemployment)
#Since P-value <2.2e-16, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no significant difference in economic indicators between selfemployed
and unemployed.
Alternative Hypothesis(H1): There is a significant difference in economic indicators between
selfemployed and unemployed."


#REGRESSION ANALYSIS
#1. Simple Linear Regression (SLR).
"Objective 1: To determine the structural reasons behind unemployment in Sub-Saharan Africa
Variables: Include Unemployment and Population variables.
Rationale: Simple Linear regression allows you to analyze the impact of an independent variable 
on the dependent variable (unemployment). This can help identify if population is significantly 
associated with unemployment."

model_1 <- lm(Unemployment ~  Population, data = data_norm) 
summary(model_1)

"Both coefficients are significant and the SLR equation will be:
Unemployment = 4.45865 + (-0.16349) × Population
R-squared is 0.07. This means Population with this regression 
equation can predict 7.4% of the entire variability in Unemployment"

#Lets visualize this.
plot(Unemployment ~ Population, data_norm,
     col = "red",
     main = "Regression: Unemployment and Population",
     xlab = "Unemployment",
     ylab = "Population")
#Lets add regression line to this plot.
abline(model_1, col= "black")
#it is observed that there is a linear relationship between Population and Unemployment.

#Now lets check for residual independence.
plot(model_1, 1)
#The correlation is approximately zero(0)

#Lets check for normality of Residuals
plot(model_1, 2)
#It appears to be normal as the observations are very close to the line

#Finally we check for equal variances of the residuals (Homoscedasticity) 
plot(model_1, 3)
"The residuals are roughly scattered around the red line, with roughly equal variability at all 
fitted values."

#Now lets predict the unemployment rate in a particular population. Assume population = 100
# Unemployment = 4.45865 + (-0.16349) × 100
#Unemployment =-11.89035. So for every 100persons, about 12 will be unemployed.

#2. Multiple Linear Regression
#Objective 2: To investigate gender disparities in unemployment in Sub-Saharan Africa.
"Variables: Include Unemployment, Unemployment_male and Unemployment_female variables.
Rationale: Multiple regression allows you to analyze the impact of multiple independent variables
on the dependent variable (unemployment rate). This can help identify which structural factors are
significantly associated with unemployment."

model_2 <- lm(Unemployment_male ~  Population_male + Selfemployed_male, data = data_norm) 
summary(model_2)


"Both coefficients are significant, and the MLR equation will be:
Unemployment_male = 6.41285 + (-0.05451) X Population_male + (-0.94031) X Selfemployed_male
R-squared is 0.33. This means Population with this regression 
equation can predict 33% of the entire variability in Male Unemployment"

data.frame(colnames(data_norm))
#Lets visualize this.

pairs(data_norm[,c(19,3,25)], lower.panel = NULL, pch = 19,cex = 0.2)

#it is observed that there is a linear relationship between Gender disparity and Unemployment.

#Now lets check for residual independence.
plot(model_2, 1)
#The correlation is approximately zero(0)

#Lets check for normality of Residuals
plot(model_2, 2)
#It appears to be normal as the observations are very close to the line

#Finally we check for equal variances of the residuals (Homoscedasticity) 
plot(model_2, 3)
"The residuals are roughly scattered around the red line, with roughly equal variability at all 
fitted values."

# Lets check for multicollinearity by using the variance inflation factor.
#First, lets install and load the Car package to use VIF
install.packages("car")
library(car)

vif(model_2)
"Both values are 1.203132 and 1.203132. Since both values are less than 5 and we can conclude there 
is no collinearity between IVs (Xs)"

"Now lets predict the unemployment rate for male in a particular population with selfemployed people. 
Assume population = 100 and selfemployed = 30"
#Unemployment_male = 6.41285 + (-0.05451) X 100 + (-0.94031) X 30
#Unemployment_male =-27.24745. So for every 100 persons with 30 self employed, about 27 will be unemployed.


#3. Linear Regression
"Objective 4: To investigate the impact of self-employment and entrepreneurship in the economy of Sub-Saharan Africa.
Variables: Include indicators of self-employment.
Rationale: Linear regression can be used if the outcome is a continuous variable (e.g., GDP)."

#Linear Regression
model_3 <- lm(Annual_GDP_growth ~ Selfemployed, data = data)
summary(model_3)


#TIME SERIES ANALYSIS
#Lets install and load ts and forecast packages
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

#Lets filter a country as sample for this analysis.
Filtered_country <- data %>%
  filter(Country == "Burundi")
unemployment_ts <- ts(Filtered_country$Unemployment, start = 1997,frequency = 1)
plot(unemployment_ts, main = "Burundi Unemployment Time Series 1997 to 2016", xlab = "Time",
     ylab = "Unemployment Rate (%)")

#Using GGPlot
ggplot(Filtered_country, aes(x = Year, y = Unemployment, color = Country)) +
  geom_line() +
  labs(title = "Burundi Unemployment Time Series 1997 to 2016",
       x = "Year", y = "Unemployment") +
  scale_y_continuous(labels = scales:: percent_format(scale = 1))+ theme(panel.background = element_blank())

adf.test(unemployment_ts)  # Augmented Dickey-Fuller test for stationarity
"since p-value = 0.37, i.e p>0.05 we accept the null hypothesis and reject alternative hypothesis
that suggests the time series is stationary."

# Lets build a model for our forecast
arima_model1 <- Arima(unemployment_ts)
print(arima_model1)
"The time series is represented simply by the ARIMA(0,0,0) with a non-zero mean model, 
which captures the variability and average level. The goodness-of-fit metrics imply that the
model describes the observed data sufficiently, but it's crucial to contrast these statistics 
with those of other models and take the particular objectives of the study into account."

# Forecast future values
forecast_values1 <- forecast(arima_model1, h = 10)  # Forecasting 10 years ahead
forecast_values1
plot(forecast_values1, main = "Unemployment Rate Forecast", xlab = "Time", ylab = "Unemployment Rate")

#Using ggplot
autoplot(forecast_values1)+labs(title = "10 years Unemployment Time Series Forcast",
                                 x = "Year", y = "Unemployment") +
  scale_y_continuous(labels = scales:: percent_format(scale = 1))+ theme(panel.background = element_blank())
"Form the forecast, we notice that 80% of the unemployment forecast for the next 10 years will fall
between 0.38% and 3.4% (tick blue box), while 95% will fall between -0.42% and 4.2% (light blue box)"

#lets build another model and use 99% CI.
arima_model2 <- auto.arima(unemployment_ts)
print(arima_model2)
forecast_values2 <- forecast(arima_model2, h = 10, level = c(80,99))
forecast_values2 
plot(forecast_values2, main = "Unemployment Rate Forecast", xlab = "Time", ylab = "Unemployment Rate")

#Using ggplot
autoplot(forecast_values2)+labs(title = "10 years Unemployment Time Series Forcast",
                               x = "Year", y = "Unemployment") +
  scale_y_continuous(labels = scales:: percent_format(scale = 1))+ theme(panel.background = element_blank())

