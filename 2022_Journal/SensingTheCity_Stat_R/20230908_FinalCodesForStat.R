library(dplyr)
library(ggplot2)
library(psych)


remove(list = ls())

# Load your data
data <- read.csv("20230907_raw.csv")
colnames(data)

##5.2.2.1	Relationship between the Urban Environment and Sentiments----

# Descriptive analysis using describe for computing various statistics including mean and sd
descriptive_stats <- describe(data[,11:23])

# Correlation matrix
cor_matrix <- cor(data[,11:23], use = "complete.obs")

# Print the results
print(descriptive_stats) #Table 1
print(cor_matrix)



#MLR 5.2.2.1
library(lmtest)

# Fit the model
mlr_model <- lm(Neg_Pos ~ Sky + Wall + Building + Fence + Tree + Road + Sidewalk + Streetlight + Signboard + Person + Bicycle + MotorVehicle, data = data)

# Summary of the model
summary(mlr_model)

# Checking for heteroskedasticity
bptest(mlr_model)


# Getting the summary of the model in a variable
model_summary <- summary(mlr_model)

# Extracting the coefficients
coefficients <- model_summary$coefficients

# Adding lower and upper bounds (95% CI) to the coefficients matrix
coefficients <- cbind(coefficients, 
                      Lower = coefficients[, "Estimate"] - (coefficients[, "Std. Error"] * 1.96), 
                      Upper = coefficients[, "Estimate"] + (coefficients[, "Std. Error"] * 1.96))

# Printing the updated coefficients matrix with lower and upper bounds
print(coefficients)




##5.2.2.2 MLR Year----
library(dplyr)
data <- read.csv("20230907_raw.csv")


# Creating a time period variable
data <- data %>%
  mutate(
    time_period = case_when(
      Date_Year < 2020 ~ "Pre-COVID",
      Date_Year == 2020 ~ "During COVID",
      Date_Year > 2020 ~ "Post-COVID"
    )
  )

# Descriptive analysis by time period using describeBy
library(psych)
descriptive_by_time <- describeBy(data[,11:23], group=data$time_period, mat=TRUE)



# MLR analysis by time period
# Pre-COVID period----
pre_covid_data <- filter(data, time_period == "Pre-COVID")
mlr_pre_covid <- lm(Neg_Pos ~ Sky + Wall + Building + Fence + Tree + Road + Sidewalk + Streetlight + Signboard + Person + Bicycle + MotorVehicle, data = pre_covid_data)
summary(mlr_pre_covid)



# Getting the summary of the model in a variable
model_summary_pre <- summary(mlr_pre_covid)

# Extracting the coefficients
coefficients_pre <- model_summary_pre_covid$coefficients

# Adding lower and upper bounds (95% CI) to the coefficients matrix
coefficients_pre <- cbind(coefficients_pre, 
                      Lower = coefficients_pre[, "Estimate"] - (coefficients_pre[, "Std. Error"] * 1.96), 
                      Upper = coefficients_pre[, "Estimate"] + (coefficients_pre[, "Std. Error"] * 1.96))

# Printing the updated coefficients matrix with lower and upper bounds
print(coefficients_pre)




# During COVID period----
during_covid_data <- filter(data, time_period == "During COVID")
mlr_during_covid <- lm(Neg_Pos ~ Sky + Wall + Building + Fence + Tree + Road + Sidewalk + Streetlight + Signboard + Person + Bicycle + MotorVehicle, data = during_covid_data)
summary(mlr_during_covid)

# Getting the summary of the model in a variable
model_summary_during <- summary(mlr_during_covid)

# Extracting the coefficients
coefficients_during <- model_summary_during$coefficients

# Adding lower and upper bounds (95% CI) to the coefficients matrix
coefficients_during <- cbind(coefficients_during, 
                          Lower = coefficients_during[, "Estimate"] - (coefficients_during[, "Std. Error"] * 1.96), 
                          Upper = coefficients_during[, "Estimate"] + (coefficients_during[, "Std. Error"] * 1.96))

# Printing the updated coefficients matrix with lower and upper bounds
print(coefficients_during)



# Post-COVID period----
post_covid_data <- filter(data, time_period == "Post-COVID")
mlr_post_covid <- lm(Neg_Pos ~ Sky + Wall + Building + Fence + Tree + Road + Sidewalk + Streetlight + Signboard + Person + Bicycle + MotorVehicle, data = post_covid_data)
summary(mlr_post_covid)

# Getting the summary of the model in a variable
model_summary_post <- summary(mlr_post_covid)

# Extracting the coefficients
coefficients_post <- model_summary_post$coefficients

# Adding lower and upper bounds (95% CI) to the coefficients matrix
coefficients_post <- cbind(coefficients_post, 
                             Lower = coefficients_post[, "Estimate"] - (coefficients_post[, "Std. Error"] * 1.96), 
                             Upper = coefficients_post[, "Estimate"] + (coefficients_post[, "Std. Error"] * 1.96))

# Printing the updated coefficients matrix with lower and upper bounds
print(coefficients_post)





### Plot on yearly data----


###Coefficient plot----
library(ggplot2)
library(dplyr)

# Extract coefficients into data frames
pre_covid_coefs <- as.data.frame(summary(mlr_pre_covid)$coefficients)
during_covid_coefs <- as.data.frame(summary(mlr_during_covid)$coefficients)
post_covid_coefs <- as.data.frame(summary(mlr_post_covid)$coefficients)

# Add variable names and time period columns
pre_covid_coefs <- pre_covid_coefs %>% mutate(variable = rownames(pre_covid_coefs), time_period = "Pre-COVID")
during_covid_coefs <- during_covid_coefs %>% mutate(variable = rownames(during_covid_coefs), time_period = "During-COVID")
post_covid_coefs <- post_covid_coefs %>% mutate(variable = rownames(post_covid_coefs), time_period = "Post-COVID")

# Combine data frames
all_coefs_covid <- bind_rows(pre_covid_coefs, during_covid_coefs, post_covid_coefs)

# Create a plot
ggplot(all_coefs_covid, aes(x = variable, y = Estimate, fill = time_period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  coord_flip() + 
  labs(title = "Coefficients Across COVID Periods",
       y = "Coefficient Estimate",
       x = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))