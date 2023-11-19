# DATA CLEANING
# load library for analysis of the dataset
library(tidyverse)
library(dplyr)

# load dataset into Rstudio
salary_data <- read.csv("raw_ds_salaries.csv")

# to check dataset variables and list of data available
glimpse(salary_data)
# to check structure of dataset
str(salary_data)
# to summarize numeric values of variables 
summary(salary_data)
# to check first 6 values for each variables
head(salary_data)

# to check wether there is any missing values in dataset
colSums(is.na(salary_data))

# SAVE CLEANED DATASET 
# Save the cleaned dataset to a new file
cleaned_dataset = salary_data
write.csv(cleaned_dataset, "cleaned_ds_salary.csv", header=TRUE,row.names = FALSE)

# check correlation of variables
cor(salary_data[, c("salary", "salary_in_usd","work_year","")])

# univariate analysis chart : 
# bar plot experience lvl ( categorical )
ggplot(salary_data, aes(x = experience_level)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Experience Level", y = "Frequency", title = "Distribution of Experience Levels")

# histogram chart remote ratios( continuous ni pilih )
# Filter the dataset for the desired values
subset_data <- subset(salary_data, remote_ratio %in% c(0, 50, 100))
# Create histogram for the filtered dataset
ggplot(subset_data, aes(x = remote_ratio)) +
  geom_histogram(fill = "#6BAED6", color = "black", bins = 3) +
  labs(x = "Remote Ratio", y = "Frequency", title = "Distribution of Remote Ratios") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
  )

# Boxplot for Categorical-Continuous Relationship
# START
ggplot(data = salary_data, aes(x = experience_level, y = salary)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +
  labs(x = "Experience Level", y = "Salary (in USD)", title = "Salary Distribution by Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Group the data by experience level and calculate the average salary
avg_salary <- aggregate(salary_in_usd ~ experience_level, data = salary_data, FUN = mean)
# Line chart for the relationship between experience level and average salary
ggplot(data = avg_salary, aes(x = experience_level, y = salary_in_usd, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(x = "Experience Level", y = "Average Salary (USD)", title = "Average Salary by Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# END

# bivariate 1 : avg salary by work year
# Group the data by work year, company size, and calculate the average salary
avg_salary <- aggregate(salary_in_usd ~ work_year + company_size, data = salary_data, FUN = mean)
# Line chart for the relationship between work year, average salary, and company size
ggplot(data = avg_salary, aes(x = work_year, y = salary_in_usd, group = company_size, color = company_size)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "Work Year", y = "Average Salary (USD)", title = "Average Salary by Work Year and Company Size") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
#END
# Group the data by work year, experience level, and job title, and calculate the count
job_counts <- aggregate(job_title ~ work_year + experience_level, data = salary_data, FUN = length)
# Grouped bar chart for the relationship between work year, job title count, and experience level
ggplot(data = job_counts, aes(x = work_year, y = job_title, fill = experience_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Work Year", y = "Job Title Count", title = "Job Title Count by Work Year and Experience Level") +
  scale_fill_manual(values = c("red", "green", "blue", "purple")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right")

 # END
colours()
