#1)	Data Wrangling
#1a)	Loading the data
install.packages("mongolite")
library(mongolite)
connection_string = 'mongodb+srv://chen1958:RWp3aSmI2EPKEDt5@cluster0.yyopkre.mongodb.net/?retryWrites=true&w=majority'
grades_collection <- mongo(url = connection_string, collection = "grades", db="sample_training")
data <- grades_collection$find() # Retrieve data from MongoDB

#1b)	Handling missing data
install.packages("tidyr")
library(tidyr)
data <- data %>%
  fill(student_id, scores, class_id, .direction = "downup")
# Check for missing values
summary(data)

#1c)	Tidying the data
install.packages("dplyr")
library(dplyr)
# Calculate the number of courses each student has taken
summary_data <- data %>%
  group_by(student_id) %>%
  summarise(courses_num = n_distinct(class_id), .groups = 'drop') # Summarize data by counting distinct class_ids per student
#View categorized data
print(summary_data) # Print the data frame to see summarized results


#2)	Data Transformation
#2a)	Data Transformation techniques
# Unnest the 'scores' column
data <- data %>%
  unnest(scores) # Flatten the 'scores' array to expand it into rows
install.packages("jsonlite")
library(jsonlite)
data <- jsonlite::fromJSON(jsonlite::toJSON(data), flatten = TRUE) # Convert data to JSON and back to flatten nested fields

#2b)	Creating new variables
# Creating separate columns for each type of score
data <- data %>%
  mutate(exam_score = if_else(type == "exam", score, as.numeric(NA)),
         quiz_score = if_else(type == "quiz", score, as.numeric(NA)),
         homework1_score = if_else(type == "homework" & row_number() %% 2 != 0, score, as.numeric(NA)),
         homework2_score = if_else(type == "homework" & row_number() %% 2 == 0, score, as.numeric(NA))) %>%
  group_by(student_id, class_id) %>%
  summarise(across(c(exam_score, quiz_score, homework1_score, homework2_score), ~ max(., na.rm = TRUE)), .groups = 'drop')

if ("score" %in% names(data)) {
  data <- data %>%
    mutate(score_normalized = (score - min(score, na.rm = TRUE)) / (max(score, na.rm = TRUE) - min(score, na.rm = TRUE)))
}

data <- data %>%
  mutate(score_normalized = (exam_score - min(exam_score, na.rm = TRUE)) / 
           (max(exam_score, na.rm = TRUE) - min(exam_score, na.rm = TRUE)))

data <- data %>%
  mutate(grade = case_when(
    score_normalized >= 0.8 ~ "A",
    score_normalized >= 0.7 ~ "B",
    score_normalized >= 0.6 ~ "C",
    score_normalized >= 0.5 ~ "D",
    TRUE ~ "F"
  )) # Assign grades based on normalized score
print(names(data)) # Print column names to verify new columns
print(head(data$grade)) # Print first few rows of grades to check them

#3)	Data Analysis
#3a)	Statistical analysis or exploratory data analysis
# Function to calculate and print descriptive statistics for a given score type
calculate_statistics <- function(data, score_column) {
  summary_stats <- data %>%
    dplyr::summarise(
      Mean = mean(!!rlang::sym(score_column), na.rm = TRUE),
      SD = sd(!!rlang::sym(score_column), na.rm = TRUE),
      Min = min(!!rlang::sym(score_column), na.rm = TRUE),
      Max = max(!!rlang::sym(score_column), na.rm = TRUE)
    )
  print(paste("Statistics for", score_column, ":"))
  print(summary_stats)
}

# Calculate and print statistics for each score type
calculate_statistics(data, "exam_score")
calculate_statistics(data, "quiz_score")
calculate_statistics(data, "homework1_score")
calculate_statistics(data, "homework2_score")

#3b)	Data visualisation
install.packages("ggplot2")
library(ggplot2)

# Histogram of Exam Scores
ggplot(data, aes(x = exam_score)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of Exam Scores")

# Bar chart for grade distribution
ggplot(data, aes(x = grade, fill = grade)) +
  geom_bar(color = "black") +  # Use default stat which is "count", so no need for y-value
  scale_fill_brewer(palette = "Paired") +  # Optional: Add color palette for aesthetics
  labs(x = "Grade", y = "Count of Students", title = "Distribution of Student Grades") +
  theme_minimal() +  # Optional: Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Angle the x-axis text for readability

# Multiple histograms for Homework Scores
ggplot(data) +
  geom_histogram(aes(x = homework1_score, fill = "Homework 1"), alpha = 0.5, bins = 30, position = "identity") +
  geom_histogram(aes(x = homework2_score, fill = "Homework 2"), alpha = 0.5, bins = 30, position = "identity") +
  ggtitle("Comparison of Homework Scores")

# Scatter plot between exam scores and the first homework
ggplot(data, aes(x = exam_score, y = homework1_score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Relationship Between Exam and Homework 1 Scores")

ggplot(data, aes(x = grade, fill = grade)) +
  geom_bar() +
  ggtitle("Grade Distribution")

#4)	Data Modelling
#4a)	Simple linear model
# Simple linear regression model predicting exam scores from homework1 scores
simple_model <- lm(exam_score ~ homework1_score, data = data)
summary(simple_model)

#4b)	General linear model
#4bi)	Predictors are categorical
# General linear model using grade as a categorical predictor
if ("grade" %in% names(data)) {
  glm_categorical <- lm(exam_score ~ grade, data = data)
  summary(glm_categorical)
}

#4bii)	Predictors are categorical and continuous
# GLM with mixed predictors
if ("grade" %in% names(data) && "homework1_score" %in% names(data)) {
  glm_mixed <- lm(exam_score ~ grade + homework1_score, data = data)
  summary(glm_mixed)
}

#4biii)	Predictors are continuous
# GLM with continuous predictors
glm_continuous <- lm(exam_score ~ homework1_score + homework2_score, data = data)
summary(glm_continuous)

#4c)	Model evaluation
# Evaluate simple model
cat("R-squared for Simple Model:", summary(simple_model)$r.squared, "\n")
cat("Residuals for Simple Model:", summary(simple_model)$residuals, "\n")

#4d)	Model Interpretation
# Interpret simple model
coef_simple_model <- coef(summary(simple_model))
print(coef_simple_model)
# Interpret GLM with mixed predictors
if (exists("glm_mixed")) {
  coef_glm_mixed <- coef(summary(glm_mixed))
  print(coef_glm_mixed)
}


