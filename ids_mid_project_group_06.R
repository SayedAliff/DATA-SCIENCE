library(dplyr)
myproject <- read.csv("F:\\Data Science Mid Project\\heart_disease_uci - modified.csv", header = TRUE, sep = ",")

myproject

#q1

#Check for Missing Values in All Columns
myproject %>% summarise_all(~sum(is.na(.)))

#Replace Missing Values in Numeric Columns with Mean
myproject <- myproject %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#Replace Missing Values in Categorical Columns with "Unknown"
myproject <- myproject %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "Unknown", .)))


#q2

#Show Count of Missing Values in Each Column
myproject %>% summarise_all(~sum(is.na(.)))

#View All Rows That Contain Missing Values
myproject %>% filter(if_any(everything(), is.na))

#Find Row Numbers That Contain NA (Optional)
which(rowSums(is.na(myproject)) > 0)


#Q3

# Step 1: Detect Outliers (Before modifying anything)
numeric_cols <- myproject %>% select(where(is.numeric))

outlier_summary_before <- numeric_cols %>%
  summarise(across(everything(), ~{
    Q1 <- quantile(., 0.25, na.rm = TRUE)
    Q3 <- quantile(., 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    sum(. < (Q1 - 1.5 * IQR_val) | . > (Q3 + 1.5 * IQR_val))
  }))
print(outlier_summary_before)

# Step 2: Handle Outliers (Replace with Median)
myproject <- myproject %>%
  mutate(across(where(is.numeric), ~{
    Q1 <- quantile(., 0.25, na.rm = TRUE)
    Q3 <- quantile(., 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    median_val <- median(., na.rm = TRUE)
    ifelse(. < lower | . > upper, median_val, .)
  }))

# Step 3: Detect Outliers Again (To verify handling worked)
numeric_cols_updated <- myproject %>% select(where(is.numeric))

outlier_summary_after <- numeric_cols_updated %>%
  summarise(across(everything(), ~{
    Q1 <- quantile(., 0.25, na.rm = TRUE)
    Q3 <- quantile(., 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    sum(. < (Q1 - 1.5 * IQR_val) | . > (Q3 + 1.5 * IQR_val))
  }))
print(outlier_summary_after)

#Q4

#Convert age (Numeric → Categorical)
myproject <- myproject %>%
  mutate(age_group = case_when(
    age < 40 ~ "Young",
    age >= 40 & age <= 60 ~ "Middle-aged",
    age > 60 ~ "Senior"
  ))

#Convert sex (Categorical → Numeric)
myproject <- myproject %>%
  mutate(
    sex_label = case_when(
      sex %in% c("Male") ~ "Male",
      sex %in% c("Female") ~ "Female",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(sex_numeric = as.numeric(factor(sex_label)))


#Q5

#Normalize it using mutate()
myproject <- myproject %>%
  mutate(chol_normalized = (chol - min(chol, na.rm = TRUE)) / 
           (max(chol, na.rm = TRUE) - min(chol, na.rm = TRUE)))

#Preview the Output (optional)
myproject %>% select(chol, chol_normalized) %>% head(10)


#Q6

#Step 1: Detect Duplicate Rows
myproject %>%
  duplicated() %>%
  sum()

#Step 2: Remove Duplicates (Keep Only Unique Rows)
myproject <- myproject %>%
  distinct()

#Optional: View rows before & after
nrow_before <- nrow(read.csv("F:\\Data Science Mid Project\\heart_disease_uci - modified.csv"))
nrow_after <- nrow(myproject)

cat("Before:", nrow_before, "After:", nrow_after, "\n")

#q7


# Filter 1: Patients older than 60
older_patients <- myproject %>%
  filter(age > 60)

head(older_patients)

# Filter 2: Patients with high cholesterol and high BP
high_risk <- myproject %>%
  filter(chol > 240 & trestbps > 140)

head(high_risk)

# Filter 3: Female patients with heart disease
female_hd <- myproject %>%
  filter(sex_label == "Female" & num > 0)

head(female_hd)


#q8

# Invalid age
myproject %>% filter(age < 1 | age > 100)

# Invalid sex
myproject %>% filter(sex %in% c("Male", "Female"))

# Invalid thalch
myproject %>% filter(thalch < 60 | thalch > 220)

# Invalid cholesterol
myproject %>% filter(chol <= 0)

myproject <- myproject %>%
  mutate(age = ifelse(age < 1 | age > 100, NA, age),
         sex = ifelse(sex %in% c("Male", "Female"), "Unknown", sex),
         thalach = ifelse(thalch < 60 | thalch > 220, NA, thalch),
         chol = ifelse(chol <= 0, NA, chol))


#q9

#Step 1: Check Class Distribution

myproject %>%
  group_by(num) %>%
  summarise(count = n())

# Step 2: Convert to Binary Classes (if needed
myproject <- myproject %>%
  mutate(heart_disease = ifelse(num == 0, 0, 1))

# Step 3: Balance the Data (Undersample the majority class)
# Count samples in each class
table(myproject$heart_disease)

# Separate classes
class0 <- myproject %>% filter(heart_disease == 0)
class1 <- myproject %>% filter(heart_disease == 1)

# Undersample class 0 to match class 1
balanced_myproject <- bind_rows(
  sample_n(class0, nrow(class1)),  # randomly pick same number as class1
  class1
)

# Optional: Check balance

balanced_myproject %>%
  group_by(heart_disease) %>%
  summarise(count = n())


#q10
#Step 1: Shuffle the data randomly using sample_frac(1)
set.seed(123)  # for reproducibility

shuffled_data <- myproject %>% sample_frac(1)

#Step 2: Create training and testing datasets
# 70% training set
train_data <- shuffled_data %>% slice(1:floor(0.7 * nrow(.)))

# 30% testing set
test_data <- shuffled_data %>% slice((floor(0.7 * nrow(.)) + 1):nrow(.))

#Optional: Check row counts
nrow(train_data)
nrow(test_data)

#q11

#Step 1: Central Tendencies for Numeric Attributes

#For age and chol
# Mean and Median
myproject %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mean_chol = mean(chol, na.rm = TRUE),
    median_chol = median(chol, na.rm = TRUE)
  )

# Mode function (custom, for numeric)
get_mode <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}

mode_age <- get_mode(myproject$age)
mode_chol <- get_mode(myproject$chol)

print(paste("Mode of age:", mode_age))
print(paste("Mode of cholesterol:", mode_chol))

# Step 2: Central Tendency for Categorical Attributes

#Mode for sex and cp
# Mode using dplyr (most frequent)
myproject %>%
  count(sex) %>%
  arrange(desc(n)) %>%
  slice(1)

myproject %>%
  count(cp) %>%
  arrange(desc(n)) %>%
  slice(1)


#q12

myproject %>%
  summarise(
    range_age = max(age, na.rm = TRUE) - min(age, na.rm = TRUE),
    IQR_age = IQR(age, na.rm = TRUE),
    var_age = var(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    
    range_chol = max(chol, na.rm = TRUE) - min(chol, na.rm = TRUE),
    IQR_chol = IQR(chol, na.rm = TRUE),
    var_chol = var(chol, na.rm = TRUE),
    sd_chol = sd(chol, na.rm = TRUE)
  )






