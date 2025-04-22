library(dplyr)
myproject <- read.csv("/Users/alif/Downloads/heart_disease_uci - modified.csv", header = TRUE, sep = ",")

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
nrow_before <- nrow(read.csv("/Users/alif/Downloads/heart_disease_uci - modified.csv"))
nrow_after <- nrow(myproject)

cat("Before:", nrow_before, "After:", nrow_after, "\n")


