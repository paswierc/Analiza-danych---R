library(dplyr)
library(janitor)
library(tidymodels)
library(tidyverse)

mobiledf <- read.csv(file ="https://raw.githubusercontent.com/paswierc/Analiza-danych---R/refs/heads/main/user_behavior_dataset.csv")%>%

#Clean column names
clean_names()

slice_head(mobiledf, n = 10)

#change Android and iOS to 1 and 2 in operating_system column
mobiledf <- mobiledf %>%
  mutate(operating_system = case_when(
    operating_system == "Android" ~ 1,
    operating_system == "iOS" ~ 2,
    TRUE ~ NA_integer_ 
  ))

#change Male and Female to 1 and 2 in gender
mobiledf <- mobiledf %>%
  mutate(gender = case_when(
    gender == "Male" ~ 1,
    gender == "Female" ~ 2
  ))

slice_head(mobiledf, n = 10)


# Encode user_behavior_class as category
mobiledf_select <- mobiledf %>%
mutate(user_behavior_class = factor(user_behavior_class, levels = c("1","2","3","4","5"))) %>% 

  # Drop User ID and device_model column
select(-user_id, -device_model) 

# Print the first 10 rows
slice_head(mobiledf_select, n = 10)

# Pivot data to a long format
mobiledf_long <- mobiledf_select %>% 
  pivot_longer(!user_behavior_class, names_to = "features", values_to = "values")

# Print the first 10 rows
mobiledf_long %>% 
  slice_head(n = 10)

theme_set(theme_light())

#Make a box plot for each predictor feature
mobiledf_long %>% 
  ggplot(mapping = aes(x = user_behavior_class, y = values, fill = features)) +
  geom_boxplot() + 
  facet_wrap(~ features, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none")


# Split data into 70% for training and 30% for testing
set.seed(2056)
mobiledf_split <- mobiledf_select %>% 
  initial_split(prop = 0.70)


# Extract the data in each split
mobiledf_train <- training(mobiledf_split)
mobiledf_test <- testing(mobiledf_split)


# Print the number of cases in each split
cat("Training cases: ", nrow(mobiledf_train), "\n",
    "Test cases: ", nrow(mobiledf_test), sep = "")


# Print out the first 10 rows of the training set
mobiledf_train %>% 
  slice_head(n = 10)


install.packages("parsnip")
install.packages("nnet")
library(parsnip)
library(nnet)

# Train a logistic multiple regression model
multinom_spec <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")


#print the model specification
multinom_spec

multinom_fit <- multinom_spec %>% 
  fit(user_behavior_class ~ ., data = mobiledf_train)

#print the model object
multinom_fit

# Make predictions then bind them to the test set
results <- mobiledf_test %>% select(user_behavior_class) %>% 
  bind_cols(multinom_fit %>% predict(new_data = mobiledf_test))

# Compare predictions 
results %>% 
  slice_head(n = 10)

# Calculate accuracy: proportion of data predicted correctly
results %>% 
  metrics(truth = user_behavior_class, estimate = .pred_class) %>% 
  filter(.metric == "accuracy")

##################################################################

# model 2- model using less predictors - removing highly correlated predictors

# Encode Diabetic as category
mobiledf_select2 <- mobiledf %>%
mutate(user_behavior_class = factor(user_behavior_class, levels = c("1","2","3","4","5"))) %>% 
# remove higly correlated predictors
select(-user_id, -device_model, -age, -app_usage_time_min_day, -screen_on_time_hours_day, -battery_drain_m_ah_day, -number_of_apps_installed)

slice_head(mobiledf_select2, n = 10)

# Pivot data to a long format
mobiledf_long2 <- mobiledf_select2 %>% 
  pivot_longer(!user_behavior_class, names_to = "features", values_to = "values")

# Print the first 10 rows
mobiledf_long2 %>% 
  slice_head(n = 10)

# Split data into 70% for training and 30% for testing
set.seed(2056)
mobiledf_split2 <- mobiledf_select2 %>% 
  initial_split(prop = 0.70)

# Extract the data in each split
mobiledf_train2 <- training(mobiledf_split2)
mobiledf_test2 <- testing(mobiledf_split2)

# Print the number of cases in each split
cat("Training cases: ", nrow(mobiledf_train2), "\n",
    "Test cases: ", nrow(mobiledf_test2), sep = "")

# Print out the first 10 rows of the training set
mobiledf_train2 %>% 
  slice_head(n = 10)

# Train a logistic multiple regression model
multinom_spec2 <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")

#print the model specification
multinom_spec2


multinom_fit2 <- multinom_spec2 %>% 
  fit(user_behavior_class ~ ., data = mobiledf_train)

#print the model object
multinom_fit2

# Make predictions then bind them to the test set
results2 <- mobiledf_test2 %>% select(user_behavior_class) %>% 
  bind_cols(multinom_fit2 %>% predict(new_data = mobiledf_test))

# Compare predictions 
results2 %>% 
  slice_head(n = 10)

print("Model 2 accuracy:")
# Calculate accuracy: proportion of data predicted correctly
results2 %>% 
  metrics(truth = user_behavior_class, estimate = .pred_class) %>% 
  filter(.metric == "accuracy")

print("Model 1 accuracy:")
# Calculate accuracy: proportion of data predicted correctly
results %>% 
  metrics(truth = user_behavior_class, estimate = .pred_class) %>% 
  filter(.metric == "accuracy")




