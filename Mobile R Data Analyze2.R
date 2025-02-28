library(janitor)
library(dplyr)
library(ggplot2)

mobiledf <- read.csv(file ="https://raw.githubusercontent.com/paswierc/Analiza-danych---R/refs/heads/main/user_behavior_dataset.csv")%>%

#Clean column names
clean_names()

slice_head(mobiledf, n = 10)

#Information about the dataset
str(mobiledf)

#Summary of the dataset
summary(mobiledf)

#Checking for missing values
sum(is.na(mobiledf))

#Checking for duplicates
sum(duplicated(mobiledf))

#drop user_id and device_model columns
mobiledf <- mobiledf %>%
  select(-user_id, -device_model)

#count iOS and Androind users and create a pie chart
mobiledf %>%
  count(operating_system) %>%
  ggplot(mapping = aes(x = "", y = n, fill = operating_system)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

#count Male and Female users and create a pie chart
mobiledf %>%
  count(gender) %>%
  ggplot(mapping = aes(x = "", y = n, fill=gender)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

#calculate mean to screen_on_time_hours_day grouped by gender
mobiledf %>%
  group_by(gender) %>%
  summarise(mean_screen_on_time_hours_day = mean(screen_on_time_hours_day, na.rm = TRUE))

#calulate mean to user_behavior_class grouped by gender
mobiledf %>%
  group_by(gender) %>%
  summarise(mean_user_behavior_class = mean(user_behavior_class, na.rm = TRUE))

#calculate mean to number_of_apps_installed grouped by gender
mobiledf %>%
  group_by(gender) %>%
  summarise(mean_number_of_apps_installed = mean(number_of_apps_installed, na.rm = TRUE))
  
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

#find correlation between independent variables
cor(mobiledf[,c(1:9)], use = "complete.obs")


#create 3 groups of age column
mobiledf <- mobiledf %>%
  mutate(age_group = case_when(
    age < 30 ~ "young",
    age >= 30 & age < 50 ~ "middle",
    age >= 50 ~ "old"
  ))

#calculate mean to screen_on_time_hours_day grouped by age_group
mobiledf %>%
  group_by(age_group) %>%
  summarise(mean_screen_on_time_hours_day = mean(screen_on_time_hours_day, na.rm = TRUE)) %>%
  arrange(mean_screen_on_time_hours_day)

#calculate mean to user_behavior_class grouped by age_group
mobiledf %>%
  group_by(age_group) %>%
  summarise(mean_user_behavior_class = mean(user_behavior_class, na.rm = TRUE)) %>%
  arrange(mean_user_behavior_class)


#calclate mean to screen_on_time_hours_day grouped by number_of_apps_installed
mobiledf %>%
  group_by(number_of_apps_installed) %>%
  summarise(mean_screen_on_time_hours_day = mean(screen_on_time_hours_day, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = number_of_apps_installed, y = mean_screen_on_time_hours_day)) +
  geom_point() +
  geom_smooth(method = "lm")


#calculate mean to number_of_apps_installed grouped by age
mobiledf %>%
  group_by(age) %>%
  summarise(mean_number_of_apps_installed = mean(number_of_apps_installed, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = age, y = mean_number_of_apps_installed)) +
  geom_point() +
  geom_smooth(method = "lm")
library(dplyr)
library(ggplot2)
library(reshape2)

# Create age bins
mobiledf <- mobiledf %>%
  mutate(Age_Group = cut(age, breaks = c(0, 18, 30, 50, 80), labels = c('Children', 'Young Adults', 'Adults', 'Seniors')))

# Group and calculate mean screen time
screen_time <- mobiledf %>%
  group_by(Age_Group, user_behavior_class) %>%
  summarise(Screen_On_Time = mean(screen_on_time_hours_day, na.rm = TRUE), .groups = 'drop')

# Reshape the data for plotting
screen_time_melt <- melt(screen_time, id.vars = c("Age_Group", "user_behavior_class"), measure.vars = "Screen_On_Time")


# show data from screen_time_melt
head(screen_time_melt)

#convert to discrete scale screen_on_time 
screen_time_melt$user_behavior_class <- factor(screen_time_melt$user_behavior_class, levels = c("1","2","3","4","5"))

# Plotting
ggplot(screen_time_melt, aes(x = Age_Group, y = value, fill = user_behavior_class)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Screen On Time by Age Group and User Behavior Class",
       x = "Age Group",
       y = "Screen On Time (hours/day)") +
  scale_fill_manual(values = c("mediumpurple", "mediumpurple1", "mediumpurple2", "mediumpurple3", "mediumpurple4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"
  ))


