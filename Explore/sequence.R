library(tidyverse)
library(ggplot2)
library(lme4)

biomech <- read.csv('poi_metrics.csv')
metadata <- read.csv('metadata.csv')

workingData <- biomech %>% 
  merge(metadata, by = 'session_swing')


# calculate the accelerations
calculate_peak_and_acceleration <- function(data, angular_velocity_columns) {
  # Find peak values
  peak_values <- sapply(angular_velocity_columns, function(col) max(data[[col]], na.rm = TRUE))
  
  # Calculate accelerations based on the provided columns for fm and fp
  accelerations <- sapply(seq(1, length(angular_velocity_columns), by = 2), function(i) {
    fm_col <- angular_velocity_columns[i]
    fp_col <- angular_velocity_columns[i + 1]
    
    
    #print(paste("Checking:", fm_col, "and", fp_col))
    
  
    fm_value <- data[[fm_col]]
    fp_value <- data[[fp_col]]
    
   
    #print(paste("FM Value for", fm_col, ":", fm_value))
    #print(paste("FP Value for", fp_col, ":", fp_value))
    
    # Check for missing 
    if (!is.null(fm_value) && !is.null(fp_value) && !any(is.na(c(fm_value, fp_value)))) {
      return(fp_value - fm_value)  # Change in velocity
    } else {
      return(NA)  # Return NA if unable to find valid values
    }
  })
  
  list(peak_values = peak_values, accelerations = accelerations)
}

# Define the actual angular velocity columns 
pelvis_columns_fm_fp <- c('pelvis_angular_velocity_fm_x', 'pelvis_angular_velocity_fp_x')
torso_columns_fm_fp <- c('torso_angular_velocity_fm_x', 'torso_angular_velocity_fp_x')
shoulder_columns_fm_fp <- c('upper_arm_speed_mag_fm_x', 'upper_arm_speed_mag_fp_x')
hand_columns_fm_fp <- c('hand_speed_mag_fm_x', 'hand_speed_mag_fp_x')

# Run the function and see if it accesses the correct columns
pelvis_results <- calculate_peak_and_acceleration(workingData, pelvis_columns_fm_fp)
torso_results <- calculate_peak_and_acceleration(workingData, torso_columns_fm_fp)
shoulder_results <- calculate_peak_and_acceleration(workingData, shoulder_columns_fm_fp)
hand_results <- calculate_peak_and_acceleration(workingData, hand_columns_fm_fp)

# Extract "accelerations"
pelvis_acc <- pelvis_results$accelerations
torso_acc <- torso_results$accelerations
shoulder_acc <- shoulder_results$accelerations
hand_acc <- hand_results$accelerations

# Add the new acceleration columns to the workingData
workingData$pelvis_acc <- pelvis_acc
workingData$torso_acc <- torso_acc
workingData$shoulder_acc <- shoulder_acc
workingData$hand_acc <- hand_acc


# # Create a function to determine the rank of changes in velocity up to the fourth greatest
# add_top_change_flags <- function(data, pelvis_acc, torso_acc, shoulder_acc, hand_acc) {
#   # Combine the acceleration columns into a matrix for easier comparison
#   acc_matrix <- cbind(pelvis_acc, torso_acc, shoulder_acc, hand_acc)
#   
#   # Create rank columns for each part (0 = greatest, 1 = second greatest, etc.)
#   ranks <- t(apply(acc_matrix, 1, function(x) {
#     # Rank values from greatest to smallest, assigning NA to missing values
#     ranks <- order(-x, na.last = TRUE)
#     ranks[x == -Inf] <- NA  # Handle edge case where all values are NA
#     return(ranks)
#   }))
#   
#   # Add columns to flag the greatest, second, third, and fourth change
#   data <- data %>%
#     mutate(
#       `p-0` = ifelse(ranks[, 1] == 1, 1, 0),
#       `t-0` = ifelse(ranks[, 2] == 1, 1, 0),
#       `s-0` = ifelse(ranks[, 3] == 1, 1, 0),
#       `h-0` = ifelse(ranks[, 4] == 1, 1, 0),
#       `p-1` = ifelse(ranks[, 1] == 2, 1, 0),
#       `t-1` = ifelse(ranks[, 2] == 2, 1, 0),
#       `s-1` = ifelse(ranks[, 3] == 2, 1, 0),
#       `h-1` = ifelse(ranks[, 4] == 2, 1, 0),
#       `p-2` = ifelse(ranks[, 1] == 3, 1, 0),
#       `t-2` = ifelse(ranks[, 2] == 3, 1, 0),
#       `s-2` = ifelse(ranks[, 3] == 3, 1, 0),
#       `h-2` = ifelse(ranks[, 4] == 3, 1, 0),
#       `p-3` = ifelse(ranks[, 1] == 4, 1, 0),
#       `t-3` = ifelse(ranks[, 2] == 4, 1, 0),
#       `s-3` = ifelse(ranks[, 3] == 4, 1, 0),
#       `h-3` = ifelse(ranks[, 4] == 4, 1, 0)
#     )
#   
#   return(data)
# }


# Create a function to determine the rank of changes in velocity
add_top_change_flags <- function(data) {
  # Create an acceleration matrix directly from the data
  acc_matrix <- as.matrix(data %>% select(starts_with("pelvis_acc"), starts_with("torso_acc"), 
                                          starts_with("shoulder_acc"), starts_with("hand_acc")))
  
  # Create rank columns for each part
  ranks <- t(apply(acc_matrix, 1, function(x) {
    order(-x, na.last = TRUE)
  }))
  
  # Add columns to flag the greatest, second, third, and fourth change
  data <- data %>%
    mutate(
      `p-0` = ifelse(ranks[, 1] == 1, 1, 0),
      `t-0` = ifelse(ranks[, 2] == 1, 1, 0),
      `s-0` = ifelse(ranks[, 3] == 1, 1, 0),
      `h-0` = ifelse(ranks[, 4] == 1, 1, 0),
      `p-1` = ifelse(ranks[, 1] == 2, 1, 0),
      `t-1` = ifelse(ranks[, 2] == 2, 1, 0),
      `s-1` = ifelse(ranks[, 3] == 2, 1, 0),
      `h-1` = ifelse(ranks[, 4] == 2, 1, 0),
      `p-2` = ifelse(ranks[, 1] == 3, 1, 0),
      `t-2` = ifelse(ranks[, 2] == 3, 1, 0),
      `s-2` = ifelse(ranks[, 3] == 3, 1, 0),
      `h-2` = ifelse(ranks[, 4] == 3, 1, 0),
      `p-3` = ifelse(ranks[, 1] == 4, 1, 0),
      `t-3` = ifelse(ranks[, 2] == 4, 1, 0),
      `s-3` = ifelse(ranks[, 3] == 4, 1, 0),
      `h-3` = ifelse(ranks[, 4] == 4, 1, 0)
    )
  
  return(data)
}

# columns to the workingData
workingData <- add_top_change_flags(workingData)

# reshape for viz
ranked_data <- workingData %>%
  select(starts_with("p-"), starts_with("t-"), starts_with("s-"), starts_with("h-")) %>%
  gather(key = "body_part_rank", value = "value")
# count for viz
rank_count <- ranked_data %>%
  group_by(body_part_rank) %>%
  summarise(count = sum(value))

# stacked bar plot to visualize the rank counts
ggplot(rank_count, aes(x = body_part_rank, y = count, fill = body_part_rank)) +
  geom_bar(stat = "identity") +
  labs(title = "Rank Count of Body Part Accelerations", x = "Body Part and Rank", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("p-0" = "blue", "t-0" = "green", "s-0" = "orange", "h-0" = "red",
                               "p-1" = "lightblue", "t-1" = "lightgreen", "s-1" = "magenta", "h-1" = "maroon",
                               "p-2" = "darkblue", "t-2" = "darkgreen", "s-2" = "darkorange", "h-2" = "darkred",
                               "p-3" = "blue4", "t-3" = "green4", "s-3" = "orange4", "h-3" = "red4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



workingData <- workingData %>%
  mutate(ranking_order = paste0(
    ifelse(`p-0` == 1, "p-0", ""),
    ifelse(`p-1` == 1, "p-1", ""),
    ifelse(`p-2` == 1, "p-2", ""),
    ifelse(`p-3` == 1, "p-3", ""),
    ifelse(`t-0` == 1, "t-0", ""),
    ifelse(`t-1` == 1, "t-1", ""),
    ifelse(`t-2` == 1, "t-2", ""),
    ifelse(`t-3` == 1, "t-3", ""),
    ifelse(`s-0` == 1, "s-0", ""),
    ifelse(`s-1` == 1, "s-1", ""),
    ifelse(`s-2` == 1, "s-2", ""),
    ifelse(`s-3` == 1, "s-3", ""),
    ifelse(`h-0` == 1, "h-0", ""),
    ifelse(`h-1` == 1, "h-1", ""),
    ifelse(`h-2` == 1, "h-2", ""),
    ifelse(`h-3` == 1, "h-3", ""),
    sep = "-"
  ))
#Define a function to map the column values to body parts and sort them
get_ranking_order <- function(row) {
  #Create a vector of body parts
  body_parts <- c("pelvis", "torso", "shoulder", "hand")
  
  #Create a vector to store the body parts = 1
  selected_parts <- c()
  
  #loop through the body parts (p, t, s, h) and check if any of them have an acceleration of 1
  for (i in 0:3) {
    if (row[[paste0("p-", i)]] == 1) selected_parts <- c(selected_parts, "pelvis")
    if (row[[paste0("t-", i)]] == 1) selected_parts <- c(selected_parts, "torso")
    if (row[[paste0("s-", i)]] == 1) selected_parts <- c(selected_parts, "shoulder")
    if (row[[paste0("h-", i)]] == 1) selected_parts <- c(selected_parts, "hand")
  }
  
  #return the ordered body parts
  return(paste(selected_parts, collapse = ", "))
}

# apply function
workingData <- workingData %>%
  mutate(ranking_order = apply(workingData, 1, get_ranking_order))


# occurrences of each unique ranking order
ranking_counts <- workingData %>%
  count(ranking_order, sort = TRUE)

# visualize the most common ranking orders
ggplot(ranking_counts, aes(x = reorder(ranking_order, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most Common Body Part Change in Velocity (first move to foot plant) Ranking Orders",
       x = "Ranking Order (greatest to slowest)",
       y = "Count") +
  theme_minimal()


# Add the columns to the workingData
# workingData <- add_top_change_flags(
#   workingData,
#   pelvis_acc,
#   torso_acc,
#   shoulder_acc,
#   hand_acc
# )

# data frame for plotting average "accelerations"
avg_changes <- data.frame(
  BodyPart = c("Pelvis", "Torso", "Shoulder", "Hand"),
  AvgChange = c(mean(pelvis_acc, na.rm = TRUE),
                mean(torso_acc, na.rm = TRUE),
                mean(shoulder_acc, na.rm = TRUE),
                mean(hand_acc, na.rm = TRUE))
)

med_changes <- data.frame(
  BodyPart = c("Pelvis", "Torso", "Shoulder", "Hand"),
  medianChange = c(median(pelvis_acc, na.rm = TRUE),
                median(torso_acc, na.rm = TRUE),
                median(shoulder_acc, na.rm = TRUE),
                median(hand_acc, na.rm = TRUE))
)


ggplot(avg_changes, aes(x = BodyPart, y = AvgChange, fill = BodyPart)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Change in Velocity by Body Part",
       y = "Average Change in Velocity",
       x = "Body Part")

ggplot(med_changes, aes(x = BodyPart, y = medianChange, fill = BodyPart)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Median Change in Velocity by Body Part",
       y = "Median Change in Velocity",
       x = "Body Part")

# Combine data for plotting
plot_data <- data.frame(
  BodyPart = rep(c("Pelvis", "Torso", "Shoulder", "Hand"), each = nrow(workingData)),
  ChangeInVelocity = c(pelvis_acc, torso_acc, shoulder_acc, hand_acc)
)

ggplot(plot_data, aes(x = BodyPart, y = ChangeInVelocity, fill = BodyPart)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Change in Velocity by Body Part",
       y = "Change in Velocity",
       x = "Body Part")

# Find the body part with the greatest acceleration change for each row
workingData$max_change <- apply(workingData %>% 
                                  select(starts_with("pelvis_acc"), 
                                         starts_with("torso_acc"),
                                         starts_with("shoulder_acc"), 
                                         starts_with("hand_acc")),
                                1, 
                                function(x) names(x)[which.max(x)])

# Convert max_change to a factor for classification
workingData$max_change <- factor(workingData$max_change)


#Predicting  ranking order 
modelW <- polr(factor(ranking_order) ~ session_mass_lbs + max_change + blast_bat_speed_mph_x.x, 
               data = workingData, Hess = TRUE)

# Summary of the ordinal regression model
summary(modelW)

# Predictions based on weight
workingData$predicted_rank <- predict(modelW, newdata = workingData, type = "class")
workingData <- workingData %>% filter(!is.na(predicted_rank))

# Convert 'ranking_order' to a factor with levels in the correct order
workingData$ranking_order <- factor(workingData$ranking_order, 
                                    levels = c("hand", "pelvis", "torso", "shoulder"))


# Build a classification model to predict max_change based on weight
model_change <- multinom(max_change ~ session_mass_lbs + x_factor_hs_z + exit_velo_mph_x.x, data = workingData)

# Predict the body part with the greatest change based on weight
workingData$predicted_max_change <- predict(model_change, newdata = workingData)



#  Predicted v Actual ranking order based on weight
ggplot(workingData, aes(x = ranking_order, y = predicted_rank)) +
  geom_jitter(aes(color = predicted_rank), width = 0.1, height = 0.1) +  # To add some spread
  labs(title = "Predicted vs Actual Ranking Order",
       x = "Actual Ranking Order", y = "Predicted Ranking Order") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none")  # Remove the legend

# Session Mass (lbs) vs Predicted Ranking Order
ggplot(workingData, aes(x = session_mass_lbs, y = as.factor(predicted_rank), color = as.factor(predicted_rank))) + 
  geom_boxplot(alpha = 0.6) + 
  labs(title = "Player Body Mass (lbs) vs Predicted Ranking Order", 
       x = "Body Mass (lbs)", y = "Predicted Ranking Order") + 
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(session_mass_lbs ~ predicted_rank, data = workingData)

# Print the test result
kruskal_test



#predicted max change vs max change
table_pred_vs_actual <- table(workingData$predicted_max_change, workingData$max_change)
plot_data <- as.data.frame(table_pred_vs_actual)
colnames(plot_data) <- c("Predicted", "Actual", "Count")

# Plot the confusion matrix as a heatmap
ggplot(plot_data, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix of Predicted vs Actual Max Change",
       x = "Predicted Max Change", y = "Actual Max Change") +
  theme_minimal()




# ggplot(workingData, aes(x = weight, fill = predicted_max_change)) +
#   geom_bar() +
#   labs(title = "Predicted Max Change in Velocity Based on Weight", x = "Weight", y = "Count")



#Modeling ranked values multinominal logistic regression
library(nnet)
# Calculate quantiles 
quantiles <- quantile(workingData$blast_bat_speed_mph_x.x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Create a new variable `bat_speed_category` to categorize the bat speed into thirds
workingData$bat_speed_category <- cut(workingData$blast_bat_speed_mph_x.x,
                                      breaks = quantiles,
                                      labels = c("Lower Third", "Middle Third", "Upper Third"),
                                      include.lowest = TRUE)

# Fit multinomial logistic regression model
rankModel <- multinom(bat_speed_category ~ `p-0` + `t-0` + `s-0` + `h-0`, data = workingData)
#DISCUSS this model too
# View the model summary
summary(rankModel)

# Fit a model with only the intercept (no predictors) to get the null deviance
null_model <- multinom(bat_speed_category ~ 1, data = workingData)

# Display the summary of the null model, which includes the null deviance
summary(null_model)



#recode
workingData <- workingData %>%
  mutate(
    # Split ranking_order into a list of ranks for each row
    ranks_list = str_split(ranking_order, ",\\s*"),  # Split by comma and optional space
    
    # Create numeric ranks for each body part
    p_rank = sapply(ranks_list, function(x) which(x == "pelvis")),
    t_rank = sapply(ranks_list, function(x) which(x == "torso")),
    s_rank = sapply(ranks_list, function(x) which(x == "shoulder")),
    h_rank = sapply(ranks_list, function(x) which(x == "hand"))
  ) %>%
  # Replace NA with a large number (or another indicator) if a body part is absent
  mutate(across(c(p_rank, t_rank, s_rank, h_rank), ~ replace_na(.x, Inf)))  # use Inf or NA as appropriate

# workingData <- workingData %>%
#   mutate(
#     p_rank = recode(p_rank, `0` = 4, `1` = 3, `2` = 2, `3` = 1),
#     t_rank = recode(t_rank, `0` = 4, `1` = 3, `2` = 2, `3` = 1),
#     s_rank = recode(s_rank, `0` = 4, `1` = 3, `2` = 2, `3` = 1),
#     h_rank = recode(h_rank, `0` = 4, `1` = 3, `2` = 2, `3` = 1)
#   )
# workingData <- workingData %>%
#   filter(!is.na(bat_speed_category))


mean_bat_speed <- mean(workingData$blast_bat_speed_mph_x.x, na.rm = TRUE)
# categorize the bat speed into above or below average
workingData$bat_speed_average <- ifelse(workingData$blast_bat_speed_mph_x.x > mean_bat_speed, 
                                        "Above Average", 
                                        "Below Average")

workingData$bat_speed_average <- as.factor(workingData$bat_speed_average)


# rankModel2 <- glm(bat_speed_category ~ p_rank + t_rank + s_rank + h_rank,
#                   data = workingData, family = "binomial")
# 
# summary(rankModel2)
# predict the probabilities using the same workingData for prediction
#workingData$predicted_prob <- predict(rankModel2, newdata = workingData, type = "response")

# summarize the data to ensure only one data point per rank
summarized_data <- workingData %>%
  group_by(p_rank, t_rank, s_rank, h_rank) %>%
  summarize(predicted_prob = mean(predicted_prob, na.rm = TRUE),
            bat_speed_average = first(bat_speed_average),  # Assuming bat_speed_average is constant within the group
            .groups = 'drop')


summarized_data$bat_speed_binary <- ifelse(summarized_data$bat_speed_average == "Above Average", 1, 0)

# Fit a logistic regression model
modelsummarized_data <- glm(bat_speed_binary ~ p_rank + t_rank + s_rank + h_rank, data = summarized_data, family = binomial)
summary(modelsummarized_data)

#predict probabilities for new data
new_data <- data.frame(p_rank = 3, t_rank = 2, s_rank = 1, h_rank = 4)
predict(modelsummarized_data, newdata = new_data, type = "response")

ggplot(summarized_data, aes(x = p_rank, y = predicted_prob, color = bat_speed_average)) +
  geom_point(aes(shape = "Pelvis"), alpha = 0.7) +
  geom_point(aes(x = t_rank, y = predicted_prob, shape = "Torso"), alpha = 0.7) +
  geom_point(aes(x = s_rank, y = predicted_prob, shape = "Shoulder"), alpha = 0.7) +
  geom_point(aes(x = h_rank, y = predicted_prob, shape = "Hand"), alpha = 0.7) +
  labs(title = "Predicted Probability of Bat Speed by Body Part Ranks",
       x = "Rank (4 represents smallest change in velocity)",
       y = "Predicted Probability",
       color = "Bat Speed Category") +
  scale_shape_manual(values = c("Pelvis" = 16, "Torso" = 17, "Shoulder" = 18, "Hand" = 21)) +
  theme_minimal() +
  facet_wrap(~ bat_speed_average, scales = "free")  # Facet by bat speed category


#MODELING acceleration values
#MODEL 1
#mixed effects on bat speed
# Fit the mixed model
mixed_model <- lmer(blast_bat_speed_mph_x.x ~ pelvis_acc + torso_acc + shoulder_acc + hand_acc + (1 | session.x), data = workingData)
summary(mixed_model)

# Plot diagnostic plots for the mixed model
plot(mixed_model)

# Generate predictions
predicted_batspeed <- predict(mixed_model, newdata = workingData, allow.new.levels = TRUE)

# Ensure the length of predictions matches the rows in the data
if (length(predicted_batspeed) == nrow(workingData)) {
  # Add the predicted values to the dataframe
  workingData$predicted_batspeed <- predicted_batspeed
  
  # Plot predicted bat speed by hitter side
  ggplot(workingData, aes(x = hitter_side, y = predicted_batspeed, fill = hitter_side)) +
    geom_boxplot() +
    labs(title = "Predicted Bat Speed by Hitter Side based on Changes in Velocity",
         x = "Hitter Side",
         y = "Predicted Bat Speed") +
    theme_minimal() +
    scale_fill_manual(values = c("left" = "blue", "right" = "red"))
} else {
  # Handle mismatch case
  warning("Prediction length does not match number of rows in workingData.")
}

  
# Calculate R-squared for mixed model
r2_mixed_model <- r.squaredGLMM(mixed_model) # R-squared values accounting for fixed and random effects
print(r2_mixed_model)

# Plot partial effects for each variable using ggplot (example for pelvis_acc)
ggplot(workingData, aes(x = pelvis_acc, y = blast_bat_speed_mph_x.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of Pelvis Acceleration on Bat Speed",
       x = "Pelvis Acceleration",
       y = "Bat Speed (mph)") +
  theme_minimal()

# generate predictions on the data used for fitting the model
# predicted_values <- predict(mixed_model)

# Check if the length matches
# #length(predicted_values)  # This should return a number less than or equal to 677
# na_rows <- which(is.na(workingData$pelvis_acc) | 
#                    is.na(workingData$torso_acc) | 
#                    is.na(workingData$shoulder_acc) | 
#                    is.na(workingData$hand_acc))
# 
# #new dataframe with only complete cases
# complete_data <- workingData[-na_rows, ]
# complete_data$predicted_batspeed <- predicted_values

# new column with NA for rows where predictions were not made
workingData <- merge(workingData, complete_data[, c("session.x", "predicted_batspeed")], 
                     by = "session.x", all.x = TRUE)
ggplot(workingData, aes(x = pelvis_acc, y = predicted_batspeed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Predicted Bat Speed Based on Pelvis Acceleration",
       x = "Pelvis Acceleration",
       y = "Predicted Bat Speed (mph)") +
  theme_minimal()


ggplot(workingData, aes(x = pelvis_acc, y = blast_bat_speed_mph_x.x, color = torso_acc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Interaction of Pelvis and Torso Acceleration on Bat Speed",
       x = "Pelvis Acceleration",
       y = "Bat Speed (mph)") +
  theme_minimal()
library(plotly)

# Filter out NA values
workingData_filtered <- workingData %>%
  filter(!is.na(pelvis_acc) & !is.na(torso_acc) & !is.na(shoulder_acc) & !is.na(hand_acc))


# cool 3d viz
# plot_ly(
#   data = workingData_filtered,
#   x = ~pelvis_acc[,1],
#   y = ~torso_acc[,1],
#   z = ~shoulder_acc[,1],
#   color = ~hand_acc[,1],  # Fourth variable represented by color
#   type = 'scatter3d',
#   mode = 'markers',
#   marker = list(size = 3)  # Set a fixed marker size to avoid line.width warnings
# ) %>%
#   layout(
#     title = "Interactions of Changes in Velocity",
#     scene = list(
#       xaxis = list(title = "Pelvis Change in Velocity"),
#       yaxis = list(title = "Torso Change in Velocity"),
#       zaxis = list(title = "Shoulder Change in Velocity")
#     )
#   )
# 
# #Torso angle model mixed
# torso_model <- lmer(torso_angle_fp_z ~ pelvis_acc + torso_acc + shoulder_acc + hand_acc + (1 | session.x), data = workingData)
# summary(torso_model)
# plot(torso_model)


# torso angle linear regression
# lm_model <- lm(torso_angle_fp_z ~ pelvis_acc + torso_acc + shoulder_acc + hand_acc, data = workingData)
# summary(lm_model) # r squared .68
# 
# 
# batspeed_lm_model <- lm(blast_bat_speed_mph_x.x ~ pelvis_acc + torso_acc + shoulder_acc + hand_acc, data = workingData)
# summary(batspeed_lm_model) # r squared .02

# Calculate the differences between accelerations
workingData$pelvis_torso_diff <- workingData$pelvis_acc - workingData$torso_acc
workingData$torso_shoulder_diff <- workingData$torso_acc - workingData$shoulder_acc
workingData$shoulder_hand_diff <- workingData$shoulder_acc - workingData$hand_acc
workingData$torso_angle_diff <- workingData$torso_angle_fp_z - workingData$torso_angle_fm_z

#MODEL 2
# Fit the linear mixed with the difference variables
batspeed_lmer_model2 <- lmer(blast_bat_speed_mph_x.x ~ pelvis_torso_diff + torso_shoulder_diff + shoulder_hand_diff + (1 | session.x), data = workingData)
summary(batspeed_lm_model2) 



# Generate predictions
predicted_batspeed2 <- predict(batspeed_lmer_model2, newdata = workingData, allow.new.levels = TRUE)

# Ensure the length of predictions matches the rows in the data
if (length(predicted_batspeed2) == nrow(workingData)) {
  # Add the predicted values to the dataframe
  workingData$predicted_batspeed2 <- predicted_batspeed2
  
  # Plot predicted bat speed by hitter side
  ggplot(workingData, aes(x = hitter_side, y = predicted_batspeed2, fill = hitter_side)) +
    geom_boxplot() +
    labs(title = "Predicted Bat Speed using Difference in Body Part Change in Velocity",
         x = "Hitter Side",
         y = "Predicted Bat Speed") +
    theme_minimal() +
    scale_fill_manual(values = c("left" = "blue", "right" = "red"))
} else {
  # Handle mismatch case
  warning("Prediction length does not match number of rows in workingData.")
}

# Fit the linear regression model with the difference variables
# torsoangle_lm_model2 <- lm(torso_angle_diff ~ pelvis_torso_diff + torso_shoulder_diff + shoulder_hand_diff, data = workingData)
# summary(torsoangle_lm_model2) # r squared .46
# plot(torsoangle_lm_model2)


#linear regression model with the difference variables
# torsoangle_lmer_model2 <- lmer(torso_angle_diff ~ pelvis_torso_diff + torso_shoulder_diff + shoulder_hand_diff + (1 | session.x), data = workingData)

# Summary of the model
# summary(torsoangle_lmer_model2) 
# plot(torsoangle_lm_model2)
# 
# workingData$predicted_torso_angle_diff <- predict(torsoangle_lmer_model2)
# ggplot(workingData, aes(x = hitter_side, y = predicted_torso_angle_diff, fill = hitter_side)) +
#   geom_boxplot() +
#   labs(title = "Predicted Torso Angle Difference by Hitter Side",
#        x = "Hitter Side",
#        y = "Predicted Torso Angle Difference") +
#   theme_minimal() +
#   scale_fill_manual(values = c("left" = "blue", "right" = "red"))  # Use fill to color boxes
# 
# 

#scaling accelerations to see impact on bat speed
workingData$scaled_pelvis_acc <- scale(workingData$pelvis_acc)
workingData$scaled_torso_acc <- scale(workingData$torso_acc)
workingData$scaled_shoulder_acc <- scale(workingData$shoulder_acc)
workingData$scaled_hand_acc <- scale(workingData$hand_acc)

# scaledBatspeed_lm_model <- lm(blast_bat_speed_mph_x.x ~ scaled_pelvis_acc + scaled_torso_acc + scaled_shoulder_acc + scaled_hand_acc, data = workingData)
# summary(batspeed_lm_model) #r squared .02


# predictors <- workingData %>%
#   select(-session_swing, -session.x, -blast_bat_speed_mph_x.x, -predicted_batspeed.y)  # Exclude target variable and unique IDs

#linear regression bat speed
# lm_modelbig <- lm(blast_bat_speed_mph_x ~ ., data = data.frame(blast_bat_speed_mph_x = workingData$blast_bat_speed_mph_x.x, predictors))
# summary(lm_modelbig)

#Variables with low pvalues and relatively high coefficients: pelvis angle fm x, torso angle fm x, x factor fm x



# tAndPmodel <- lmer(torso_angle_fp_z ~ (scaled_pelvis_acc * scaled_torso_acc) + (scaled_shoulder_acc * scaled_hand_acc) 
#                    +  (1 | session.x), data = workingData)
# summary(tAndPmodel)
# plot(tAndPmodel)
# cooks_distance <- cooks.distance(tAndPmodel)
# plot(cooks_distance)
# abline(h = 4 / length(cooks_distance), col = "red")  # Threshold line for influential points
# Remove rows with Cook's distance greater than 4/n (where n is the number of observations)
# clean_data <- workingData[cooks_distance <= 4 / nrow(workingData), ]
# # Refit the model using the cleaned dataset 
# tAndPmodel_clean <- lmer(torso_angle_fp_z ~ (scaled_pelvis_acc * scaled_torso_acc) + 
#                            (scaled_shoulder_acc * scaled_hand_acc) + (1 | session.x), data = clean_data)
# summary(tAndPmodel_clean)

#test for correlation, if highly correlated, will need a scaled value
cor.test(workingData$pelvis_acc, workingData$max_cog_velo_x)
#not highly correlated, and different units, so okay to use normal variable #mODEL 4
tAndPmodelBS <- lmer(blast_bat_speed_mph_x.x ~ (scaled_pelvis_acc * scaled_torso_acc)  + (scaled_shoulder_acc * scaled_hand_acc) + session_mass_lbs
                   +  (1 | session.x), data = workingData)
summary(tAndPmodelBS)
plot(tAndPmodelBS)

# Generate predictions
predicted_batspeed3 <- predict(tAndPmodelBS, newdata = workingData, allow.new.levels = TRUE)

# Ensure the length of predictions matches the rows in the data
if (length(predicted_batspeed3) == nrow(workingData)) {
  # Add the predicted values to the dataframe
  workingData$predicted_batspeed3 <- predicted_batspeed3
  
  # Plot predicted bat speed by hitter side
  ggplot(workingData, aes(x = hitter_side, y = predicted_batspeed3, fill = hitter_side)) +
    geom_boxplot() +
    labs(title = "Predicted Bat Speed using Interaction Terms",
         x = "Hitter Side",
         y = "Predicted Bat Speed") +
    theme_minimal() +
    scale_fill_manual(values = c("left" = "blue", "right" = "red"))
} else {
  # Handle mismatch case
  warning("Prediction length does not match number of rows in workingData.")
}

# ggplot(workingData, aes(x= blast_bat_speed_mph_x.x, y = session_mass_lbs )) +
#   geom_point()
# 
# ggplot(workingData, aes(x= exit_velo_mph_x.x, y = session_mass_lbs )) +
#   geom_point()
