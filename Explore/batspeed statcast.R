statcast <- read.csv('stats (1).csv')



#data wrangle to merge


library(Lahman)

player_weights_heights <- People[, c("playerID", "nameFirst", "nameLast", "weight", "height")]
People$Name <- paste(People$nameLast, People$nameFirst, sep = ", ")

statcast <- merge(statcast, People[, c("Name", "height", "weight")],
                  by.x = "last_name..first_name", by.y = "Name", all.x = TRUE)
# statcast <- statcast %>%
#   select(-Height.inches., -Weight.pounds.)

statcast_clean <- na.omit(statcast)

colnames(statcast_clean)


#Training model
library(randomForest)

rf_model <- randomForest(avg_swing_speed ~ weight + xwoba + xiso + avg_swing_length + barrel_batted_rate + hard_hit_percent,  
                         data = model_data)
summary(rf_model)
# create a weight range
weight_range <- seq(170, 240, by = 1)

# prediction data
predict_data <- data.frame(
  weight = weight_range,
  xwoba = rep(mean(model_data$xwoba), length(weight_range)),  # Use mean of xwoba
  xiso = rep(mean(model_data$xiso), length(weight_range)),    # Use mean of xiso
  avg_swing_length = rep(mean(model_data$avg_swing_length), length(weight_range)),  # Use mean of avg_swing_length
  barrel_batted_rate = rep(mean(model_data$barrel_batted_rate), length(weight_range)),  # Use mean of barrel rate
  hard_hit_percent = rep(mean(model_data$hard_hit_percent), length(weight_range))  # Use mean of hard hit %
)

# predict bat speed
predicted_bat_speed <- predict(rf_model, newdata = predict_data)

# predicted bat speeds with their corresponding weights
result <- data.frame(
  weight = weight_range,
  predicted_bat_speed = predicted_bat_speed
)

# Objective function to calculate the desired optimization value
objective_function <- function(weight, model_data, rf_model, alpha = 1, beta = 1, gamma = 1) {
  
  # Ensure that avg_swing_speed is available in model_data
  if (!"avg_swing_speed" %in% colnames(model_data)) {
    stop("avg_swing_speed column is missing from model_data!")
  }
  
  # Prepare prediction data
  predict_data <- data.frame(
    weight = rep(weight, nrow(model_data)),  # Repeat weight for each row of model_data
    avg_swing_speed = model_data$avg_swing_speed,  # Use avg_swing_speed from model_data
    xwoba = mean(model_data$xwoba),  # Use mean xwoba from model_data
    xiso = mean(model_data$xiso),  # Use mean xiso from model_data
    avg_swing_length = mean(model_data$avg_swing_length),  # Use mean avg_swing_length from model_data
    barrel_batted_rate = mean(model_data$barrel_batted_rate),  # Use mean barrel_batted_rate
    hard_hit_percent = mean(model_data$hard_hit_percent)  # Use mean hard_hit_percent
  )
  
  # Debugging
  print(paste("Predict Data for Weight", weight, ":", toString(predict_data)))
  
  # Make predictions with the random forest model to predict bat speed
  predicted_bat_speed <- predict(rf_model, newdata = predict_data)
  
  # Debugging
  print(paste("Predicted Bat Speed:", predicted_bat_speed))
  
  predicted_bat_speed <- as.numeric(predicted_bat_speed)
  
  # Calculate the objective value
  # Maximize xwoba (positive), minimize avg_swing_length (negative)
  objective_value <- alpha * predicted_bat_speed + beta * mean(model_data$xwoba) - gamma * mean(model_data$avg_swing_length)
  
  return(objective_value)
}

print(result)

# result <- data.frame(weight = weight_range, predicted_bat_speed = predicted_bat_speed)

# linear regression model to see if weight affects optimal bat speed
linear_model <- lm(predicted_bat_speed ~ weight, data = result)
model_summary <- summary(linear_model)
print(model_summary)

# pvalue
p_value_weight <- model_summary$coefficients["weight", "Pr(>|t|)"]
print(paste("P-value for weight:", p_value_weight))



ggplot(result, aes(x = weight, y = predicted_bat_speed)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of the data points
  geom_smooth(method = "lm", color = "red") +  # Add a linear regression line with CI
  labs(title = "Optimal Predicted Bat Speed vs Weight (With Confidence Interval)", 
       x = "Weight (lbs)", 
       y = "Predicted Bat Speed") +
  theme_minimal()  # Clean theme for the plot
# mean_rf_rsq <- mean(rf_model$rsq)
# mean_rf_rsq
