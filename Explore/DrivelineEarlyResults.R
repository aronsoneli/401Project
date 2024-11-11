library(tidyverse)
library(ggplot2)
library(reshape2)
#Load in driveline data
biomech <- read.csv('poi_metrics.csv')
metadata <- read.csv('metadata.csv')

workingData <- biomech %>% 
  merge(metadata, by = 'session_swing')
  
## EXPLORATION
# Summary statistics for exit velo
summary(workingData$exit_velo_mph_x.x)

# Summary statistics for bat speed
summary(workingData$blast_bat_speed_mph_x.x)

# Summary statistics for pelvis angular velocity @ fp
summary(workingData$pelvis_angular_velocity_fp_x)

# Summary statistics for hand speed
summary(workingData$hand_speed_blast_bat_mph_max_x)

# Summary statistics for torso  angle in z direction
summary(workingData$torso_angle_fp_z)

# Summary statistics for torso  angle in y direction
summary(workingData$torso_angle_fp_y)



correlation_data <- workingData %>%
  select(exit_velo_mph_x.x, blast_bat_speed_mph_x.x, pelvis_angular_velocity_fp_x,
         hand_speed_blast_bat_mph_max_x, torso_angle_fp_z, torso_angle_fp_y)

# Correlation matrix
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

melted_cor_matrix <- melt(cor_matrix)

ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap of Correlations with Exit Velocity")




#Define Attack Angle
workingData <- workingData %>%
  mutate(attack_angle_category = case_when(
    attack_angle_contact_x < 0 ~ "Steep",
    attack_angle_contact_x >= 0 & attack_angle_contact_x <= 10 ~ 'Flat',
    attack_angle_contact_x >10 ~ 'Upward'))

#identify what impacts exit velocity or attack angle
summary(data_clean$exit_velo_mph_x.x)
hist(data_clean$exit_velo_mph_x.x)
data_clean <- workingData %>%
  select(attack_angle_contact_x, 
         torso_angle_fp_y, pelvis_angular_velocity_fp_x, 
         torso_angular_velocity_fp_x, torso_launchpos_y, 
         torso_angle_fp_y, torso_angle_fp_z, exit_velo_mph_x.x)
drop_na(data_clean)
data_clean <- workingData %>%
  select(attack_angle_contact_x, 
         torso_angle_fp_y, 
         pelvis_angular_velocity_fp_x, 
         torso_angular_velocity_fp_x, 
         torso_launchpos_y, 
         torso_angle_fp_y, 
         torso_angle_fp_z, 
         exit_velo_mph_x.x) %>%
  drop_na()  

correlationMatrixOne <- cor(data_clean, method = "spearman")



# Extract correlations with exit velocity and attack angle
exit_velocity_corr <- correlationMatrixOne[,"exit_velo_mph_x.x"]
attack_angle_corr <- correlationMatrixOne[,"attack_angle_contact_x"]

# Sort correlations for better readability
sorted_exit_velocity_corr <- sort(exit_velocity_corr, decreasing = TRUE)
sorted_attack_angle_corr <- sort(attack_angle_corr, decreasing = TRUE)

# Print sorted correlations
cat("Correlations with Exit Velocity:\n")
print(sorted_exit_velocity_corr)

cat("\nCorrelations with Attack Angle:\n")
print(sorted_attack_angle_corr)


#No clear correlations for biomech variables, looking at interactions that may be useful
data_interactions <- workingData %>%
  mutate(
    torso_pelvis_interaction = torso_angle_fp_y * pelvis_angular_velocity_fp_x, #torso position and speed of pevlis
    torso_launch_pelvis_angle = torso_launchpos_y * torso_angle_fp_y, # launch position and the angle at foot plant
    angular_velocity_interaction = pelvis_angular_velocity_fp_x * torso_angular_velocity_fp_x, # combined velocities
    torso_angle_ratio = torso_angle_fp_y / torso_angle_fp_z, # y is horizontal lean, z is vertical 
    multi_torso_interaction = torso_angle_fp_y * torso_angle_fp_x * torso_launchpos_y #?
  )

#Remove outliers for torso launch pelvis angle
# Calculate IQR for torso_launch_pelvis_angle
Q1 <- quantile(data_interactions$torso_launch_pelvis_angle, 0.25, na.rm = TRUE)
Q3 <- quantile(data_interactions$torso_launch_pelvis_angle, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define the bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out outliers
data_interactions <- data_interactions %>%
  filter(torso_launch_pelvis_angle >= lower_bound & torso_launch_pelvis_angle <= upper_bound)

#Create the first linear model
model_exit_velocity <- lm(exit_velo_mph_x.x ~ attack_angle_contact_x + 
                            torso_angle_fp_y + pelvis_angular_velocity_fp_x + 
                            torso_angular_velocity_fp_x + torso_launchpos_y + 
                            torso_angle_fp_y + torso_angle_fp_z + 
                            torso_pelvis_interaction + torso_launch_pelvis_angle + 
                            angular_velocity_interaction + torso_angle_ratio + 
                            multi_torso_interaction,
                          data = data_interactions)
summary(model_exit_velocity)
par(mfrow = c(2, 2))
plot(model_exit_velocity)
#Rsquared = .181

model_attack_angle  <- lm( attack_angle_contact_x ~ exit_velo_mph_x.x + 
                            torso_angle_fp_y + pelvis_angular_velocity_fp_x + 
                            torso_angular_velocity_fp_x + torso_launchpos_y + 
                            torso_angle_fp_y + torso_angle_fp_z + 
                            torso_pelvis_interaction + torso_launch_pelvis_angle + 
                            angular_velocity_interaction + torso_angle_ratio + 
                            multi_torso_interaction,
                          data = data_interactions)
summary(model_attack_angle)
plot(model_attack_angle)
#bad
library(lme4)
#using mixed linear modeling
meeModelOne <- lmer(exit_velo_mph_x.x ~ attack_angle_contact_x + torso_angle_fp_y + 
                (1 | session.x), data = data_interactions)

summary(meeModelOne)
plot(meeModelOne)
#install.packages("MuMIn")
library(MuMIn)

r.squared <- r.squaredGLMM(model)
print(r.squared)
#better rsquared 
# attack angle model
meeModelTwo <- lmer(attack_angle_contact_x ~ torso_angle_fp_y + exit_velo_mph_x.x + (1 | session.x), data = data_interactions)

summary(meeModelTwo)
r_squared_one <- r.squaredGLMM(meeModelOne)
r_squared_two <- r.squaredGLMM(meeModelTwo)

# print R-squared values
print(r_squared_one)
print(r_squared_two)
AIC(meeModelOne, meeModelTwo)

par(mfrow = c(1, 2))
plot(meeModelOne)
plot(meeModelTwo)


#third model looking at exit velocity
meeModelThree <- lmer(exit_velo_mph_x.x ~ attack_angle_category + torso_launch_pelvis_angle + 
                      (1 | session.x), data = data_interactions)

summary(meeModelThree)
plot(meeModelThree)
r_squared_three <- r.squaredGLMM(meeModelThree)
print(r_squared_three)
#Use torso angle 


#More refined exit velocity model
meeModelFour <- lmer(exit_velo_mph_x.x ~ bat_speed_mph_max_x.x+ torso_launch_pelvis_angle + 
                        (1 | session.x), data = data_interactions)

summary(meeModelFour)
plot(meeModelFour)
r_squared_four <- r.squaredGLMM(meeModelFour)
print(r_squared_four)


# Create a prediction grid to plot model
bat_speed_values <- seq(min(data_interactions$bat_speed_mph_max_x.x, na.rm = TRUE),
                        max(data_interactions$bat_speed_mph_max_x.x, na.rm = TRUE),
                        by = 1)

# Use a constant value for torso_launch_pelvis_angle
constant_torso_launch_pelvis_angle <- mean(data_interactions$torso_launch_pelvis_angle, na.rm = TRUE)

# Create a data frame with all combinations
pred_grid2 <- expand.grid(
  bat_speed_mph_max_x.x = bat_speed_values,
  torso_launch_pelvis_angle = constant_torso_launch_pelvis_angle
)

# Predict exit velocities using the fitted model
pred_grid2$predicted_exit_velocity <- predict(meeModelFour, newdata = pred_grid2, re.form = NA)


# Plot the predicted exit velocities based on the model 4
ggplot(pred_grid2, aes(x = bat_speed_mph_max_x.x, y = predicted_exit_velocity)) +
  geom_line(color = "blue", linewidth = 1, alpha = 0.8) +  # Line for predictions
  labs(title = "Predicted Exit Velocity Based on Bat Speed and Torso Launch Angle",
       x = "Bat Speed (mph)",
       y = "Predicted Exit Velocity (mph)") +
  theme_minimal()


#EXPLORE NONlinear relationship
# Fit a model with a polynomial term
meeModelPoly <- lmer(exit_velo_mph_x.x ~ poly(bat_speed_mph_max_x.x, 2) +  (1 | session.x), data = data_interactions)

# Predict with polynomial model
pred_grid2$predicted_exit_velocity_poly <- predict(meeModelPoly, newdata = pred_grid2, re.form = NA)

# Plot predictions with polynomial fit
ggplot(pred_grid2, aes(x = bat_speed_mph_max_x.x)) +
  geom_line(aes(y = predicted_exit_velocity_poly), color = "red", linewidth = 1, alpha = 0.8) +
  labs(title = "Predicted Exit Velocity with Polynomial Fit",
       x = "Bat Speed (mph)",
       y = "Predicted Exit Velocity (mph)") +
  theme_minimal()

summary(meeModelPoly)

#Find bat speed as an output - model 5
meeModelFive <- lmer(bat_speed_mph_max_x.x ~  attack_angle_category * x_factor_fp_z + torso_launch_pelvis_angle + 
                       (1 | session.x), data = data_interactions)

summary(meeModelFive)


constant_torso_launch_pelvis_angle <- mean(data_interactions$torso_launch_pelvis_angle, na.rm = TRUE)  # or any representative value

# Create a data frame with all combinations of attack angle categories and x_factor values
attack_angles <- unique(data_interactions$attack_angle_category)
pred_grid <- expand.grid(
  attack_angle_category = attack_angles,
  x_factor_fp_z = x_factor_values,
  torso_launch_pelvis_angle = constant_torso_launch_pelvis_angle  # Add this constant
)

# Predict bat speeds using the fitted model
pred_grid$predicted_bat_speed <- predict(meeModelFive, newdata = pred_grid, re.form = NA)


ggplot(pred_grid, aes(x = x_factor_fp_z, y = predicted_bat_speed, color = attack_angle_category)) +
  geom_line(linewidth = 1, alpha = 0.8) +  # Lines for each attack angle category
  geom_point(size = 2, alpha = 0.7) +      # Points for individual predictions
  labs(title = "Interaction between X-Factor Z, Attack Angle and Torso Position on Predicted Bat Speed",
       x = "X-Factor Z",
       y = "Predicted Bat Speed (mph)") +
  theme_minimal() +
  scale_color_manual(values = c("Steep" = "blue", "Upward" = "orange", "Flat" = "green")) +  # Customize colors if desired
  theme(legend.title = element_blank())  # Remove legend title for clarity

plot(meeModelFive)
r_squared_five <- r.squaredGLMM(meeModelFive)
print(r_squared_five)


#relationship between exit velocity and hip shoulder seperartion - exploratory, doesnt show much
ggplot(data_interactions, aes(x = x_factor_fp_z, y = exit_velo_mph_x.x)) +
  geom_point(alpha = 0.6, color = "blue") +  # Adjust color as desired
  labs(title = "Scatter Plot of X-Factor Z vs Exit Velocity",
       x = "X-Factor Z",
       y = "Exit Velocity (mph)") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)


ggplot(data_interactions, aes(x = torso_angle_fp_z, y = exit_velo_mph_x.x)) +
  facet_wrap(~ hitter_side) + 
  geom_point(alpha = 0.6, color = "blue") +  # Adjust color as desired
  labs(title = "Scatter Plot of Torso Angle vs Exit Velocity",
       x = "Torso Angle",
       y = "Exit Velocity (mph)") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)

#new model with spine angle
# Define model 6 with hitting side included as an interaction term
meeModelSix <- lmer(bat_speed_mph_max_x.x ~ attack_angle_category * torso_angle_fp_z * pelvis_angle_fp_z *
                      hitter_side +
                      (1 | session.x), data = data_interactions)

summary(meeModelSix)

#attack angle and hitter side are factors
# data_interactions$attack_angle_category <- as.factor(data_interactions$attack_angle_category)
# data_interactions$hitter_side <- as.factor(data_interactions$hitter_side)
# 
# # Define a prediction grid that includes the range of torso_angle_fp_z values, attack angles, and hitting side
# pred_grid_handedness <- expand.grid(
#   attack_angle_category = levels(data_interactions$attack_angle_category),
#   torso_angle_fp_z = seq(min(data_interactions$torso_angle_fp_z, na.rm = TRUE),
#                          max(data_interactions$torso_angle_fp_z, na.rm = TRUE),
#                          length.out = 100),  # 100 values across the range for smooth lines
#   hitter_side = levels(data_interactions$hitter_side)  # Ensure these match exactly
# )
# 
# # Convert to factors
# pred_grid_handedness$attack_angle_category <- factor(pred_grid_handedness$attack_angle_category,
#                                                      levels = levels(data_interactions$attack_angle_category))
# pred_grid_handedness$hitter_side <- factor(pred_grid_handedness$hitter_side,
#                                            levels = levels(data_interactions$hitter_side))
# 
# # Predict bat speeds using the fitted model 
# pred_grid_handedness$predicted_bat_speed <- predict(meeModelSix, newdata = pred_grid_handedness, re.form = NA)
# 
# Ensure `attack_angle_contact_x` is available in `data_interactions`
data_interactions$attack_angle_contact_x <- as.numeric(data_interactions$attack_angle_contact_x)
data_interactions$attack_angle_category <- as.factor(data_interactions$attack_angle_category)
data_interactions$hitter_side <- as.factor(data_interactions$hitter_side)

# Calculate mean and standard deviation of attack_angle_contact_x
mean_attack_angle <- mean(data_interactions$attack_angle_contact_x, na.rm = TRUE)
sd_attack_angle <- sd(data_interactions$attack_angle_contact_x, na.rm = TRUE)

# Define the threshold (e.g., 3 standard deviations from the mean)
threshold <- 3

# Filter out outliers based on the threshold
data_interactions <- data_interactions %>%
  filter(abs(attack_angle_contact_x - mean_attack_angle) <= threshold * sd_attack_angle)

pred_grid_handedness <- expand.grid(
  attack_angle_category = levels(data_interactions$attack_angle_category),
  torso_angle_fp_z = seq(min(data_interactions$torso_angle_fp_z, na.rm = TRUE),
                         max(data_interactions$torso_angle_fp_z, na.rm = TRUE),
                         length.out = 100),  # 100 values across the range for smooth lines
  hitter_side = levels(data_interactions$hitter_side)  # Ensure these match exactly
)

# Convert relevant columns to factors to match the model structure
pred_grid_handedness$attack_angle_category <- factor(pred_grid_handedness$attack_angle_category,
                                                     levels = levels(data_interactions$attack_angle_category))
pred_grid_handedness$hitter_side <- factor(pred_grid_handedness$hitter_side,
                                           levels = levels(data_interactions$hitter_side))

# Predict bat speeds using the fitted model 
pred_grid_handedness$predicted_bat_speed <- predict(meeModelSix, newdata = pred_grid_handedness, re.form = NA)

# Plotting with attack_angle_contact_x as a continuous variable
# ggplot(pred_grid_handedness, aes(x = torso_angle_fp_z, y = predicted_bat_speed, color = attack_angle_contact_x)) +
#   geom_line(linewidth = 1, alpha = 0.8) +  # Lines representing the attack angle spectrum
#   facet_wrap(~ hitter_side) +              # Separate plots by handedness
#   labs(title = "Interaction between Torso Angle at Footplant, Attack Angle Contact, and Handedness on Predicted Bat Speed",
#        x = "Torso Angle FP Z",
#        y = "Predicted Bat Speed (mph)") +
#   theme_minimal() +
#   scale_color_gradientn(colors = c("blue", "green", "orange")) +  # Gradient for continuous color spectrum
#   theme(legend.title = element_blank())  # Remove legend title for clarity

# plot continious predictions for model 6
ggplot(pred_grid_handedness, aes(x = torso_angle_fp_z, y = predicted_bat_speed, color = attack_angle_category)) +
  geom_line(linewidth = 1, alpha = 0.8) +  # Lines for each attack angle category
  facet_wrap(~ hitter_side) +               # Separate plots by handedness
  labs(title = "Interaction between Torso Angle at footplant, Attack Angle, and Handedness on Predicted Bat Speed",
       x = "Torso Angle FP Z",
       y = "Predicted Bat Speed (mph)") +
  theme_minimal() +
  scale_color_manual(values = c("Steep" = "blue", "Upward" = "orange", "Flat" = "green")) +  # Customize colors if desired
  theme(legend.title = element_blank())  # Remove legend title for clarity




# plot diagnostics for meeModelSix
plot(meeModelSix)
r_squared_six <- r.squaredGLMM(meeModelSix)
print(r_squared_six)


descriptive_stats <- data_interactions %>%
  group_by(hitter_side) %>%
  summarise(
    mean_bat_speed = mean(bat_speed_mph_max_x.x, na.rm = TRUE),
    sd_bat_speed = sd(bat_speed_mph_max_x.x, na.rm = TRUE),
    count = n()
  )

print(descriptive_stats)

# T-test to compare bat speeds between hitter sides
t_test_result <- t.test(bat_speed_mph_max_x.x ~ hitter_side, data = data_interactions)

print(t_test_result)

#exploratory
ggplot(data_interactions, aes(x = torso_angle_fp_z, y = blast_bat_speed_mph_x.x)) +
  facet_wrap(~ hitter_side) + 
  geom_point(alpha = 0.6, color = "blue") +  # Adjust color as desired
  labs(title = "Scatter Plot of Torso Angle vs Bat Speed",
       x = "Torso Angle",
       y = "Exit Velocity (mph)") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)

ggplot(data_interactions, aes(x = pelvis_angle_fp_z, y = blast_bat_speed_mph_x.x)) +
  facet_wrap(~ hitter_side) + 
  geom_point(alpha = 0.6, color = "blue") +  # Adjust color as desired
  labs(title = "Scatter Plot of Pelvis Angle vs Bat Speed",
       x = "Torso Angle",
       y = "Exit Velocity (mph)") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)

ggplot(data_interactions, aes(x = pelvis_angle_fp_z, y = torso_angle_fp_z)) +
  geom_point()





