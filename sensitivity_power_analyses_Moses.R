library(tidyverse)
library(simr)
library(effectsize)
library(glmmTMB)

#IMPORTANT: Please note that in the code and preregistration, "study 3" refers to an additional comparison experiment that is not reported in the paper,
#while "study 4" refers to the two-block experiment, which is referred to as "experiment 3" in the paper.

# save load function
save_load <- function(object, mode) {
  data_folder <- "./Power_analyses/"  # Specify the data folder path relative to your working directory
  
  if (mode == "save") {
    # Save the object as an .rds file with the object name in the specified folder
    file_name <- paste0(data_folder, deparse(substitute(object)), ".rds")
    saveRDS(object, file_name)
    cat("Object saved as", file_name, "\n")
  } else if (mode == "load") {
    # Load the object from the .rds file with the object name in the specified folder
    file_name <- paste0(data_folder, deparse(substitute(object)), ".rds")
    if (file.exists(file_name)) {
      object <- readRDS(file_name)
      cat("Object loaded from", file_name, "\n")
      return(object)
    } else {
      cat("Error: File", file_name, "does not exist.\n")
    }
  } else {
    cat("Error: Mode must be 'save' or 'load'.\n")
  }
}

#### ACCURACY

# Study 1 - n = 100

model = save_load(model_accu_S1_pwr, "load")
temp = save_load(temp_S1_pwr, "load")

# we set sum coding
contrasts(temp$conflict) = contr.sum(levels(temp$conflict))
contrasts(temp$response_stage) = contr.sum(levels(temp$response_stage))

# Create a vector of effect size in log odds ranging from Cohen's d = -0.20 to -0.50 (by 0.05 steps)
sequence <- seq(0.2, 0.5, by = 0.05)
# Apply the function to each element in the sequence
eff_size<- sapply(sequence, d_to_logoddsratio)

# Create an empty list to store results
power_analyses_n_200 <- list()

# Loop over the eff_size vector
for (i in seq_along(eff_size)) {

  # Set the current effect size value
  current_effect_size <- eff_size[i]

  # Update the fixed effect size in the model
  fixef(model)["conflict1"] <- current_effect_size

  # Compute power simulation
  pwr_sim_result <- powerSim(model,
                             test = fixed("conflict1", method = "z"),
                             seed = 123,
                             nsim = 1000,
                             progress = TRUE,
                             alpha = 0.045)

  # Store the entire result object in the list
  power_analyses_n_100[[i]] <- list(effect_size = current_effect_size,
                            pwr_sim_result = pwr_sim_result)
}

save_load(power_analyses_n_100, "save")
power_analyses_n_100 = save_load(power_analyses_n_100, "load")


# Create an empty data frame to store the results
result_df_n100 <- data.frame(
  Cohen_d = numeric(length(power_analyses_n_100)),
  Power = numeric(length(power_analyses_n_100)),
  Lower_CI = numeric(length(power_analyses_n_100)),
  Upper_CI = numeric(length(power_analyses_n_100))
)

# Loop over the full list length
for (i in seq_along(power_analyses_n_100)) {
  
  # Access the current result
  current_result <- power_analyses_n_100[[i]]
  
  # Access specific information within the current result
  effect_size <- logoddsratio_to_d(current_result$effect_size)
  
  output = capture.output(print(current_result$pwr_sim_result))
  
  # Extract power, lower CI, and upper CI from the character output
  power_ci <- str_match(output[2], "(\\d+\\.\\d+)% \\((\\d+\\.\\d+), (\\d+\\.\\d+)\\)")
  

  # Assign values to the data frame
  result_df_n100[i, "Cohen_d"] <- effect_size
  result_df_n100[i, "Power"] <- as.numeric(power_ci[2])
  result_df_n100[i, "Lower_CI"] <- as.numeric(power_ci[3])
  result_df_n100[i, "Upper_CI"] <- as.numeric(power_ci[4])

}

# we plot the curve
power_n_100_plot = ggplot(result_df_n100) +
  geom_line(mapping = aes(x = Cohen_d, y = Power, group = 1), color = "red") +
  geom_ribbon(mapping=aes(x = Cohen_d, ymin= Lower_CI, ymax=Upper_CI), alpha = 0.2) +
  labs(x = "Cohen's d", y = "Power (%)") +
  scale_x_continuous(
    breaks = seq(0.2, 0.5, 0.05),
    limits = c(0.2, 0.5),
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black"),
        strip.text.x = element_text(size = 10), axis.title.x = element_text(size = 10)) +
  theme_minimal()

# save the plot as a svg file
ggsave("./Output/power_analysis_n_100.svg", power_n_100_plot,
       width = 6, height = 6, units = "in", dpi = 600)

# STUDY 4 - N = 200
model = save_load(model_accu_C_S4_pwr, "load")
temp = save_load(temp_S4_pwr, "load")

# Create a vector of effect size in log odds ranging from Cohen's d = -0.10 to -0.50 (by 0.05 steps)
sequence <- seq(0.05, 0.4, by = 0.05)
# Apply the function to each element in the sequence
eff_size <- sapply(sequence, d_to_logoddsratio)

# Create an empty list to store results
power_analyses_n_200 <- list()

# Loop over the eff_size vector
for (i in seq_along(eff_size)) {

  # Set the current effect size value
  current_effect_size <- eff_size[i]

  # Update the fixed effect size in the model
  fixef(model)["impostor1:response_block1"] <- current_effect_size

  # Compute power simulation
  pwr_sim_result <- powerSim(model,
                             test = fixed("impostor1:response_block1", method = "z"),
                             seed = 123,
                             nsim = 1000,
                             progress = TRUE,
                             alpha = 0.045)

  # Store the entire result object in the list
  power_analyses_n_200[[i]] <- list(effect_size = current_effect_size,
                            pwr_sim_result = pwr_sim_result)

  # Store the result for the current iteration with a different name each time
  last_iter_n_200 = list(effect_size = current_effect_size, pwr_sim_result = pwr_sim_result)

  # Save the result at each iteration using the save_load function
  save_load(last_iter_n_200, "save")
}

save_load(power_analyses_n_200, "save")
power_analyses_n_200 = save_load(power_analyses_n_200, "load")


# Create an empty data frame to store the results
result_df_n_200 <- data.frame(
  Cohen_d = numeric(length(power_analyses_n_200)),
  Power = numeric(length(power_analyses_n_200)),
  Lower_CI = numeric(length(power_analyses_n_200)),
  Upper_CI = numeric(length(power_analyses_n_200))
)

# Loop over the full list length
for (i in seq_along(power_analyses_n_200)) {
  
  # Access the current result
  current_result <- power_analyses_n_200[[i]]
  
  # Access specific information within the current result
  effect_size <- logoddsratio_to_d(current_result$effect_size)
  
  output = capture.output(print(current_result$pwr_sim_result))
  
  # Extract power, lower CI, and upper CI from the character output
  power_ci <- str_match(output[2], "(\\d+\\.\\d+)% \\((\\d+\\.\\d+), (\\d+\\.\\d+)\\)")
  
  
  # Assign values to the data frame
  result_df_n_200[i, "Cohen_d"] <- effect_size
  result_df_n_200[i, "Power"] <- as.numeric(power_ci[2])
  result_df_n_200[i, "Lower_CI"] <- as.numeric(power_ci[3])
  result_df_n_200[i, "Upper_CI"] <- as.numeric(power_ci[4])
  
}

# we plot the curve
power_n_200_plot = ggplot(result_df_n_200) +
  geom_line(mapping = aes(x = Cohen_d, y = Power, group = 1), color = "red") +
  geom_ribbon(mapping=aes(x = Cohen_d, ymin= Lower_CI, ymax=Upper_CI), alpha = 0.2) +
  labs(x = "Cohen's d", y = "Power (%)") +
  scale_x_continuous(
    breaks = seq(0.05, 0.4, 0.05),
    limits = c(0.05, 0.4)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black"),
        strip.text.x = element_text(size = 10), axis.title.x = element_text(size = 10)) +
  theme_minimal()


# save the plot as a svg file
ggsave("./Output/power_analysis_n_200.svg", power_n_200_plot,
       width = 6, height = 6, units = "in", dpi = 600)


#### CONFIDENCE

# STUDY 1 - N = 100

# we load the df and the model for the confidence in S1
temp_S1_conf = save_load(temp_conf_S1_conf, "load") %>% 
  mutate(biased = factor(biased),
         item_number = factor(item_number))

CD_S1_beta_model_1 = save_load(CD_S1_beta_model_1, "load")
# unable to simulate from the betaregression mixed effect model
# so we use a linear mixed effects model on confidence

model = lmer(conf ~ 1 + biased + (1 | item_number), data = temp_S1_conf,
             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


# Create a vector of effect size in confidence % points ranging from 0 to 12
# with steps of one
eff_size <- seq(1, 8, by = 1)

# Create an empty list to store results
power_analyses_conf_100 <- list()

# Loop over the eff_size vector
for (i in seq_along(eff_size)) {

  # Set the current effect size value
  current_effect_size <- eff_size[i]

  # Update the fixed effect size in the model
  fixef(model)["biased1"] <- current_effect_size

  # Compute power simulation
  pwr_sim_result <- powerSim(model,
                             test = fixed("biased1", method = "z"),
                             seed = 123,
                             nsim = 1000,
                             progress = TRUE,
                             alpha = 0.045)

  # Store the entire result object in the list
  power_analyses_conf_100[[i]] <- list(effect_size = current_effect_size,
                            pwr_sim_result = pwr_sim_result)
}

save_load(power_analyses_conf_100, "save")
power_analyses_conf_100 = save_load(power_analyses_conf_100, "load")


# Create an empty data frame to store the results
result_df_conf_n100 <- data.frame(
  Effect= numeric(length(power_analyses_conf_100)),
  Power = numeric(length(power_analyses_conf_100)),
  Lower_CI = numeric(length(power_analyses_conf_100)),
  Upper_CI = numeric(length(power_analyses_conf_100))
)

# Loop over the full list length
for (i in seq_along(power_analyses_conf_100)) {

  # Access the current result
  current_result <- power_analyses_conf_100[[i]]

  # Access specific information within the current result
  effect_size <- current_result$effect_size

  output = capture.output(print(current_result$pwr_sim_result))

  # Extract power, lower CI, and upper CI from the character output
  power_ci <- str_match(output[2], "\\b(\\d+\\.\\d+)%\\s*\\(\\s*(\\d+\\.\\d+),\\s*(\\d+\\.\\d+)\\)")

  # Assign values to the data frame
  result_df_conf_n100[i, "Effect"] <- effect_size
  result_df_conf_n100[i, "Power"] <- as.numeric(power_ci[2])
  result_df_conf_n100[i, "Lower_CI"] <- as.numeric(power_ci[3])
  result_df_conf_n100[i, "Upper_CI"] <- as.numeric(power_ci[4])

}

# we plot the curve
power_conf_n_100_plot = ggplot(result_df_conf_n100) +
  geom_line(mapping = aes(x = Effect, y = Power, group = 1), color = "red") +
  geom_ribbon(mapping=aes(x = Effect, ymin= Lower_CI, ymax=Upper_CI), alpha = 0.2) +
  labs(x = "Confidence difference (%)", y = "Power (%)") +
  scale_x_continuous(
    breaks = seq(1, 8, 1),
    limits = c(1, 8),
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black"),
        strip.text.x = element_text(size = 10), axis.title.x = element_text(size = 10)) +
  theme_minimal(); power_conf_n_100_plot

# save the plot as a svg file
ggsave("./Output/power_analysis_conf_n_100.svg", power_conf_n_100_plot,
       width = 6, height = 6, units = "in", dpi = 600)


# STUDY 4 - N = 200

# we load the df and the model for the confidence interaction in S4
temp_S4_conf = save_load(temp_S4_conf, "load")
model_S4_conf_fast_1 = save_load(model_S4_conf_fast_1, "load")

# unable to simulate from the betaregression mixed effect model
# so we use a linear mixed effects model on confidence
model = lmer(conf ~ 1 + impostor + group + impostor:group + (1 | subject) +
               (1 | item_number), data = temp_S4_conf,
             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


# Create a vector of effect size in confidence % points ranging from 0 to 10
# with steps of one
eff_size <- seq(1, 10, by = 1)

# Create an empty list to store results
power_analyses_conf_200 <- list()

# Loop over the eff_size vector
for (i in seq_along(eff_size)) {

  # Set the current effect size value
  current_effect_size <- eff_size[i]

  # Update the fixed effect size in the model
  fixef(model)["impostorweak:group2"] <- current_effect_size

  # Compute power simulation
  pwr_sim_result <- powerSim(model,
                             test = fixed("impostorweak:group2", method = "z"),
                             seed = 123,
                             nsim = 1000,
                             progress = TRUE,
                             alpha = 0.045)

  # Store the entire result object in the list
  power_analyses_conf_200[[i]] <- list(effect_size = current_effect_size,
                                       pwr_sim_result = pwr_sim_result)
}

save_load(power_analyses_conf_200, "save")
power_analyses_conf_200 = save_load(power_analyses_conf_200, "load")


# Create an empty data frame to store the results
result_df_conf_n200 <- data.frame(
  Effect= numeric(length(power_analyses_conf_200)),
  Power = numeric(length(power_analyses_conf_200)),
  Lower_CI = numeric(length(power_analyses_conf_200)),
  Upper_CI = numeric(length(power_analyses_conf_200))
)


# Loop over the full list length
for (i in seq_along(power_analyses_conf_200)) {
  
  # Access the current result
  current_result <- power_analyses_conf_200[[i]]
  
  # Access specific information within the current result
  effect_size <- current_result$effect_size

  output = capture.output(print(current_result$pwr_sim_result))
  
  # Extract power, lower CI, and upper CI from the character output
  power_ci <- str_match(output[2], "\\b(\\d+\\.\\d+)%\\s*\\(\\s*(\\d+\\.\\d+),\\s*(\\d+\\.\\d+)\\)")
  
  # Assign values to the data frame
  result_df_conf_n200[i, "Effect"] <- effect_size
  result_df_conf_n200[i, "Power"] <- as.numeric(power_ci[2])
  result_df_conf_n200[i, "Lower_CI"] <- as.numeric(power_ci[3])
  result_df_conf_n200[i, "Upper_CI"] <- as.numeric(power_ci[4])
  
}

# we plot the curve
power_conf_n_200_plot = ggplot(result_df_conf_n200) +
  geom_line(mapping = aes(x = Effect, y = Power, group = 1), color = "red") +
  geom_ribbon(mapping=aes(x = Effect, ymin= Lower_CI, ymax=Upper_CI), alpha = 0.2) +
  labs(x = "Confidence difference (%)", y = "Power (%)") +
  scale_x_continuous(
    breaks = seq(1, 19, 1),
    limits = c(1, 10),
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black"),
        strip.text.x = element_text(size = 10), axis.title.x = element_text(size = 10)) +
  theme_minimal(); power_conf_n_200_plot

# save the plot as a svg file
ggsave("./Output/power_analysis_conf_n_200.svg", power_conf_n_200_plot,
       width = 6, height = 6, units = "in", dpi = 600)