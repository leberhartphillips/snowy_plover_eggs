# Model the effect of uncertainty in the BaSTA age estimates

#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

load("data/ceuta_egg_chick_female_data.rds")
load("output/stats_laydate_mod.rds")
load("output/stats_eggv_mod.rds")

#### Bootstrap function ----
# Function to randomly draw an age estimate from the BaSTA posteriors, run the
# age-dependent mixed models, and store the results
BaSTA_est_age_boot <- function(nreps = 1000, df = ceuta_egg_chick_female_data){
  
  ceuta_egg_chick_female_data_skew <- 
    ceuta_egg_chick_female_data %>% 
    mutate(est_age_lower = round(est_age_lower),
           est_age_upper = round(est_age_upper),
           est_age = round(est_age)) %>% 
    dplyr::select(ring, est_age_t, est_age, est_age_lower, est_age_upper, age_first_cap) %>% 
    distinct() %>% 
    arrange(ring, est_age) %>% 
    group_by(ring) %>% 
    slice(1) %>% 
    mutate(upper_skew = est_age - round(est_age_upper),
           lower_skew = est_age - round(est_age_lower))
  
  # storage matrices for the predicted values and model statistics
  mod_eggv_boot_age_dev_fits_storage_matrix <- matrix(numeric(13*nreps), nreps)
  mod_eggv_boot_first_age_fits_storage_matrix <- matrix(numeric(8*nreps), nreps)
  mod_eggv_boot_last_age_fits_storage_matrix <- matrix(numeric(13*nreps), nreps)
  mod_eggv_boot_coef_storage_matrix <- matrix(numeric(9*nreps), nreps)
  
  mod_laydate_boot_age_dev_fits_storage_matrix <- matrix(numeric(13*nreps), nreps)
  mod_laydate_boot_first_age_fits_storage_matrix <- matrix(numeric(8*nreps), nreps)
  mod_laydate_boot_last_age_fits_storage_matrix <- matrix(numeric(13*nreps), nreps)
  mod_laydate_boot_coef_storage_matrix <- matrix(numeric(7*nreps), nreps)

  # the bootstrap for-loop
  for (i in 1:nreps) {
    
    # sample a random age for each individual based on their BaSTA posterior
    # distribution (i.e., draw an age value from a skewed normal distribution 
    # estimated from the mean and 95% confidence limits provided by BaSTA)
    ceuta_egg_chick_female_data_boot <- 
      ceuta_egg_chick_female_data_skew %>% 
      mutate(skew = -(upper_skew - lower_skew)) %>% 
      mutate(rand_age = ceiling(rsnorm(n = 1, 
                                       mean = est_age, 
                                       sd = 2, 
                                       xi = skew))) %>% 
      mutate(rand_age = ifelse(age_first_cap == "J", est_age, 
                               ifelse(rand_age > est_age_upper, est_age_upper,
                                      ifelse(rand_age < 1, est_age_lower, rand_age)))) %>% 
      mutate(rand_diff = rand_age - est_age) %>% 
      dplyr::select(ring, rand_diff) %>% 
      left_join(dplyr::select(ceuta_egg_chick_female_data,
                              ring, ID, year, volume_cm, est_age_t, first_age_t,
                              last_age_t, age_first_cap, first_laydate, 
                              n_years_obs, nest_order, avg_ad_tarsi,
                              laydate_deviation), ., 
                by = "ring") %>% 
      mutate(boot_est_age_t = est_age_t + rand_diff,
             boot_firstage_t = first_age_t + rand_diff,
             boot_lastage_t = last_age_t + rand_diff) %>% 
      arrange(age_first_cap, ring, ID) %>% 
      mutate(boot_est_age_t_deviation = boot_est_age_t - boot_firstage_t)
    
    first_nests_age_data_boot <-
      ceuta_egg_chick_female_data_boot %>% 
      dplyr::select(ring, ID, first_laydate, boot_est_age_t_deviation, year,
                    boot_firstage_t, boot_lastage_t, n_years_obs, avg_ad_tarsi,
                    age_first_cap, nest_order, est_age_t) %>% 
      distinct() %>% 
      dplyr::filter(!is.na(boot_est_age_t_deviation) & 
                      nest_order == 1 &
                      year != "2006")
    
    mod_eggv_poly_age_boot <-
      lmer(volume_cm ~ poly(boot_est_age_t_deviation,2) +
             boot_firstage_t + boot_lastage_t + avg_ad_tarsi + 
             laydate_deviation +
             poly(first_laydate, 2) +
             (1 | ID) + (1 | ring) + (1 | year),
           data = ceuta_egg_chick_female_data_boot)
    
    mod_laydate_poly_age_boot <-
      lmer(first_laydate ~ poly(boot_est_age_t_deviation, 2) + 
             boot_firstage_t + boot_lastage_t + avg_ad_tarsi + age_first_cap +
             (1|ring) + (1|year),
           data = first_nests_age_data_boot)
    
    # extract fitted values
    mod_eggv_boot_age_dev_fits <- 
      as.data.frame(effect("poly(boot_est_age_t_deviation,2)", mod_eggv_poly_age_boot, 
                           xlevels = list(boot_est_age_t_deviation = 0:max(ceuta_egg_chick_female_data$est_age_t_deviation))))
    
    mod_eggv_boot_first_age_fits <- 
      as.data.frame(effect("boot_firstage_t", mod_eggv_poly_age_boot, 
                           xlevels = list(boot_firstage_t = c(min(ceuta_egg_chick_female_data$first_age_t):max(ceuta_egg_chick_female_data$first_age_t)))))
    
    mod_eggv_boot_last_age_fits <- 
      as.data.frame(effect("boot_lastage_t", mod_eggv_poly_age_boot, 
                           xlevels = list(boot_lastage_t = c(min(ceuta_egg_chick_female_data$last_age_t):max(ceuta_egg_chick_female_data$last_age_t)))))
    
    
    mod_laydate_boot_age_dev_fits <- 
      as.data.frame(effect("poly(boot_est_age_t_deviation,2)", mod_laydate_poly_age_boot, 
                           xlevels = list(boot_est_age_t_deviation = 0:max(ceuta_egg_chick_female_data$est_age_t_deviation))))
    
    mod_laydate_boot_first_age_fits <- 
      as.data.frame(effect("boot_firstage_t", mod_laydate_poly_age_boot, 
                           xlevels = list(boot_firstage_t = c(min(ceuta_egg_chick_female_data$first_age_t):max(ceuta_egg_chick_female_data$first_age_t)))))
    
    mod_laydate_boot_last_age_fits <- 
      as.data.frame(effect("boot_lastage_t", mod_laydate_poly_age_boot, 
                           xlevels = list(boot_lastage_t = c(min(ceuta_egg_chick_female_data$last_age_t):max(ceuta_egg_chick_female_data$last_age_t)))))
    
    # store results
    mod_eggv_boot_age_dev_fits_storage_matrix[i, ] <- mod_eggv_boot_age_dev_fits$fit
    mod_eggv_boot_first_age_fits_storage_matrix[i, ] <- mod_eggv_boot_first_age_fits$fit
    mod_eggv_boot_last_age_fits_storage_matrix[i, ] <- mod_eggv_boot_last_age_fits$fit
    
    mod_eggv_boot_coef_storage_matrix[i,1] <- summary(mod_eggv_poly_age_boot)$coefficients[1,1]
    mod_eggv_boot_coef_storage_matrix[i,2] <- summary(mod_eggv_poly_age_boot)$coefficients[2,1]
    mod_eggv_boot_coef_storage_matrix[i,3] <- summary(mod_eggv_poly_age_boot)$coefficients[3,1]
    mod_eggv_boot_coef_storage_matrix[i,4] <- summary(mod_eggv_poly_age_boot)$coefficients[4,1]
    mod_eggv_boot_coef_storage_matrix[i,5] <- summary(mod_eggv_poly_age_boot)$coefficients[5,1]
    mod_eggv_boot_coef_storage_matrix[i,6] <- summary(mod_eggv_poly_age_boot)$coefficients[6,1]
    mod_eggv_boot_coef_storage_matrix[i,7] <- summary(mod_eggv_poly_age_boot)$coefficients[7,1]
    mod_eggv_boot_coef_storage_matrix[i,8] <- summary(mod_eggv_poly_age_boot)$coefficients[8,1]
    mod_eggv_boot_coef_storage_matrix[i,9] <- summary(mod_eggv_poly_age_boot)$coefficients[9,1]
    
    mod_laydate_boot_age_dev_fits_storage_matrix[i, ] <- mod_laydate_boot_age_dev_fits$fit
    mod_laydate_boot_first_age_fits_storage_matrix[i, ] <- mod_laydate_boot_first_age_fits$fit
    mod_laydate_boot_last_age_fits_storage_matrix[i, ] <- mod_laydate_boot_last_age_fits$fit
    
    mod_laydate_boot_coef_storage_matrix[i,1] <- summary(mod_laydate_poly_age_boot)$coefficients[1,1]
    mod_laydate_boot_coef_storage_matrix[i,2] <- summary(mod_laydate_poly_age_boot)$coefficients[2,1]
    mod_laydate_boot_coef_storage_matrix[i,3] <- summary(mod_laydate_poly_age_boot)$coefficients[3,1]
    mod_laydate_boot_coef_storage_matrix[i,4] <- summary(mod_laydate_poly_age_boot)$coefficients[4,1]
    mod_laydate_boot_coef_storage_matrix[i,5] <- summary(mod_laydate_poly_age_boot)$coefficients[5,1]
    mod_laydate_boot_coef_storage_matrix[i,6] <- summary(mod_laydate_poly_age_boot)$coefficients[6,1]
    mod_laydate_boot_coef_storage_matrix[i,7] <- summary(mod_laydate_poly_age_boot)$coefficients[7,1]
    
  }
  
  # save as a list
  results_list <-
    list(mod_eggv_boot_age_dev_fits = mod_eggv_boot_age_dev_fits_storage_matrix,
         mod_eggv_boot_first_age_fits = mod_eggv_boot_first_age_fits_storage_matrix,
         mod_eggv_boot_last_age_fits = mod_eggv_boot_last_age_fits_storage_matrix,
         mod_eggv_boot_coefs = mod_eggv_boot_coef_storage_matrix,
           
         mod_laydate_boot_age_dev_fits = mod_laydate_boot_age_dev_fits_storage_matrix,
         mod_laydate_boot_first_age_fits = mod_laydate_boot_first_age_fits_storage_matrix,
         mod_laydate_boot_last_age_fits = mod_laydate_boot_last_age_fits_storage_matrix,
         mod_laydate_boot_coefs = mod_laydate_boot_coef_storage_matrix)
  
}

set.seed(14)

# run the bootstrap
# est_age_boot_out <- 
#   BaSTA_est_age_boot(nreps = 1000)

save(est_age_boot_out, file = "output/age_estimate_uncertainty_bootstraps.rds")

load("output/age_estimate_uncertainty_bootstraps.rds")

#### Egg volume ----
mod_eggv_poly <-
  lmer(volume_cm ~ poly(est_age_t_deviation, 2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         poly(first_laydate, 2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

# Uncertainty in estimate of first age at breeding on between individual variation in egg volume
eggv_first_age_boot_out_melt <- melt(t(est_age_boot_out[["mod_eggv_boot_first_age_fits"]]))
colnames(eggv_first_age_boot_out_melt) <- c("first_age", "iteration", "volume_cm")

eggv_mod_first_age_fits <- 
  as.data.frame(effect(term = "first_age_t", mod = mod_eggv_poly, 
                       xlevels = list(first_age_t = seq(min(ceuta_egg_chick_female_data$first_age_t, na.rm = TRUE), 
                                                        max(ceuta_egg_chick_female_data$first_age_t, na.rm = TRUE), 1))))

boot_eggv_first_age_plot <- 
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = first_age_t + 1, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = eggv_mod_first_age_fits, 
              aes(x = first_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = eggv_first_age_boot_out_melt,
            aes(x = as.numeric(first_age), y = volume_cm, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = eggv_mod_first_age_fits, aes(x = first_age_t + 1, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")", sep = ""))) +
  xlab("Estimated age at first\nlocal breeding attempt") +
  scale_x_continuous(limits = c(0.5, max(ceuta_egg_chick_female_data$first_age_t) + 1.5), 
                     breaks = c(1:(max(ceuta_egg_chick_female_data$first_age_t) + 1)))
                     
# Uncertainty in estimate of last age at breeding on between individual variation in egg volume
eggv_last_age_boot_out_melt <- melt(t(est_age_boot_out[["mod_eggv_boot_last_age_fits"]]))
colnames(eggv_last_age_boot_out_melt) <- c("last_age", "iteration", "volume_cm")

eggv_mod_last_age_fits <- 
  as.data.frame(effect(term = "last_age_t", mod = mod_eggv_poly, 
                       xlevels = list(last_age_t = seq(min(ceuta_egg_chick_female_data$last_age_t, na.rm = TRUE), 
                                                        max(ceuta_egg_chick_female_data$last_age_t, na.rm = TRUE), 1))))

boot_eggv_last_age_plot <- 
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = last_age_t + 1, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = eggv_mod_last_age_fits, 
              aes(x = last_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = eggv_last_age_boot_out_melt,
            aes(x = as.numeric(last_age), y = volume_cm, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = eggv_mod_last_age_fits, aes(x = last_age_t + 1, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")", sep = ""))) +
  xlab("Estimated age at last\nlocal breeding attempt") +
  scale_x_continuous(limits = c(0.5, max(ceuta_egg_chick_female_data$last_age_t) + 1.5), 
                     breaks = seq(1, (max(ceuta_egg_chick_female_data$last_age_t) + 1), 2))

# Uncertainty in estimate of age on within individual variation in egg volume
eggv_age_dev_boot_out_melt <- melt(t(est_age_boot_out[["mod_eggv_boot_age_dev_fits"]]))
colnames(eggv_age_dev_boot_out_melt) <- c("est_age_t_deviation", "iteration", "volume_cm")

eggv_mod_age_dev_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = mod_eggv_poly, 
                       xlevels = list(est_age_t_deviation = seq(min(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 1))))

boot_eggv_age_dev_plot <- 
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_t_deviation, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = eggv_mod_age_dev_fits, 
              aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = eggv_age_dev_boot_out_melt,
            aes(x = as.numeric(est_age_t_deviation) - 1, y = volume_cm, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = eggv_mod_age_dev_fits, aes(x = est_age_t_deviation, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")", sep = ""))) +
  xlab("Years since first local\nbreeding attempt") +
  scale_x_continuous(limits = c(-0.5, max(ceuta_egg_chick_female_data$est_age_t_deviation) + 0.5), 
                     breaks = seq(0, (max(ceuta_egg_chick_female_data$est_age_t_deviation)), by = 2))

#### Laydate ----
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap, nest_order, est_age_t) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation) & 
                  nest_order == 1 &
                  year != "2006") %>%
  mutate(age_first_cap_plot = ifelse(age_first_cap == "J", 2.2, 0.8))

mod_laydate_poly <-
  lmer(first_laydate ~ poly(est_age_t_deviation, 2) + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = first_nests_age_data)

# Uncertainty in estimate of first age at breeding on between individual variation in laydate
laydate_first_age_boot_out_melt <- melt(t(est_age_boot_out[["mod_laydate_boot_first_age_fits"]]))
colnames(laydate_first_age_boot_out_melt) <- c("first_age", "iteration", "first_laydate")

laydate_mod_first_age_fits <- 
  as.data.frame(effect(term = "first_age_t", mod = mod_laydate_poly, 
                       xlevels = list(first_age_t = seq(min(ceuta_egg_chick_female_data$first_age_t, na.rm = TRUE), 
                                                        max(ceuta_egg_chick_female_data$first_age_t, na.rm = TRUE), 1))))

boot_laydate_first_age_plot <-
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = first_age_t + 1, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = laydate_mod_first_age_fits, 
              aes(x = first_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = laydate_first_age_boot_out_melt,
            aes(x = as.numeric(first_age), y = first_laydate, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = laydate_mod_first_age_fits, aes(x = first_age_t + 1, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Standardized lay date of first nest") +
  xlab("Estimated age at first\nlocal breeding attempt") +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_continuous(limits = c(0.5, max(ceuta_egg_chick_female_data$first_age_t) + 1.5), 
                     breaks = c(1:(max(ceuta_egg_chick_female_data$first_age_t) + 1)))

# Uncertainty in estimate of last age at breeding on between individual variation in laydate
laydate_last_age_boot_out_melt <- melt(t(est_age_boot_out[["mod_laydate_boot_last_age_fits"]]))
colnames(laydate_last_age_boot_out_melt) <- c("last_age", "iteration", "first_laydate")

laydate_mod_last_age_fits <- 
  as.data.frame(effect(term = "last_age_t", mod = mod_laydate_poly, 
                       xlevels = list(last_age_t = seq(min(ceuta_egg_chick_female_data$last_age_t, na.rm = TRUE), 
                                                       max(ceuta_egg_chick_female_data$last_age_t, na.rm = TRUE), 1))))

boot_laydate_last_age_plot <- 
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = last_age_t + 1, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = laydate_mod_last_age_fits, 
              aes(x = last_age_t + 1, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = laydate_last_age_boot_out_melt,
            aes(x = as.numeric(last_age), y = first_laydate, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = laydate_mod_last_age_fits, aes(x = last_age_t + 1, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Standardized lay date of first nest") +
  xlab("Estimated age at last\nlocal breeding attempt") +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_continuous(limits = c(0.5, max(ceuta_egg_chick_female_data$last_age_t) + 1.5), 
                     breaks = seq(1, (max(ceuta_egg_chick_female_data$last_age_t) + 1), 2))

# Uncertainty in estimate of age on within individual variation in laydate
laydate_age_dev_boot_out_melt <- melt(t(est_age_boot_out[["mod_laydate_boot_age_dev_fits"]]))
colnames(laydate_age_dev_boot_out_melt) <- c("est_age_t_deviation", "iteration", "first_laydate")

laydate_mod_age_dev_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = mod_laydate_poly, 
                       xlevels = list(est_age_t_deviation = seq(min(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(ceuta_egg_chick_female_data$est_age_t_deviation, na.rm = TRUE), 1))))

boot_laydate_age_dev_plot <- 
  ggplot() +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_t_deviation, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_ribbon(data = laydate_mod_age_dev_fits, 
              aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = laydate_age_dev_boot_out_melt,
            aes(x = as.numeric(est_age_t_deviation) - 1, y = first_laydate, group = iteration),
            lwd = 1, alpha = 0.01, color = "black") +
  geom_line(data = laydate_mod_age_dev_fits, aes(x = est_age_t_deviation, y = fit),
            lwd = 0.5, color = brewer.pal(8, "Set2")[c(6)]) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.ticks.y = element_blank()) +
  ylab("Standardized lay date of first nest") +
  xlab("Years since first local\nbreeding attempt") +
  scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_continuous(limits = c(-0.5, max(ceuta_egg_chick_female_data$est_age_t_deviation) + 0.5), 
                     breaks = seq(0, (max(ceuta_egg_chick_female_data$est_age_t_deviation)), by = 2))

#### Combo plot of uncertainty ----
Est_age_boot_plot <- 
  (boot_eggv_age_dev_plot | boot_eggv_first_age_plot | boot_eggv_last_age_plot) / 
  (boot_laydate_age_dev_plot | boot_laydate_first_age_plot | boot_laydate_last_age_plot) + 
  plot_annotation(tag_levels = 'A')

Est_age_boot_plot

# write plot to disk
ggsave(plot = Est_age_boot_plot,
       filename = "products/figures/svg/Est_age_boot_plot.svg",
       width = 8,
       height = 8, units = "in")

ggsave(plot = Est_age_boot_plot,
       filename = "products/figures/jpg/Est_age_boot_plot.jpg",
       width = 8,
       height = 8, units = "in")