# Model age-dependent variation in lay date

# Product: stats and figure of the lay date ~ senescence model

#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
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

first_nests_age_data %>% 
  # summarise(n_ind = n_distinct(ring),
  #           n_nests = n_distinct(ID)) %>% 
  # group_by(ring, year) %>% 
  summarise(n_nests = n_distinct(ID)) %>% 
  arrange(desc(n_nests))

#### Modeling ----
# Procedure:
# mixed effects regression of laydate ~ senescence with mother ID as random
# effects
mod_laydate_I <-
  lmer(first_laydate ~ est_age_t_deviation + I(est_age_t_deviation^2) + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_laydate <-
  tidy(mod_laydate_I, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_mod_laydate <-
  rpt(first_laydate ~ est_age_t_deviation + I(est_age_t_deviation^2) + 
        first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = filter(first_nests_age_data, year != "2006"),
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 to obtain marginal R2, parameter estimates, and beta weights
mod_laydate_poly <-
  lmer(first_laydate ~ poly(est_age_t_deviation, 2) + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

R2m_mod_laydate <-
  partR2(mod_laydate_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "avg_ad_tarsi",
                      "age_first_cap"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_mod_laydate <-
  partR2(mod_laydate_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "avg_ad_tarsi",
                      "age_first_cap"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_laydate_mod <-
  list(mod_I = mod_laydate_I,
       mod_poly = mod_laydate_poly,
       tidy = tidy_mod_laydate,
       rptR = rpt_mod_laydate,
       partR2m = R2m_mod_laydate,
       partR2c = R2c_mod_laydate)

save(stats_laydate_mod,
     file = "output/Stats_laydate_mod.rds")

load(file = "output/stats_laydate_mod.rds")


plot(allEffects(mod_laydate_p_no_outlier))
random_parameters(mod_laydate_p_no_outlier)
model_parameters(mod_laydate_poly, standardize = "refit")

#### Peak-performance post-hoc analysis ----
# set seed to make simulation reproducible
set.seed(14)

# specify the number of simulations you would like to produce
n_sim = 5000

# create a sim object containing all the components of mod6
mod_laydate_I_sim <- sim(mod_laydate_I, n.sim = n_sim)

# Calculate the traits' value at peak performance and store in table
ypeak_mod <- 
  mod_laydate_I_sim@fixef[, 1] - ((mod_laydate_I_sim@fixef[, 2])^2 / (4 * mod_laydate_I_sim@fixef[, 3]))

coef_ypeak_mod <- 
  c(mean(ypeak_mod), quantile(ypeak_mod, c(0.025,0.975)))

coefYpeakTable <- 
  as.data.frame(cbind(coefName = "Lay date at peak", 
                      as.data.frame(t(as.data.frame(coef_ypeak_mod)))))

# Calculate the age at peak performance and store in table
xpeak_mod <- 
  -(mod_laydate_I_sim@fixef[, 2]) / (2 * mod_laydate_I_sim@fixef[, 3])

coef_xpeak_mod <- 
  c(mean(xpeak_mod), quantile(xpeak_mod, c(0.025, 0.975)))

coefXpeakTable <- 
  as.data.frame(cbind(coefName = "Age at peak", 
                      as.data.frame(t(as.data.frame(coef_xpeak_mod)))))

# Calculate the difference between age 0 and 1 expected from (age+age^2) and 
# Min_Age (if mirrored is their sum).
difAgeMin_mod <- 
  mod_laydate_I_sim@fixef[, 2] + mod_laydate_I_sim@fixef[, 3] + mod_laydate_I_sim@fixef[, 4]

coef_difAgeMin_mod <- 
  c(mean(difAgeMin_mod), quantile(difAgeMin_mod, c(0.025, 0.975)))

coefdifAgeMinTable <- 
  as.data.frame(cbind(coefName = "DifAgeMin", 
                      as.data.frame(t(as.data.frame(coef_difAgeMin_mod)))))

# set peak based on the mean estimate from the previous simulation
first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation < floor(coefXpeakTable$V1)] <- "0"
first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation > floor(coefXpeakTable$V1) - 1] <- "1"

# run peak analysis (i.e., now the quadratic effect is broken up into two
# pieces reflective of the pre- and post-peak sections of the curve)
mod_laydate_I_peak <-
  lmer(first_laydate ~ PrePeak_mod + est_age_t_deviation * PrePeak_mod + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006" & est_age_t_deviation < 9))

# Simulate posterior distribution of peak model estimates
n.sim <- 5000 

mod_laydate_I_peak_sim <- sim(mod_laydate_I_peak, n.sim = n.sim) 

colnames(mod_laydate_I_peak_sim@fixef) <- names(fixef(mod_laydate_I_peak))

# calculate the slope of the pre-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is simply the baseline age 
# slope)
PrePeak_age_effect_sim <- 
  c(mean(mod_laydate_I_peak_sim@fixef[, "est_age_t_deviation"]), 
    quantile(mod_laydate_I_peak_sim@fixef[, "est_age_t_deviation"], prob = c(0.025, 0.975)))

# calculate the slope of the post-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is calculated as the baseline 
# age slope plus the interaction slope)
PostPeak_age_effect_sim <- 
  c(mean(mod_laydate_I_peak_sim@fixef[, "est_age_t_deviation"] + 
           mod_laydate_I_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"]), 
    quantile(mod_laydate_I_peak_sim@fixef[, "est_age_t_deviation"] + 
               mod_laydate_I_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"], 
             prob = c(0.025, 0.975)))

#### remove outlier old individual ----
mod_laydate_I_no_outlier <-
  lmer(first_laydate ~ est_age_t_deviation + I(est_age_t_deviation^2) +
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, est_age_t_deviation < 9))

plot(allEffects(mod_laydate_I_no_outlier))
random_parameters(mod_laydate_I_no_outlier)
model_parameters(mod_laydate_I_no_outlier)

#### Peak-performance post-hoc analysis ----
# set seed to make simulation reproducible
set.seed(14)

# specify the number of simulations you would like to produce
n_sim = 5000

# create a sim object containing all the components of mod6
mod_laydate_I_no_outlier_sim <- 
  sim(mod_laydate_I_no_outlier, n.sim = n_sim)

# Calculate the traits' value at peak performance and store in table
ypeak_mod <- 
  mod_laydate_I_no_outlier_sim@fixef[, 1] - ((mod_laydate_I_no_outlier_sim@fixef[, 2])^2 / (4 * mod_laydate_I_no_outlier_sim@fixef[, 3]))

coef_ypeak_mod <- 
  c(mean(ypeak_mod), quantile(ypeak_mod, c(0.025,0.975)))

coefYpeakTable <- 
  as.data.frame(cbind(coefName = "Lay date at peak", 
                      as.data.frame(t(as.data.frame(coef_ypeak_mod)))))

# Calculate the age at peak performance and store in table
xpeak_mod <- 
  -(mod_laydate_I_no_outlier_sim@fixef[, 2]) / (2 * mod_laydate_I_no_outlier_sim@fixef[, 3])

coef_xpeak_mod <- 
  c(mean(xpeak_mod), quantile(xpeak_mod, c(0.025, 0.975)))

coefXpeakTable_no_outlier <- 
  as.data.frame(cbind(coefName = "Age at peak", 
                      as.data.frame(t(as.data.frame(coef_xpeak_mod)))))

# Calculate the difference between age 0 and 1 expected from (age+age^2) and 
# Min_Age (if mirrored is their sum).
difAgeMin_mod <- 
  mod_laydate_I_no_outlier_sim@fixef[, 2] + mod_laydate_I_no_outlier_sim@fixef[, 3] + mod_laydate_I_no_outlier_sim@fixef[, 4]

coef_difAgeMin_mod <- 
  c(mean(difAgeMin_mod), quantile(difAgeMin_mod, c(0.025, 0.975)))

coefdifAgeMinTable <- 
  as.data.frame(cbind(coefName = "DifAgeMin", 
                      as.data.frame(t(as.data.frame(coef_difAgeMin_mod)))))

# set peak based on the mean estimate from the previous simulation
first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation < round(coefXpeakTable_no_outlier$V1)] <- "0"
first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation > round(coefXpeakTable_no_outlier$V1) - 1] <- "1"

filter(first_nests_age_data, est_age_t_deviation < 9) %>% 
  arrange(desc(est_age_t_deviation))

# run peak analysis (i.e., now the quadratic effect is broken up into two
# pieces reflective of the pre- and post-peak sections of the curve)
mod_laydate_I_no_outlier_peak <-
  lmer(first_laydate ~ PrePeak_mod + est_age_t_deviation * PrePeak_mod + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006" & est_age_t_deviation < 9))

# Simulate posterior distribution of peak model estimates
n.sim <- 5000 

mod_laydate_I_no_outlier_peak_sim <- sim(mod_laydate_I_no_outlier_peak, n.sim = n.sim) 

colnames(mod_laydate_I_no_outlier_peak_sim@fixef) <- names(fixef(mod_laydate_I_no_outlier_peak))

# calculate the slope of the pre-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is simply the baseline age 
# slope)
PrePeak_age_effect_sim <- 
  c(mean(mod_laydate_I_no_outlier_peak_sim@fixef[, "est_age_t_deviation"]), 
    quantile(mod_laydate_I_no_outlier_peak_sim@fixef[, "est_age_t_deviation"], prob = c(0.025, 0.975)))

# calculate the slope of the post-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is calculated as the baseline 
# age slope plus the interaction slope)
PostPeak_age_effect_sim <- 
  c(mean(mod_laydate_I_no_outlier_peak_sim@fixef[, "est_age_t_deviation"] + 
           mod_laydate_I_no_outlier_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"]), 
    quantile(mod_laydate_I_no_outlier_peak_sim@fixef[, "est_age_t_deviation"] + 
               mod_laydate_I_no_outlier_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"], 
             prob = c(0.025, 0.975)))

model_parameters(mod_laydate_I_no_outlier_peak, standardize = "refit")
model_parameters(mod_laydate_I_no_outlier_peak)
summary(mod_laydate_I_no_outlier_peak)


#### Plotting ----
mod_laydate_poly_no_outlier <-
  lmer(first_laydate ~ poly(est_age_t_deviation, 2) +
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, est_age_t_deviation < 9))

laydate_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = mod_laydate_poly_no_outlier, 
                       xlevels = list(est_age_t_deviation = seq(min(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(dplyr::filter(first_nests_age_data, est_age_t_deviation < 9)$est_age_t_deviation, na.rm = TRUE), 1))))
pre_post_laydate_mod_age_fits <- 
  as.data.frame(effect(term = "PrePeak_mod:est_age_t_deviation", mod = mod_laydate_I_no_outlier_peak, 
                       xlevels = list(est_age_t_deviation = seq(min(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(dplyr::filter(first_nests_age_data, est_age_t_deviation < 9)$est_age_t_deviation, na.rm = TRUE), 1),
                                      PrePeak_mod = c("0", "1")))) %>% 
  filter((PrePeak_mod == "0" & est_age_t_deviation < round(coefXpeakTable_no_outlier$V1)) | 
           (PrePeak_mod == "1" & est_age_t_deviation > round(coefXpeakTable_no_outlier$V1) - 2))


laydate_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age_t_deviation, 2)", mod = stats_laydate_mod$mod_poly, 
                       xlevels = list(est_age_t_deviation = seq(min(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 1))))
pre_post_laydate_mod_age_fits <- 
  as.data.frame(effect(term = "PrePeak_mod:est_age_t_deviation", mod = mod_laydate_I_peak, 
                       xlevels = list(est_age_t_deviation = seq(min(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 
                                                                max(first_nests_age_data$est_age_t_deviation, na.rm = TRUE), 1),
                                      PrePeak_mod = c("0", "1")))) %>% 
  filter((PrePeak_mod == "0" & est_age_t_deviation < round(coefXpeakTable$V1)) | 
           (PrePeak_mod == "1" & est_age_t_deviation > round(coefXpeakTable$V1) - 2))

# plot pre and post peak trend
pre_post_date_age_trend_plot <-
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.ticks.y = element_blank()) +
  # geom_jitter(data = filter(first_nests_age_data, est_age_t_deviation < 9), 
  #             alpha = 0.4, width = 0.3,
  #             aes(x = est_age_t_deviation, y = first_laydate),
  #             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age_t_deviation, y = first_laydate),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = laydate_mod_age_fits, aes(x = est_age_t_deviation, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = laydate_mod_age_fits,
              aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  geom_line(data = filter(pre_post_laydate_mod_age_fits, PrePeak_mod == "0"),
            aes(x = est_age_t_deviation, y = fit),
            lwd = 1, alpha = 1, color = "#D95F02") +
  # geom_ribbon(data = filter(pre_post_laydate_mod_age_fits, PrePeak_mod == "0"),
  #             aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
  #             lwd = 1, alpha = 0.25, fill = "#D95F02") +
  # geom_line(data = filter(pre_post_laydate_mod_age_fits, PrePeak_mod == "1"),
  #           aes(x = est_age_t_deviation, y = fit),
  #           lwd = 1, alpha = 1, color = "#1B9E77") +
  # geom_ribbon(data = filter(pre_post_laydate_mod_age_fits, PrePeak_mod == "1"),
  #             aes(x = est_age_t_deviation, ymax = upper, ymin = lower),
  #             lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Years since first local breeding attempt") +
  scale_x_continuous(limits = c(-0.5, 12.5), breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(limits = c(-60, 60)) +
  annotate(geom = "text", y = 55, x = 0,
           label = "First nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# extract fitted values
laydate_mod_rec_fits <- 
  as.data.frame(effect(term = "age_first_cap", mod = stats_laydate_mod$mod_poly, 
                       xlevels = list(age_first_cap = c("A", "J")))) %>%
  mutate(age_first_cap_plot = ifelse(age_first_cap == "J", 1.8, 1.2))

plot_palette_recruit <- brewer.pal(6, "Dark2")[c(2,3)]

date_recruit_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_age_data,
               aes(x = age_first_cap_plot, y = first_laydate,
                   group = age_first_cap, fill = age_first_cap),
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_errorbar(data = laydate_mod_rec_fits, 
                aes(x = age_first_cap_plot, ymax = upper, ymin = lower),
                alpha = 1, color = "black", width = 0.05, lwd = 0.5) + 
  geom_point(data = laydate_mod_rec_fits, 
             aes(x = age_first_cap_plot, y = fit, fill = age_first_cap),
             lwd = 1, shape = 21, color= "black") +
  geom_jitter(data = first_nests_age_data, 
              aes(x = age_first_cap, y = first_laydate, 
                  group = age_first_cap, 
                  fill = age_first_cap, color = age_first_cap), 
              width = 0.02, alpha = 0.2, shape = 19) +
  luke_theme +
  theme(legend.position = "none",
        panel.border = element_blank(),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_text(size = 11)) +
  scale_y_continuous(limits = c(-60, 60)) +
  scale_x_discrete(labels = c("A" = "Immigrant",
                              "J" = "Local\nrecruit")) +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Origin") +
  scale_color_manual(values = rev(plot_palette_recruit)) +
  scale_fill_manual(values = rev(plot_palette_recruit))# +
# annotate(geom = "text", x = 0.5, y = 58,
#          label = "First nests of the season",
#          color = "black", size = 3, fontface = 'italic', hjust = 0)

date_recruit_plot

#### Combo plot of age dynamics ----
Age_plot <-
  (pre_post_date_age_trend_plot | date_recruit_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')),
              widths = unit(c(7, 4), c('cm', 'cm')))
Age_plot
