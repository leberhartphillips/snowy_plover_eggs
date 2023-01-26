# Model seasonal- and age-dependent variation in egg volume

# Product: stats and figure of the chick egg volume ~ senescence + season model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

#### Modeling egg volume ----
# Full model predicting egg volume:
# Fixed effects
# - quadratic age effect (i.e., within-group deviation in age sensu van de Pol and Verhulst 2006)
# - selective appearance and disappearence (i.e., age at first and last observation)
# - female structural size (tarsus)
# - quadratic seasonal effect of first lay date (i.e., between individual effect)
# - linear within seasonal effect (i.e., within-group deviation in laydate)
# Random intercepts: nest, individual, year

# use van der Pol method to obtain the within-group deviation scores for age and
# lay date. The "x_deviation" effect is interpreted as the within individual
# effect, whereas the "first_x" (or "last_x") effect is interpreted as the between 
# individual effect.
mod_eggv_poly <-
  lmer(volume_cm ~ poly(est_age_t_deviation, 2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         poly(first_laydate, 2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

mod_eggv_poly_no_laydate <-
  lmer(volume_cm ~ poly(est_age_t, 2) +
         first_age_t + last_age_t + #avg_ad_tarsi + 
         # laydate_deviation +
         # poly(first_laydate, 2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

mod_eggv_I <-
  lmer(volume_cm ~ est_age_t_deviation + I(est_age_t_deviation^2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         first_laydate + I(first_laydate^2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

mod_eggv_I_no_laydate <-
  lmer(volume_cm ~ est_age_t_deviation + I(est_age_t_deviation^2) +
         first_age_t + last_age_t + #avg_ad_tarsi + 
         # laydate_deviation +
         # first_laydate + I(first_laydate^2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

# run tidy bootstrap to obtain model diagnostics
tidy_mod_eggv <-
  tidy(mod_eggv_I, conf.int = TRUE, conf.method = "boot", nsim = 1000)

tidy_mod_eggv_poly <-
  tidy(mod_eggv_poly, conf.int = TRUE, conf.method = "boot", nsim = 1000)

tidy_mod_eggv_poly_no_laydate <-
  tidy(mod_eggv_poly_no_laydate, conf.int = TRUE, conf.method = "boot", nsim = 1000)

model_parameters(mod_eggv_I)
random_parameters(mod_eggv_I)

# run rptR to obtain repeatabilities of random effects
rpt_mod_eggv <-
  rpt(volume_cm ~ poly(est_age_t_deviation, 2) + first_age_t + last_age_t + 
        avg_ad_tarsi + 
        laydate_deviation + poly(first_laydate, 2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2m_mod_eggv <-
  partR2(mod_eggv_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "poly(first_laydate, 2)",
                      "laydate_deviation",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

R2c_mod_eggv <-
  partR2(mod_eggv_poly,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "poly(first_laydate, 2)",
                      "laydate_deviation",
                      "avg_ad_tarsi"),
         R2_type = "conditional",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

model_parameters(stats_eggv_mod$mod_I, standardize = "refit")
random_parameters(stats_eggv_mod$mod_poly)
plot(allEffects(stats_eggv_mod$mod_I))

#### Modeling egg volume (known-age dataset) ----
# Full model predicting egg volume:
# Fixed effects
# - quadratic age effect (i.e., within-group deviation in age sensu van de Pol and Verhulst 2006)
# - selective appearance and disappearance (i.e., age at first and last observation)
# - female structural size (tarsus)
# - quadratic seasonal effect of first lay date (i.e., between individual effect)
# - linear within seasonal effect (i.e., within-group deviation in laydate)
# Random intercepts: nest, individual, year

# use van der Pol method to obtain the within-group deviation scores for age and
# lay date. The "x_deviation" effect is interpreted as the within individual
# effect, whereas the "first_x" (or "last_x") effect is interpreted as the between 
# individual effect.
# Note: random effect "Year" is removed from this reduced model due to sigularity
mod_eggv_poly_ka <-
  lmer(volume_cm ~ poly(est_age_t_deviation, 2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         poly(first_laydate, 2) +
         (1 | ID) + (1 | ring),
       data = filter(ceuta_egg_chick_female_data, age_first_cap == "J"))

mod_eggv_I_ka <-
  lmer(volume_cm ~ est_age_t_deviation + I(est_age_t_deviation^2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         first_laydate + #I(first_laydate^2) +
         (1 | ID) + (1 | ring),
       data = filter(ceuta_egg_chick_female_data, age_first_cap == "J"))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_eggv_ka <-
  tidy(mod_eggv_I_ka, conf.int = TRUE, conf.method = "boot", nsim = 1000)

model_parameters(mod_eggv_I_ka)
random_parameters(mod_eggv_I_ka)

# run rptR to obtain repeatabilities of random effects
rpt_mod_eggv_ka <-
  rpt(volume_cm ~ poly(est_age_t_deviation, 2) + first_age_t + last_age_t + 
        avg_ad_tarsi + 
        laydate_deviation + poly(first_laydate, 2) +
        (1|ID) + (1|ring),
      grname = c("ID", "ring", "Fixed"),
      data = filter(ceuta_egg_chick_female_data, age_first_cap == "J"),
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2m_mod_eggv_ka <-
  partR2(mod_eggv_poly_ka,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "poly(first_laydate, 2)",
                      "laydate_deviation",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

R2c_mod_eggv_ka <-
  partR2(mod_eggv_poly_ka,
         partvars = c("poly(est_age_t_deviation, 2)",
                      "first_age_t",
                      "last_age_t",
                      "poly(first_laydate, 2)",
                      "laydate_deviation",
                      "avg_ad_tarsi"),
         R2_type = "conditional",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save model, tidy, rptR, and partR2 output as a list (includes both analyses
# for full dataset and reduced dataset of only known-aged individuals)
stats_eggv_mod <-
  list(mod_I = mod_eggv_I,
       mod_poly = mod_eggv_poly,
       tidy = tidy_mod_eggv,
       rptR = rpt_mod_eggv,
       partR2m = R2m_mod_eggv,
       partR2c = R2c_mod_eggv,
       mod_I_ka = mod_eggv_I_ka,
       mod_poly_ka = mod_eggv_poly_ka,
       tidy_ka = tidy_mod_eggv_ka,
       rptR_ka = rpt_mod_eggv_ka,
       partR2m_ka = R2m_mod_eggv_ka,
       partR2c_ka = R2c_mod_eggv_ka)

save(stats_eggv_mod,
     file = "output/Stats_eggv_mod.rds")

load("output/stats_eggv_mod.rds")

model_parameters(stats_eggv_mod$mod_I_ka, standardize = "refit")
model_parameters(stats_eggv_mod$mod_I, standardize = "refit")
random_parameters(stats_eggv_mod$mod_poly)
plot(allEffects(stats_eggv_mod$mod_I))

#### Reviewer comment 6 ----
# Did egg volume differ between females that were polyandrous versus renested, 
# controlling for lay date?

# need to subset dataset to females that had at least 2 nests in a given season,
# then specify the status of the previous nest (i.e., renest or not) and mate 
# status of second attempt (same or different).
rev6_data_rm <- 
ceuta_egg_chick_female_data %>% 
  # 1) subset to only cases in which the first nest failed
  # dplyr::filter(nest_order == 2) %>%
  select(ID, ring, year, nest_1_fate, jul_lay_date_std_num, nest_order, polyandry, avg_ad_tarsi) %>%
  mutate(nest_2_type = ifelse(nest_order == 2 & polyandry == "poly" & (nest_1_fate != "Hatch" & nest_1_fate != "Unknown"), "fail_poly",
                              ifelse(nest_order == 2 & polyandry == "mono" & (nest_1_fate == "Hatch"), "succ_mono", NA))) %>% 
  filter(!is.na(nest_2_type)) %>% 
  arrange(ID) %>% 
  distinct()

rev6_data_rm %>% 
  group_by(nest_2_type) %>% 
  summarise(n_distinct(ID))

rev6_data <-
  ceuta_egg_chick_female_data %>% 
  # 1) subset to only cases in which the first nest failed
  # dplyr::filter(nest_order == 2) %>%
  select(ID, ring, year, nest_1_fate, jul_lay_date_std_num, nest_order, polyandry, volume_cm, avg_ad_tarsi) %>%
  mutate(nest_2_type = ifelse(nest_order == 2 & polyandry == "mono" & (nest_1_fate != "Hatch" & nest_1_fate != "Unknown"), "renest",
                              ifelse(nest_order == 2 & polyandry == "poly", "poly", NA))) %>% 
  filter(!is.na(nest_2_type)) %>% 
  arrange(ID) %>% 
  distinct() %>% 
  filter(ID %!in% rev6_data_rm$ID)

rev6_data %>% 
  # group_by(ring) %>% 
  summarise(n_distinct(year))

mod_eggv_rev6 <-
  lmer(volume_cm ~ avg_ad_tarsi + 
         jul_lay_date_std_num + 
         nest_2_type +
         (1 | ID) + (1 | ring),
       data = rev6_data)

eggv_mod_rev6_fits <- 
  as.data.frame(effect("nest_2_type", mod_eggv_rev6, 
                       xlevels = list(nest_2_type = c("renest", "poly"))))



eggv_mod_rev6_fits %>% 
ggplot() +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper,
                    x = nest_2_type),
                alpha = 1, color = "#2E3440", 
                size = 0.5,
                width = 0) +
  geom_point(aes(y = fit, x = nest_2_type),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = "#2E3440", 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  ylab(expression(paste("Model predictions of egg volume (cm", ''^{3}, ") of second clutch" %+-% "95% CI", sep = ""))) +
  xlab("Female breeding state at \nsecond clutch") +
  ylim(c(6, 9)) +
  scale_x_discrete(labels = c("poly" = "polyandrous",
                              "renest" = "renester"))

ggsave(plot = adult_tsm_plot,
       filename = "products/figures/png/adult_tsm_plot.png",
       width = 3,
       height = 4, units = "in")

mod_eggv_rev6 <-
  lmer(volume_cm ~ avg_ad_tarsi + 
         jul_lay_date_std_num + 
         nest_2_type +
         (1 | ID) + (1 | ring),
       data = rev6_data)

# run tidy bootstrap to obtain model diagnostics
tidy_mod_eggv_rev6 <-
  tidy(mod_eggv_rev6, conf.int = TRUE, conf.method = "boot", nsim = 1000)

model_parameters(mod_eggv_I)
random_parameters(mod_eggv_I)

# run rptR to obtain repeatabilities of random effects
rpt_mod_eggv_rev6 <-
  rpt(volume_cm ~ avg_ad_tarsi + 
        jul_lay_date_std_num + 
        nest_2_type +
        (1 | ID) + (1 | ring),
      grname = c("ID", "ring", "Fixed"),
      data = rev6_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2m_mod_eggv_rev6 <-
  partR2(mod_eggv_rev6,
         partvars = c("nest_2_type",
                      "jul_lay_date_std_num",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

R2c_mod_eggv_rev6 <-
  partR2(mod_eggv_rev6,
         partvars = c("nest_2_type",
                      "jul_lay_date_std_num",
                      "avg_ad_tarsi"),
         R2_type = "conditional",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

stats_eggv_mod_rev6 <-
  list(mod = mod_eggv_rev6,
       tidy = tidy_mod_eggv_rev6,
       rptR = rpt_mod_eggv_rev6,
       partR2m = R2m_mod_eggv_rev6,
       partR2c = R2c_mod_eggv_rev6)

save(stats_eggv_mod_rev6,
     file = "output/Stats_eggv_mod_rev6.rds")

load("output/Stats_eggv_mod_rev6.rds")

#### LRS of polyandrous mating strategy ----
ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

LRS_polyandry <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, year, fate, clutch_size, polyandry) %>% 
  distinct() %>% 
  mutate(clutch_size = as.numeric(str_remove_all(clutch_size, "\\+"))) %>% 
  mutate(chicks = ifelse(fate == "Hatch", clutch_size, 0)) %>% 
  group_by(ring, polyandry) %>% 
  dplyr::summarise(sum_chicks = sum(chicks, na.rm = TRUE),
                   n_years = n_distinct(year, na.rm = TRUE)) %>%
  mutate(polyandry_plot = ifelse(polyandry == "poly", 2.2, 0.8))

LRS_polyandry %>% 
  group_by(polyandry) %>% 
  summarise(mean_chicks = mean(sum_chicks, na.rm = TRUE),
            sd_chicks = sd(sum_chicks, na.rm = TRUE),
            median_chicks = median(sum_chicks, na.rm = TRUE))

LRS_polyandry

mod_LRS_rev10 <-
  lmer(sum_chicks ~ polyandry +
         (1 | ring),
       data = LRS_polyandry)

plot(allEffects(mod_LRS_rev10))

mod_LRS_rev10_fits <- 
  as.data.frame(effect(term = "polyandry", mod = mod_LRS_rev10, 
                       xlevels = list(polyandry = c("mono", "poly")))) %>%
  mutate(polyandry_plot = ifelse(polyandry == "poly", 1.8, 1.2))

LRS_chicks_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = LRS_polyandry,
               aes(x = polyandry_plot, y = sum_chicks,
                   group = polyandry, fill = polyandry),
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_errorbar(data = mod_LRS_rev10_fits, 
                aes(x = polyandry_plot, ymax = upper, ymin = lower),
                alpha = 1, color = "black", width = 0.05, lwd = 0.5) + 
  geom_point(data = mod_LRS_rev10_fits, 
             aes(x = polyandry_plot, y = fit, fill = polyandry),
             lwd = 1, shape = 21, color= "black") +
  geom_jitter(data = LRS_polyandry, 
              aes(x = polyandry, y = sum_chicks, 
                  group = polyandry, 
                  fill = polyandry, color = polyandry), 
              width = 0.02, alpha = 0.2, shape = 19) +
  luke_theme +
  theme(legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_text(size = 11),
        # axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey70", size = 0.1),
        axis.title.x = element_text(size = 11)) +
  # scale_y_continuous(limits = c(-65, 65), breaks = c(-60, -30, 0, 30, 60)) +
  scale_x_discrete(labels = c("mono" = "Monogamous years",
                              "poly" = "Polyandrous years")) +
  ylab(expression(paste("Lifetime number of chicks produced" %+-%  "95% CI", sep = ""))) +
  xlab("Mating behaviour") +
  scale_color_manual(values = rev(plot_palette_polyandry)) +
  scale_fill_manual(values = rev(plot_palette_polyandry))# +
# annotate(geom = "text", x = 0.5, y = 58,
#          label = "First nests of the season",
#          color = "black", size = 3, fontface = 'italic', hjust = 0)

LRS_chicks_plot

LRS_polyandry %>% 
  ungroup() %>% 
  summarise(Year = n_distinct(year),
            Individual = n_distinct(ring),
            Nests = n_distinct(ID),
            Observations = nrow(.))

mod_LRS_rev10_fits[2, 2] - mod_LRS_rev10_fits[1, 2] 
mod_LRS_rev10_fits[2, 4] - mod_LRS_rev10_fits[1, 4] 
mod_LRS_rev10_fits[2, 5] - mod_LRS_rev10_fits[1, 5] 


#### Peak-performance post-hoc analysis ----
# set seed to make simulation reproducible
set.seed(14)

# specify the number of simulations you would like to produce
n_sim = 5000

# create a sim object containing all the components of mod6
mod_eggv_I_sim <- sim(stats_eggv_mod$mod_I, n.sim = n_sim)

# Calculate the traits' value at peak performance and store in table
ypeak_mod <- 
  mod_eggv_I_sim@fixef[, 1] - ((mod_eggv_I_sim@fixef[, 8])^2 / (4 * mod_eggv_I_sim@fixef[, 9]))

coef_ypeak_mod <- 
  c(mean(ypeak_mod), quantile(ypeak_mod, c(0.025,0.975)))

coefYpeakTable <- 
  as.data.frame(cbind(coefName = "Egg volume at peak", 
                      as.data.frame(t(as.data.frame(coef_ypeak_mod)))))

# Calculate the age at peak performance and store in table
xpeak_mod <- 
  -(mod_eggv_I_sim@fixef[, 8]) / (2 * mod_eggv_I_sim@fixef[, 9])

coef_xpeak_mod <- 
  c(mean(xpeak_mod), quantile(xpeak_mod, c(0.025, 0.975)))

coefXpeakTable <- 
  as.data.frame(cbind(coefName = "Date at peak", 
                      as.data.frame(t(as.data.frame(coef_xpeak_mod)))))

# Calculate the difference between age 0 and 1 expected from (age+age^2) and 
# Min_Age (if mirrored is their sum).
difAgeMin_mod <- 
  mod_eggv_I_sim@fixef[, 2] + mod_eggv_I_sim@fixef[, 3] + mod_eggv_I_sim@fixef[, 4]

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
mod_eggv_I_peak <-
  lmer(volume_cm ~ PrePeak_mod + est_age_t_deviation * PrePeak_mod + 
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

# Simulate posterior distribution of peak model estimates
n.sim <- 5000 

mod_eggv_I_peak_sim <- sim(mod_eggv_I_peak, n.sim = n.sim) 

colnames(mod_eggv_I_peak_sim@fixef) <- names(fixef(mod_eggv_I_peak))

# calculate the slope of the pre-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is simply the baseline age 
# slope)
PrePeak_age_effect_sim <- 
  c(mean(mod_eggv_I_peak_sim@fixef[, "est_age_t_deviation"]), 
    quantile(mod_eggv_I_peak_sim@fixef[, "est_age_t_deviation"], prob = c(0.025, 0.975)))

# calculate the slope of the post-peak age effect (i.e., because pre-peak is 
# set as the baseline level of the factor, this is calculated as the baseline 
# age slope plus the interaction slope)
PostPeak_age_effect_sim <- 
  c(mean(mod_eggv_I_peak_sim@fixef[, "est_age_t_deviation"] + 
           mod_eggv_I_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"]), 
    quantile(mod_eggv_I_peak_sim@fixef[, "est_age_t_deviation"] + 
               mod_eggv_I_peak_sim@fixef[, "PrePeak_mod1:est_age_t_deviation"], 
             prob = c(0.025, 0.975)))