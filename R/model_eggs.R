# Model seasonal- and age-dependent variation in egg volume

# Product: stats and figure of the chick egg volume ~ senescence + season model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

load("data/ceuta_egg_chick_female_data.rds")

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

mod_eggv_I <-
  lmer(volume_cm ~ est_age_t_deviation + I(est_age_t_deviation^2) +
         first_age_t + last_age_t + avg_ad_tarsi + 
         laydate_deviation +
         first_laydate + I(first_laydate^2) +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data)

# run tidy bootstrap to obtain model diagnostics
tidy_mod_eggv <-
  tidy(mod_eggv_I, conf.int = TRUE, conf.method = "boot", nsim = 1000)

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

# save model, tidy, rptR, and partR2 output as a list
stats_eggv_mod <-
  list(mod_I = mod_eggv_I,
       mod_poly = mod_eggv_poly,
       tidy = tidy_mod_eggv,
       rptR = rpt_mod_eggv,
       partR2m = R2m_mod_eggv,
       partR2c = R2c_mod_eggv)

# save(stats_eggv_mod,
#      file = "output/stats_eggv_mod.rds")

load("output/stats_eggv_mod.rds")

model_parameters(stats_eggv_mod$mod, standardize = "refit")
random_parameters(stats_eggv_mod$mod)
plot(allEffects(stats_eggv_mod$mod_I))

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