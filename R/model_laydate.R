# Model age-dependent variation in lay date

# Product: stats and figure of the lay date ~ senescence model

#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap, nest_order, est_age_t) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation) & nest_order == 1) #%>% 
  # mutate(age_first_cap_dummy = ifelse(age_first_cap == "J", 1, 0))

first_nests_age_data %>% 
  # summarise(n_ind = n_distinct(ring),
  #           n_nests = n_distinct(ID)) %>% 
  group_by(ring, year) %>% 
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
     file = "output/stats_laydate_mod.rds")

load(file = "output/stats_laydate_mod.rds")

mod_laydate <-
  lmer(first_laydate ~ est_age_t_deviation +
         first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

model_parameters(mod_laydate, standardize = "refit")
model_parameters(mod_laydate)
model_parameters(stats_laydate_mod$mod_I, standardize = "refit")
model_parameters(stats_laydate_mod$mod_I)

plot(allEffects(stats_laydate_mod$mod_I))
random_parameters(stats_laydate_mod$mod_I)

#### Peak-performance post-hoc analysis ----
# set seed to make simulation reproducable
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
       data = filter(first_nests_age_data, year != "2006"))

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