# Model seasonal- and age-dependent variation in egg volume

# Product: stats and figure of the chick egg volume ~ senescence + season model

#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

load("data/ceuta_egg_chick_female_data.rds")

#### Modeling egg volume ----
# Full model predicting egg volume variation by:
# - quadratic age effect
# - selective appearence and dissapearence
# - female structural size (tarsus)
# - quadratic seasonal effect
# Random intercepts: nest, individual, year
# mod_eggv_age_date_tarsi <-
#   lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
#          poly(jul_lay_date_std_num, 2) +
#          (1|ID) + (1|ring) + (1|year),
#        data = ceuta_egg_chick_female_data)
# 
# # run tidy bootstrap to obtain model diagnostics
# tidy_eggv_age_date_tarsi <-
#   tidy(mod_eggv_age_date_tarsi, conf.int = TRUE, conf.method = "boot", nsim = 1000)
# 
# # run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# # weights
# rpt_eggv_age_date_tarsi <-
#   rpt(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
#         poly(jul_lay_date_std_num, 2) +
#         (1|ID) + (1|ring) + (1|year),
#       grname = c("ID", "ring", "year", "Fixed"),
#       data = ceuta_egg_chick_female_data,
#       datatype = "Gaussian",
#       nboot = 1000, npermut = 1000, ratio = TRUE,
#       adjusted = FALSE, ncores = 4, parallel = TRUE)
# 
# # run rptR to obtain repeatabilities of random effects
# R2_eggv_age_date_tarsi <-
#   partR2(mod_eggv_age_date_tarsi,
#          partvars = c("poly(est_age_trans, 2)",
#                       "poly(jul_lay_date_std_num, 2)",
#                       "firstage",
#                       "lastage",
#                       "avg_ad_tarsi"),
#          R2_type = "marginal",
#          nboot = 1000,
#          CI = 0.95,
#          max_level = 1)
# 
# # save model, tidy, rptR, and partR2 output as a list
# stats_eggv_age_date_tarsi <-
#   list(mod = mod_eggv_age_date_tarsi,
#        tidy = tidy_eggv_age_date_tarsi,
#        rptR = rpt_eggv_age_date_tarsi,
#        partR2 = R2_eggv_age_date_tarsi)
# 
# save(stats_eggv_age_date_tarsi,
#      file = "output/stats_eggv_age_date_tarsi.rds")

load("output/stats_eggv_age_date_tarsi.rds")

# model summary a diagnostics
summary(stats_eggv_age_date_tarsi$mod)
plot(allEffects(stats_eggv_age_date_tarsi$mod))
coefplot2(stats_eggv_age_date_tarsi$mod)
summary(glht(stats_eggv_age_date_tarsi$mod))