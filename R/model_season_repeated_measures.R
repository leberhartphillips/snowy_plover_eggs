# Model within female variation in egg volume over season

#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
library(parameters)
load("data/ceuta_egg_chick_female_data.rds")

#### Seasonal Repeated Measures ----
# do females increase in their body weight over the season? NO

# first wrangle the data to extract only females with a weight measure and
# multiple nests in a single season
multi_nesters <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(n_nests == 2) %>% 
  group_by(ID, ring_year, ring, year, est_age_trans, jul_lay_date,
           firstage, lastage, jul_lay_date_std_num, 
           avg_ad_tarsi, nest_order, nest_1_fate, polyandry) %>% 
  summarise(avg_egg_volume = mean(volume_cm, na.rm = TRUE),
            sd_egg_volume = sd(volume_cm, na.rm = TRUE)) %>% 
  group_by(ring_year) %>% 
  arrange(nest_order) %>% 
  mutate(volume_cm_diff = avg_egg_volume - lag(avg_egg_volume),
         lay_date_diff = jul_lay_date - lag(jul_lay_date),
         nest_1_lay_date = jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         nest_2_lay_date = jul_lay_date_std_num[which.max(jul_lay_date_std_num)],
         nest_1_fate_simp = as.factor(ifelse(nest_1_fate == "Hatch", "Hatched", "Failed")),
         nest_order = as.factor(nest_order)) %>% 
  arrange(ring_year)

multi_nesters <- cbind(
  multi_nesters,
  demean(multi_nesters, select = c("jul_lay_date_std_num"), group = "ring_year")
)

mod_multi_nesters_b_w <-
  lmer(avg_egg_volume ~ poly(est_age_trans, 2) +
         firstage + lastage + avg_ad_tarsi + 
         poly(jul_lay_date_std_num_between, 2) +
         jul_lay_date_std_num_within +
         (1 | ring_year),
       data = multi_nesters)
plot(allEffects(mod_multi_nesters_b_w))


ggplot(data = multi_nesters) +
  geom_point(aes(x = jul_lay_date_std_num, y = avg_egg_volume, color = nest_order),
             alpha = 0.5) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.2) +
  # geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  theme(legend.position = "none") +
  ylab("Average egg volume") +
  xlab("Standardized lay date")

ceuta_egg_chick_female_data_mc <- 
  ceuta_egg_chick_female_data %>% 
  select(ID, ring_year, jul_lay_date_std_num, jul_lay_date) %>%
  distinct() %>% 
  arrange(ring_year, jul_lay_date) %>% 
  group_by(ring_year) %>% 
  mutate(date_deviation = jul_lay_date_std_num - jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         first_date = jul_lay_date_std_num[which.min(jul_lay_date_std_num)]) %>% 
  bind_cols(., demean(., select = c("jul_lay_date_std_num"), group = "ring_year")) %>% 
  ungroup()

first_date_bin = quantile(ceuta_egg_chick_female_data_mc$first_date, c(0, 1/3, 2/3, 1))
first_date_bin[1] = first_date_bin[1] - 0.00005

ceuta_egg_chick_female_data_mc <- 
  ceuta_egg_chick_female_data_mc %>% 
  mutate(first_date_bin = cut(first_date, 
                              breaks = first_date_bin, 
                              labels = c("Early","Middle","Late"))) %>% 
  left_join(select(ceuta_egg_chick_female_data, -jul_lay_date_std_num, -jul_lay_date), .,
                   by = c("ID", "ring_year"))

ceuta_egg_chick_female_data_mc

ceuta_egg_chick_female_data_mc %>% 
  filter(ring_year == "CN0056_2020") %>% 
  select(ID, ring_year, nest_order, first_date_bin, date_deviation, first_date, volume_cm) %>% 
  arrange(ring_year, ID)

mod_multi_nesters <-
  lmer(avg_egg_volume ~ avg_ad_tarsi + jul_lay_date_std_num_between + 
         jul_lay_date_std_num_within +
         (1 | ring),
       data = multi_nesters)

mod_mc <-
  lmer(volume_cm ~ avg_ad_tarsi + jul_lay_date_std_num_between + 
         jul_lay_date_std_num_within +
         (1 | ID) + (1 | ring) + (1 | year),
       data = ceuta_egg_chick_female_data_mc)

mod_mc_full_scale <-
  lmer(scale(volume_cm) ~ scale(poly(est_age_trans, 2)) + scale(firstage) + scale(lastage) + scale(avg_ad_tarsi) +
         scale(jul_lay_date_std_num_between) + scale(jul_lay_date_std_num_within) +
         (1|ID) + (1 + scale(jul_lay_date_std_num_within)|ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

mod_mc_full <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
         jul_lay_date_std_num_between + jul_lay_date_std_num_within +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

coef(mod_mc_full)$ring

intercepts <- coef(mod_mc_full)$ring[, "(Intercept)"]
slopes <- coef(mod_mc_full)$ring[, "scale(jul_lay_date_std_num_within)"]

ggplot() +
  geom_point(aes(x = scale(jul_lay_date_std_num_between), y = volume_cm),
             alpha = 0.5, data = ceuta_egg_chick_female_data_mc) +
  geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  xlab("Scaled lay date") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")", sep = "")))

summary(mod_mc_full_scale)
plot(allEffects(mod_mc_full_scale))
coefplot2(mod_mc_full_scale)
summary(glht(mod_mc_full_scale))

model_parameters(mod_multi_nesters)

# Procedure:
# linear mixed effects regression of egg volume ~ laydate with mother ID and
# year as random effects and random slope for laydate
mod_eggv_laydate_rs <- 
  lmer(avg_egg_volume ~ poly(jul_lay_date_std_num, 2) +
         (jul_lay_date_std_num | ring_year),
       data = multi_nesters)

mod_eggv_diff_fate_x_1date <- 
  lmer(volume_cm_diff ~ nest_1_fate_simp * nest_1_lay_date +
         (1 | ring) + (1 | year),
       data = multi_nesters)

mod_eggv_diff_1date_x_date_diff <- 
  lmer(volume_cm_diff ~ nest_1_lay_date * lay_date_diff +
         (1 | ring) + (1 | year),
       data = multi_nesters)

# simple model summary a diagnostics
summary(mod_eggv_diff_fate_x_1date)
plot(allEffects(mod_eggv_diff_fate_x_1date))
coefplot2(mod_eggv_diff_fate_x_1date)
summary(glht(mod_eggv_diff_fate_x_1date))

coef(mod_eggv_laydate_rs)$ring_year

intercepts <- coef(mod_eggv_laydate_rs)$ring_year[,1] # Specifying the first column only
slopes <- coef(mod_eggv_laydate_rs)$ring_year[,2] # Specifying the second column only

multi_nesters_summary <- 
  multi_nesters %>% 
  mutate(nest_1_lay_date_bin = cut(nest_1_lay_date, 
                                   breaks = nest_1_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         nest_2_lay_date_bin = cut(nest_2_lay_date, 
                                   breaks = nest_2_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         volume_cm_diff = ifelse(is.na(volume_cm_diff), 0, volume_cm_diff)) %>% 
  group_by(nest_1_lay_date_bin, nest_2_lay_date_bin, nest_order) %>% 
  summarise(mean_volume_cm_diff = mean(volume_cm_diff))

hatch_plot <- 
  multi_nesters %>% 
  mutate(nest_1_lay_date_bin = cut(nest_1_lay_date, 
                            breaks = nest_1_lay_date_bin, 
                            labels = c("Early","Middle","Late")),
         nest_2_lay_date_bin = cut(nest_2_lay_date, 
                                   breaks = nest_2_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         volume_cm_diff = ifelse(is.na(volume_cm_diff), 0, volume_cm_diff)) %>%
  filter(nest_1_fate_simp == "Hatched") %>% 
  group_by(nest_1_lay_date_bin, nest_2_lay_date_bin, nest_order) %>% 
  mutate(mean_volume_cm_diff = mean(volume_cm_diff),
         mean_jul_lay_date_std_num = mean(jul_lay_date_std_num)) %>%
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm_diff, color = nest_order),
             alpha = 0.5) +
  geom_line(aes(x = jul_lay_date_std_num, y = volume_cm_diff, group = ring_year),
            alpha = 0.2) +
  geom_line(aes(x = mean_jul_lay_date_std_num, y = mean_volume_cm_diff, group = ring_year),
            alpha = 1) +
  # geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  theme(legend.position = "none",
        plot.margin = unit(c(0.8,0.8,0.2,0.2), "cm")) +
  ylab("Difference in average egg volume between successive nests") +
  xlab("Standardized lay date") +
  # facet_grid(nest_1_fate_simp ~ .) +
  facet_grid(nest_2_lay_date_bin ~ nest_1_lay_date_bin)

multi_nesters_summary <- 
  multi_nesters %>% 
  mutate(nest_1_lay_date_bin = cut(nest_1_lay_date, 
                                   breaks = nest_1_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         nest_2_lay_date_bin = cut(nest_2_lay_date, 
                                   breaks = nest_2_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         volume_cm_diff = ifelse(is.na(volume_cm_diff), 0, volume_cm_diff)) %>%
  group_by(nest_1_lay_date_bin, nest_2_lay_date_bin, nest_order) %>% 
  summarise(mean_volume_cm_diff = mean(volume_cm_diff),
            mean_jul_lay_date_std_num = mean(jul_lay_date_std_num),
            sd_volume_cm_diff = sd(volume_cm_diff),
            n_volume_cm_diff = n()) %>%
  mutate(upper_volume_cm_diff = mean_volume_cm_diff + (1.96*(sd_volume_cm_diff/sqrt(n_volume_cm_diff))),
         lower_volume_cm_diff = mean_volume_cm_diff - (1.96*(sd_volume_cm_diff/sqrt(n_volume_cm_diff))))

multi_nesters %>% 
  mutate(nest_1_lay_date_bin = cut(nest_1_lay_date, 
                                   breaks = nest_1_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         nest_2_lay_date_bin = cut(nest_2_lay_date, 
                                   breaks = nest_2_lay_date_bin, 
                                   labels = c("Early","Middle","Late")),
         volume_cm_diff = ifelse(is.na(volume_cm_diff), 0, volume_cm_diff)) %>%
  group_by(nest_1_lay_date_bin, nest_2_lay_date_bin, nest_order) %>% 
  mutate(mean_volume_cm_diff = mean(volume_cm_diff),
         mean_jul_lay_date_std_num = mean(jul_lay_date_std_num)) %>%
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm_diff, color = nest_order),
             alpha = 0.5) +
  geom_line(aes(x = jul_lay_date_std_num, y = volume_cm_diff, group = ring_year),
            alpha = 0.2) +
  geom_line(aes(x = mean_jul_lay_date_std_num, y = mean_volume_cm_diff),
            alpha = 1, data = multi_nesters_summary) +
  geom_errorbar(aes(ymin = lower_volume_cm_diff,
                    ymax = upper_volume_cm_diff,
                    x = mean_jul_lay_date_std_num),
                alpha = 1, 
                size = 0.5,
                height = 0, data = multi_nesters_summary) +
  # geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  theme(legend.position = "none",
        plot.margin = unit(c(0.8,0.8,0.2,0.2), "cm")) +
  ylab("Difference in average egg volume between successive nests") +
  xlab("Standardized lay date") +
  # facet_grid(nest_1_fate_simp ~ .) +
  facet_grid(nest_2_lay_date_bin ~ nest_1_lay_date_bin)

grid::grid.text(unit(0.98,"npc"), unit(0.5,"npc"), 
                label = expression(italic('Relative laydate of second nest')), rot = 270) # right
grid::grid.text(unit(0.5,"npc"), unit(.98,'npc'), 
                label = expression(italic('Relative laydate of first nest')), rot = 0)   # top

nest_1_lay_date_bin = quantile(multi_nesters$nest_1_lay_date, c(0, 1/3, 2/3, 1))
nest_1_lay_date_bin[1]=nest_1_lay_date_bin[1]-.00005

nest_2_lay_date_bin = quantile(multi_nesters$nest_2_lay_date, c(0, 1/3, 2/3, 1))
nest_2_lay_date_bin[1]=nest_2_lay_date_bin[1]-.00005

df1 <- df %>% mutate(category=cut(a, breaks=xs, labels=c("low","middle","high")))
boxplot(df1$a~df1$category,col=3:5)

multi_nesters %>% 
  mutate(volume_cm_diff = ifelse(is.na(volume_cm_diff), 0, volume_cm_diff),
         lay_date_diff = ifelse(is.na(lay_date_diff), 0, lay_date_diff),
         lay_date_bin = cut(nest_1_lay_date, 
                            breaks = xs, labels = c("early","middle","late"))) %>% 
  ggplot(.) +
  geom_point(aes(x = lay_date_diff, y = volume_cm_diff),
             alpha = 0.5) +
  geom_line(aes(x = lay_date_diff, y = volume_cm_diff, group = ring_year),
            alpha = 0.5) +
  luke_theme +
  ylab("Difference in average egg volume between successive nests") +
  xlab("Time between successive nests (days)") +
  facet_grid(nest_1_fate_simp ~ lay_date_bin)

ggplot(data = multi_nesters) +
  geom_point(aes(x = nest_1_lay_date, y = volume_cm_diff),
             alpha = 0.5) +
  # geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
  #           alpha = 0.5) +
  # geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  ylab("Egg volume") +
  xlab("Standardized lay date")

#### Do large females lay large eggs? ----
# Yes: strong effect
# Procedure:
# linear mixed effects regression of 
# year standardized mother weight ~ egg volume with mother ID and
# year as random effects 
mod_motherw_eggv <- 
  lmer(avg_egg_volume_std ~ ad_weight_std +
         (ad_weight_std|mother_ring) + (1|year), 
       data = dplyr::filter(eggs_and_female_nest_summary, 
                            !is.na(ad_weight_std) & n_years_obs > 2))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_motherw_laydate_rs <-
  tidy(mod_motherw_laydate_rs, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_chickw_eggv <-
  rpt(ad_weight ~ avg_egg_volume +
        (1|mother_ring) + (1|year), 
      grname = c("mother_ring", "year", "Fixed"), 
      data = dplyr::filter(eggs_and_female_nest_summary, !is.na(ad_weight)), 
      datatype = "Gaussian", 
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
R2_chickw_eggv <- 
  partR2(mod_chickw_eggv,  
         partvars = c("avg_egg_volume"), 
         R2_type = "marginal", nboot = 1000, CI = 0.95, max_level = 1)

# # save model, tidy, rptR, and partR2 output as a list
stats_chickw_eggv <- 
  list(mod = mod_chickw_eggv,
       tidy = tidy_chickw_eggv,
       rptR = rpt_chickw_eggv,
       partR2 = R2_chickw_eggv)

save(stats_chickw_eggv,
     file = "data/out/stats_chickw_eggv.rds")

mod_motherw_eggv <- 
  lmer(volume_cm ~ ad_weight +
         (1|ID) + (ad_weight|ring) + (1|year), 
       data = dplyr::filter(ceuta_egg_chick_female_data, 
                            !is.na(ad_weight) & n_years_obs > 2))

mod_motherw_eggv <- 
  lmer(volume_cm ~ poly(est_age, 2) + firstage + lastage + 
         poly(jul_lay_date_std_num, 2) + scale(ad_weight) +
         (1|ID) + (scale(ad_weight)|ring) + (1|year), 
       data = dplyr::filter(ceuta_egg_chick_female_data, 
                            !is.na(ad_weight) & n_years_obs > 0))

# model summary a diagnostics
summary(mod_motherw_eggv)
plot(allEffects(mod_motherw_eggv))
coefplot2(mod_motherw_eggv)
summary(glht(mod_motherw_eggv))

coef(mod_motherw_eggv)$ring

intercepts <- coef(mod_motherw_eggv)$ring[,1] # Specifying the first column only
slopes <- coef(mod_motherw_eggv)$ring[,2] # Specifying the second column only

ggplot(data = dplyr::filter(ceuta_egg_chick_female_data, 
                            !is.na(ad_weight) & n_years_obs > 2)) +
  geom_point(aes(x = ad_weight, y = volume_cm),
             alpha = 0.5) +
  geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  xlab("Mother weight (g)") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")", sep = ""))) 

ggplot(data = dplyr::filter(eggs_and_female_nest_summary, 
                            !is.na(ad_weight_std) & n_years_obs > 2)) +
  geom_point(aes(x = ad_weight_std, y = avg_egg_volume_std),
             alpha = 0.5) +
  geom_abline(slope = slopes, intercept = intercepts) +
  luke_theme +
  xlab("Standardized mother weight (g)") +
  ylab(expression(paste("Standardized avg. egg volume (cm", ''^{3}, ")", sep = ""))) 

# Mother tarsi model
mod_mothert_eggv_rs <- 
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + 
         poly(jul_lay_date_std_num, 2) + avg_ad_tarsi +
         (1|ID) + (poly(est_age_trans, 2)|ring) + (1|year), 
       data = ceuta_egg_chick_female_data)

# model summary a diagnostics
summary(mod_mothert_eggv_rs)
plot(allEffects(mod_mothert_eggv_rs))
coefplot2(mod_mothert_eggv_rs)
summary(glht(mod_mothert_eggv_rs))

# run tidy bootstrap to obtain model diagnostics
tidy_mod_mothert_eggv_rs <-
  tidy(mod_mothert_eggv_rs, conf.int = TRUE, conf.method = "boot", nsim = 1000)

save(tidy_mod_mothert_eggv_rs, file = "data/out/tidy_mod_mothert_eggv_rs.rds")

# run rptR to obtain repeatabilities of random effects
rpt_mod_mothert_eggv_rs <-
  rpt(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + 
        poly(jul_lay_date_std_num, 2) + avg_ad_tarsi +
        (1|ID) + (poly(est_age_trans, 2)|ring) + (1|year), 
      grname = c("ID", "ring", "year", "Fixed"), 
      data = ceuta_egg_chick_female_data, 
      datatype = "Gaussian", 
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

save(rpt_mod_mothert_eggv_rs, file = "data/out/rpt_mod_mothert_eggv_rs.rds")

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
mod_mothert_eggv <- 
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + 
         poly(jul_lay_date_std_num, 2) + avg_ad_tarsi +
         (1|ID) + (1|ring) + (1|year), 
       data = ceuta_egg_chick_female_data)

R2_mod_mothert_eggv <- 
  partR2(mod_mothert_eggv,
         partvars = c("poly(est_age_trans, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)