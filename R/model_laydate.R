# Model age-dependent variation in lay date

# Product: stats and figure of the lay date ~ senescence model

#### Libraries and data ----
source("R/001_libraries.R")
source("R/002_functions.R")

load("data/raw/ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, jul_lay_date_std_num, est_age, year,
                firstage, lastage, nest_order, n_years_obs) %>% 
  dplyr::filter(nest_order == 1) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age))

#### Modeling ----
# # Procedure:
# # mixed effects regression of laydate ~ senescence with mother ID as random
# # effects
# mod_date_age <-
#   lmer(jul_lay_date_std_num ~ poly(est_age, 2) + firstage + lastage +
#          (1|ring) + (1|year),
#        data = first_nests_age_data)
# 
# # run tidy bootstrap to obtain model diagnostics
# tidy_date_age <-
#   tidy(mod_date_age, conf.int = TRUE, conf.method = "boot", nsim = 1000)
# 
# # run rptR to obtain repeatabilities of random effects
# rpt_date_age <-
#   rpt(jul_lay_date_std_num ~ poly(est_age, 2) + firstage + lastage + 
#         (1|ring) + (1|year),
#       grname = c("ring", "Fixed"),
#       data = first_nests_age_data,
#       datatype = "Gaussian",
#       nboot = 1000, npermut = 1000, ratio = TRUE,
#       adjusted = FALSE, ncores = 4, parallel = TRUE)
# 
# # run partR2 to obtain marginal R2, parameter estimates, and beta weights
# R2_date_age <-
#   partR2(mod_date_age,
#          partvars = c("poly(est_age, 2)",
#                       "firstage",
#                       "lastage"),
#          R2_type = "marginal",
#          nboot = 1000, CI = 0.95, max_level = 1)
# 
# # save model, tidy, rptR, and partR2 output as a list
# stats_date_age <-
#   list(mod = mod_date_age,
#        tidy = tidy_date_age,
#        rptR = rpt_date_age,
#        partR2 = R2_date_age)
# save(stats_date_age,
#      file = "data/out/stats_date_age.rds")

#### Results ----
load("data/out/stats_date_age.rds")

#### Quick model diagnostics ----
plot(allEffects(stats_date_age$mod))
coefplot2(stats_date_age$mod)
summary(glht(stats_date_age$mod))
summary(stats_date_age$mod)

#### Repeatabilities ----
date_age_mod_rpt_R <- 
  cbind(t(stats_date_age$rptR$R), stats_date_age$rptR$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(stats_date_age$rptR$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = c("Laydate", "Marginal_R2"))

#### Forest plots of Marginal R2 output ----
date_age_mod_out_R2 = stats_date_age$partR2$R2
col_all <- "#2E3440"
date_age_mod_out_R2[date_age_mod_out_R2$term == "Full", 1] <- "Model"
names(date_age_mod_out_R2) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
date_age_mod_out_R2 <- 
  date_age_mod_out_R2 %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "firstage", "First age breeding",
                               ifelse(combs == "lastage", "Last age breeding", "Model"))))
date_age_mod_out_R2$combs <- 
  factor(date_age_mod_out_R2$combs, 
         levels = rev(date_age_mod_out_R2$combs))

date_age_mod_R2_plot <- 
  stats_date_age$partR2$R2 %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)", "Senescence", 
                       ifelse(term == "firstage", "First-age breeding",
                              ifelse(term == "lastage", "Last-age breeding", "Model")))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding")))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_text(color = col_all, size = 11, face = "italic"),
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab("Variance Explained") +
  ylab("Lay date model") +
  ggtitle(expression(paste(italic("Partitioned R"), ''^{2}, sep = ""))) +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

#### Forest plots of model estimates ----
date_age_mod_out_ests = stats_date_age$partR2$Ests
col_all <- "#2E3440"

date_age_mod_out_ests[date_age_mod_out_ests$term == "Full", 1] <- "Model"
names(date_age_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
date_age_mod_out_ests <- 
  date_age_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "firstage", "First-age breeding",
                                      ifelse(combs == "lastage", "Last-age breeding", "Model")))))
date_age_mod_out_ests$combs <- factor(date_age_mod_out_ests$combs, levels = rev(date_age_mod_out_ests$combs))

date_age_mod_out_ests_plot <- 
  stats_date_age$partR2$Ests %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text(color = col_all), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab("Standardized lay date") +
  ggtitle("Model estimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5)

#### Forest plots of model estimates ----
date_age_mod_out_BW = stats_date_age$partR2$BW
col_all <- "#2E3440"

date_age_mod_out_BW[date_age_mod_out_BW$term == "Full", 1] <- "Model"
names(date_age_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
date_age_mod_out_BW <- 
  date_age_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "firstage", "First-age breeding",
                                      ifelse(combs == "lastage", "Last-age breeding", "Model")))))
date_age_mod_out_BW$combs <- factor(date_age_mod_out_BW$combs, levels = rev(date_age_mod_out_BW$combs))

date_age_mod_out_BW_plot <- 
  stats_date_age$partR2$BW %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot(aes_string(x = "estimate", y = "term", 
                    xmax = "CI_upper", xmin = "CI_lower"), 
         data = .) + 
  geom_vline(xintercept = 0, color = col_all, 
             linetype = "dashed", size = 0.5) + 
  theme_classic(base_line_size = 0.5, base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(color = col_all), 
        axis.text.y = element_blank(), 
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab(~paste(italic(beta["weights"]))) +
  ggtitle("Standardized\nestimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5)

#### Combo Forest Plot ----
date_age_forest_plot <- 
  (date_age_mod_R2_plot + date_age_mod_out_ests_plot + date_age_mod_out_BW_plot) +
  plot_annotation(tag_levels = "A")
date_age_forest_plot

#### Plot of trend ----
# extract fitted values
date_age_mod_fits <- 
  as.data.frame(effect(term = "poly(est_age, 2)", mod = stats_date_age$mod, 
                       xlevels = list(est_age = seq(min(first_nests_age_data$est_age, na.rm = TRUE), 
                                                    max(first_nests_age_data$est_age, na.rm = TRUE), 1))))

# plot predicted trend and raw data
date_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_jitter(data = first_nests_age_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age, y = jul_lay_date_std_num),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = date_age_mod_fits, aes(x = est_age, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = date_age_mod_fits, 
              aes(x = est_age, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Standardized lay date" %+-%  "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13))
