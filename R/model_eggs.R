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
#   lmer(volume_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
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
#   rpt(volume_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
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
#          partvars = c("poly(est_age, 2)",
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

#### Present Results ----
load("output/stats_eggv_age_date_tarsi.rds")

#### Repeatabilities of egg morphometrics (Table) ----
eggv_mod_rpt_R <- 
  cbind(t(eggv_mod_rpt$R), eggv_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggv_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Volume")

egg_shape_rpt_R <- 
  bind_rows(eggv_mod_rpt_R, eggw_mod_rpt_R, eggl_mod_rpt_R)

egg_shape_rpt_R %>% 
  dplyr::filter(group != "Fixed") %>% 
  mutate(group = ifelse(group == "ring", "Individual", 
                        ifelse(group == "ID", "Nest",
                               ifelse(group == "year", "Year", "marginal R@2~")))) %>% 
  mutate(confid_int = ifelse(!is.na(lower95),
                             paste0("[", 
                                    round(lower95, 3), ", ", 
                                    round(upper95, 3), "]"),
                             NA),
         mean = round(mean_estimate, 3)) %>% 
  dplyr::select(trait, group, mean, confid_int) %>% 
  gt(rowname_col = "row",
     groupname_col = "trait") %>% 
  cols_label(group = "Random effect",
             mean = "Adjusted repeatability",
             confid_int = "95% confidence interval") %>% 
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = brewer.pal(9,"Greys")[3],
              table.font.size = 12,
              data_row.padding = 3,
              row_group.padding = 4,
              summary_row.padding = 2,
              column_labels.font.size = 14,
              row_group.font.size = 12,
              table.width = pct(80)) %>% 
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>% 
        str_replace_all("~",
                        "</sup>") }
  )

#### Forest Plots of Marginal R2 egg volume model ####
eggv_mod_out = eggv_mod_R2[["R2"]]
col_all <- "#2E3440"

eggv_mod_out[eggv_mod_out$term == "Full", 1] <- "Model"
names(eggv_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggv_mod_out <- 
  eggv_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggv_mod_out$combs <- factor(eggv_mod_out$combs, levels = rev(eggv_mod_out$combs))

eggv_mod_R2_plot <- 
  eggv_mod_R2[["R2"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)", "Senescence", 
                       ifelse(term == "poly(jul_std_date, 2)", "Seasonality",
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Seasonality")))) %>% 
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
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab(expression(paste("R", ''^{2}, sep = ""))) +
  ylab("Volume model") +
  ggtitle(expression(paste(italic("Partitioned R"), ''^{2}, sep = ""))) +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

#### Forest Plot of egg volume model estimates ####
eggv_mod_out_ests = eggv_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggv_mod_out_ests[eggv_mod_out_ests$term == "Full", 1] <- "Model"
names(eggv_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggv_mod_out_ests <- 
  eggv_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                       ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(combs == "firstage", "First-age breeding",
                                                   ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggv_mod_out_ests$combs <- factor(eggv_mod_out_ests$combs, levels = rev(eggv_mod_out_ests$combs))

eggv_mod_ests_plot <- 
  eggv_mod_R2[["Ests"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                        ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(term == "firstage", "First-age breeding",
                                                    ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
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
  xlab(expression(paste("Volume (mm", ''^{3},")", sep = ""))) +
  ggtitle("Model estimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
  # scale_x_continuous(limits = c(0, 0.11))

#### Forest Plot of egg volume beta weights (standardized estimates) ####
eggv_mod_out_BW = eggv_mod_R2[["BW"]]
col_all <- "#2E3440"

eggv_mod_out_BW[eggv_mod_out_BW$term == "Full", 1] <- "Model"
names(eggv_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggv_mod_out_BW <- 
  eggv_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggv_mod_out_BW$combs <- factor(eggv_mod_out_BW$combs, levels = rev(eggv_mod_out_BW$combs))

eggv_mod_BW_plot <- 
  eggv_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
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
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25),
        plot.title = element_text(color = col_all, size = 11, hjust = 0.5, face = "italic")) + 
  xlab("Standardized\nvolume") +
  ggtitle("Standardized\nestimates") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(-0.17, 0.22))

#### Full forest plot of model results ---- 
egg_volume_forest_plot <- 
  (eggv_mod_R2_plot + eggv_mod_ests_plot + eggv_mod_BW_plot) +
  plot_annotation(tag_levels = "A")
egg_volume_forest_plot

#### Trend plot of egg volume over season ----
# extract the fitted values of the polynomial season effect
eggv_mod_date_fits <- 
  as.data.frame(effect("poly(jul_lay_date_std_num, 2)", eggv_age_date_mod, 
                       xlevels = list(jul_lay_date_std_num = seq(min(ceuta_egg_chick_female_data$jul_lay_date_std_num), 
                                                                 max(ceuta_egg_chick_female_data$jul_lay_date_std_num), 1))))

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ggplot() +
  geom_point(data = ceuta_egg_chick_female_data, alpha = 0.4,
             aes(x = jul_lay_date_std_num, y = volume_cm), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_date_fits, aes(x = jul_lay_date_std_num, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits, aes(x = jul_lay_date_std_num, ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  theme(legend.position = "none") +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

#### Combo plot of seasonal dynamics ----
# run the "model_polyandry script to get the polyandry plots
# source("R/revision/model_polyandry.R")
Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

Season_plot

#### Trend plot of egg volume over age ----
# extract fitted values
eggv_mod_age_fits <- 
  as.data.frame(effect(term = "poly(est_age, 2)", mod = eggv_age_date_mod, 
                       xlevels = list(est_age = seq(min(ceuta_egg_chick_female_data$est_age, na.rm = TRUE), 
                                                    max(ceuta_egg_chick_female_data$est_age, na.rm = TRUE), 1))))

# plot fitted values and raw data
eggv_age_trend_plot <- 
  ggplot() +
  luke_theme +
  theme(panel.border = element_blank()) +
  geom_jitter(data = ceuta_egg_chick_female_data, 
              alpha = 0.4, width = 0.3,
              aes(x = est_age, y = volume_cm),
              shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = eggv_mod_age_fits, aes(x = est_age, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = eggv_mod_age_fits, 
              aes(x = est_age, ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25, fill = "grey20") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Estimated age (years)") +
  scale_x_continuous(limits = c(0.5, 13.5), breaks = c(1:13))

#### Combo plot of age dynamics ----
# run the "model_laydate script to get the laydate plot
# source("R/revision/model_laydate.R")
Age_plot <-
  (date_age_trend_plot / eggv_age_trend_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 7), c('cm', 'cm')))

Age_plot