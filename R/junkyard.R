#### Old egg volume modelling stuff ----

mod_eggv_age_date_tarsi <-
  lmer(volume_cm ~ est_age_trans + I(est_age_trans^2) + firstage + lastage + 
         avg_ad_tarsi + jul_lay_date_std_num + I(jul_lay_date_std_num^2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

# run tidy bootstrap to obtain model diagnostics
tidy_eggv_age_date_tarsi <-
  tidy(mod_eggv_age_date_tarsi, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
rpt_eggv_age_date_tarsi <-
  rpt(volume_cm ~ est_age_trans + I(est_age_trans^2) + firstage + lastage + avg_ad_tarsi +
        jul_lay_date_std_num + I(jul_lay_date_std_num^2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run rptR to obtain repeatabilities of random effects
mod_eggv_age_date_tarsi_ <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
         poly(jul_lay_date_std_num, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

R2m_eggv_age_date_tarsi <-
  partR2(mod_eggv_age_date_tarsi_,
         partvars = c("poly(est_age_trans, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

R2c_eggv_age_date_tarsi <-
  partR2(mod_eggv_age_date_tarsi_,
         partvars = c("poly(est_age_trans, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "conditional",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_eggv_age_date_tarsi <-
  list(mod_I = mod_eggv_age_date_tarsi,
       mod_poly = mod_eggv_age_date_tarsi_,
       tidy = tidy_eggv_age_date_tarsi,
       rptR = rpt_eggv_age_date_tarsi,
       partR2m = R2m_eggv_age_date_tarsi,
       partR2c = R2c_eggv_age_date_tarsi)

save(stats_eggv_age_date_tarsi,
     file = "output/stats_eggv_age_date_tarsi_.rds")


# model summary a diagnostics
summary(stats_eggv_age_date_tarsi$mod)
plot(allEffects(stats_eggv_age_date_tarsi$mod))
coefplot2(stats_eggv_age_date_tarsi$mod)
summary(glht(stats_eggv_age_date_tarsi$mod))

#### mean-centered model of egg volume ----


load("output/stats_eggv_van_de_pol.rds")

# model summary a diagnostics
summary(mod_eggv_van_de_pol)
plot(allEffects(mod_eggv_van_de_pol))
coefplot2(mod_eggv_van_de_pol)
summary(glht(mod_eggv_van_de_pol))
model_parameters(mod_eggv_van_de_pol, standardize = "refit")
random_parameters(mod_eggv_van_de_pol)

# extract the fitted values of the within- and between-individual effects
mod_eggv_van_de_pol_w_fits <- 
  as.data.frame(effect("date_deviation_w", mod_eggv_van_de_pol, 
                       xlevels = list(date_deviation_w = seq(min(ceuta_egg_chick_female_data_mc$date_deviation_w), 
                                                             max(ceuta_egg_chick_female_data_mc$date_deviation_w), 1)))) %>%
  mutate(date_deviation_w_adj = date_deviation_w - max(date_deviation_w)/2)

mod_eggv_van_de_pol_b_fits <- 
  as.data.frame(effect("poly(firstdate_b, 2)", mod_eggv_van_de_pol, 
                       xlevels = list(firstdate_b = seq(min(ceuta_egg_chick_female_data_mc$firstdate_b), 
                                                        max(ceuta_egg_chick_female_data_mc$firstdate_b), 1))))

eggv_date_mod_plot_test <- 
  ceuta_egg_chick_female_data_mc %>% 
  group_by(ID) %>% 
  mutate(avg_egg_volume = mean(volume_cm)) %>% 
  ggplot(.) +
  geom_point(aes(x = jul_lay_date_std_num, y = volume_cm),
             shape = 19, color = brewer.pal(8, "Set1")[c(2)], alpha = 0.1) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.1) +
  geom_line(data = mod_eggv_van_de_pol_w_fits, aes(x = date_deviation_w_adj, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_eggv_van_de_pol_w_fits, aes(x = date_deviation_w_adj,
                                                     ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = mod_eggv_van_de_pol_b_fits, aes(x = firstdate_b, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod_eggv_van_de_pol_b_fits, aes(x = firstdate_b, 
                                                     ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-60, 60)) +
  annotate(geom = "text", y = 9, x = -58,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot_test) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Season_plot_test.png",
       width = 10,
       height = 20, units = "cm", dpi = 600)



mod_mc_full_scale <-
  lmer(scale(volume_cm) ~ scale(poly(est_age_trans, 2)) + scale(firstage) + scale(lastage) + scale(avg_ad_tarsi) +
         scale(jul_lay_date_std_num_between) + scale(jul_lay_date_std_num_within) +
         (1|ID) + (1 + scale(jul_lay_date_std_num_within)|ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

# run tidy bootstrap to obtain model diagnostics
tidy_mod_mc_full_scale <-
  tidy(mod_mc_full_scale, conf.int = TRUE, conf.method = "boot", nsim = 1000)

model_parameters(mod_mc_full_scale)
random_parameters(mod_mc_full_scale)

# run partR2 on each model to obtain marginal R2, parameter estimates, and beta
# weights
rpt_mod_mc_full_scale <-
  rpt(scale(volume_cm) ~ scale(poly(est_age_trans, 2)) + 
        scale(firstage) + scale(lastage) + scale(avg_ad_tarsi) +
        scale(jul_lay_date_std_num_between) + scale(jul_lay_date_std_num_within) +
        (1|ID) + (1 + scale(jul_lay_date_std_num_within)|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data_mc,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run rptR to obtain repeatabilities of random effects
mod_mc_full_scale_ri <-
  lmer(scale(volume_cm) ~ scale(poly(est_age_trans, 2)) + scale(firstage) + scale(lastage) + scale(avg_ad_tarsi) +
         scale(jul_lay_date_std_num_between) + scale(jul_lay_date_std_num_within) +
         (1|ID) + (1 |ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

mod_mc_full_ri <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
         jul_lay_date_std_num_between + jul_lay_date_std_num_within +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

mod_mc_full_ri_poly <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
         poly(jul_lay_date_std_num_between, 2) + jul_lay_date_std_num_within +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data_mc)

mod_mc_sub_ri <-
  lmer(volume_cm ~ poly(est_age_trans, 2) + firstage + lastage + avg_ad_tarsi +
         jul_lay_date_std_num_between + jul_lay_date_std_num_within +
         (1|ID) + (1|ring) + (1|year),
       data = filter(ceuta_egg_chick_female_data_mc, n_nests > 1))

model_parameters(mod_mc_sub_ri, standardize = "refit", summary = TRUE, bootstrap = TRUE)
test <- model_parameters(mod_mc_full_ri, standardize = "refit", summary = TRUE, bootstrap = TRUE, robust = TRUE)

boot_test <- bootstrap_parameters(mod_mc_full_ri)
boot_test_scale <- bootstrap_parameters(mod_mc_full_scale_ri)


plot(allEffects(mod_mc_full_ri)$jul_lay_date_std_num_within)

plot(allEffects(mod_mc_full_ri)$jul_lay_date_std_num_between)

# between-individual date effect full data
allEffects(mod_mc_full_ri)[5]
7.478388 - 7.723046

# within-individual date effect full data
plot(allEffects(mod_mc_full_ri)[6])
7.447550 - 7.725868

allEffects(mod_eggv_age_date_tarsi)[6]
6.937909 - 7.621047

# within-individual date effect subset data
allEffects(mod_mc_full_ri)[1]
7.593806 - 7.323466

# tarsus effect
allEffects(mod_mc_full_ri)[4]
7.337063 - 7.913600

# within-individual date effect subset data
allEffects(mod_mc_sub_ri)[6]
7.435108 - 7.711165

# between-individual quadratic date effect full data
allEffects(mod_mc_full_ri_poly)[5]
7.401746 - 7.644479

# 0.05mm error in measurement results in a 0.47cm^3 difference in egg size
0.486 * (3.09) * (2.24)^2 -
  0.486 * (3.14) * (2.29)^2

plot(allEffects(mod_mc_full_ri)$jul_lay_date_std_num_within)
plot(allEffects(mod_mc_sub_ri)$jul_lay_date_std_num_within)

ceuta_egg_chick_female_data_mc %>% 
  select(ID, ring, year, est_age_trans, firstage, lastage, avg_ad_tarsi, jul_lay_date_std_num,
         jul_lay_date_std_num_between, jul_lay_date_std_num_within) %>% 
  distinct() %>% 
  arrange(ring, year, jul_lay_date_std_num) %>% 
  head()

# model summary a diagnostics
summary(mod_mc_full_scale_ri)
plot(allEffects(mod_mc_full_ri))
coefplot2(mod_mc_full_ri)
summary(glht(mod_mc_full_ri))

R2m_eggv_age_date_tarsi <-
  partR2(mod_mc_full_scale_ri,
         partvars = c("scale(poly(est_age_trans, 2))",
                      "scale(jul_lay_date_std_num_between)",
                      "scale(jul_lay_date_std_num_within)",
                      "scale(firstage)",
                      "scale(lastage)",
                      "scale(avg_ad_tarsi)"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

R2c_mc_full_scale_ri <-
  partR2(mod_mc_full_scale_ri,
         partvars = c("scale(poly(est_age_trans, 2))",
                      "scale(jul_lay_date_std_num_between)",
                      "scale(jul_lay_date_std_num_within)",
                      "scale(firstage)",
                      "scale(lastage)",
                      "scale(avg_ad_tarsi)"),
         R2_type = "conditional",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_eggv_age_mc_date_tarsi_scale <-
  list(mod_rs = mod_mc_full_scale,
       mod_ri = mod_mc_full_scale_ri,
       tidy = tidy_mod_mc_full_scale,
       rptR = rpt_mod_mc_full_scale,
       partR2m = R2m_eggv_age_date_tarsi,
       partR2c = R2c_mc_full_scale_ri,
       mod_parameters = model_parameters(mod_mc_full_scale),
       ran_parameters = random_parameters(mod_mc_full_scale))

save(stats_eggv_age_mc_date_tarsi_scale,
     file = "output/stats_eggv_age_mc_date_tarsi_scale.rds")

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
  mutate(combs = ifelse(combs == "poly(est_age_trans, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", 
                                             ifelse(combs == "avg_ad_tarsi", "Body size", "Model"))))))
eggv_mod_out$combs <- factor(eggv_mod_out$combs, levels = rev(eggv_mod_out$combs))

eggv_mod_R2_plot <- 
  eggv_mod_R2[["R2"]] %>% 
  mutate(term = ifelse(term == "poly(est_age_trans, 2)", "Senescence", 
                       ifelse(term == "poly(jul_std_date, 2)", "Seasonality",
                              ifelse(term == "firstage", "First-age breeding",
                                     ifelse(term == "lastage", "Last-age breeding", 
                                            ifelse(combs == "avg_ad_tarsi", "Body size", "Model")))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Seasonality", "Body size")))) %>% 
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
  mutate(combs = ifelse(combs == "poly(est_age_trans, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age_trans, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", 
                                                           ifelse(combs == "avg_ad_tarsi", "Tarsus length", "Model"))))))))
eggv_mod_out_ests$combs <- factor(eggv_mod_out_ests$combs, levels = rev(eggv_mod_out_ests$combs))

eggv_mod_ests_plot <- 
  eggv_mod_R2[["Ests"]] %>% 
  mutate(term = ifelse(term == "poly(est_age_trans, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age_trans, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(combs == "lastage", "Last-age breeding", 
                                                          ifelse(combs == "avg_ad_tarsi", "Tarsus length", "Model")))))))) %>% 
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
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5)

#### Forest Plot of egg volume beta weights (standardized estimates) ####
eggv_mod_out_BW = eggv_mod_R2[["BW"]]
col_all <- "#2E3440"

eggv_mod_out_BW[eggv_mod_out_BW$term == "Full", 1] <- "Model"
names(eggv_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggv_mod_out_BW <- 
  eggv_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age_trans, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age_trans, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", 
                                                           ifelse(combs == "avg_ad_tarsi", "Tarsus length", "Model"))))))))
eggv_mod_out_BW$combs <- factor(eggv_mod_out_BW$combs, levels = rev(eggv_mod_out_BW$combs))

eggv_mod_BW_plot <- 
  eggv_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age_trans, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age_trans, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", 
                                                          ifelse(combs == "avg_ad_tarsi", "Tarsus length", "Model")))))))) %>% 
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

#### Dingemanse et al. (2020) method to calculate model statistics ----
# set seed to make simulation reproducible
set.seed(14)

# specify the number of simulations you would like to produce
n_sim = 5000

mod2 <- stats_eggv_age_date_tarsi$mod

summary(mod2)

# create a sim object containing all the components of mod2
mod2_sim <- sim(mod2, n.sim = n_sim)

# Retrieve all random effect estimates (mean, credible intervals)
# speficy column names of the sim object as the names of the model components
colnames(mod2_sim@fixef) <- 
  names(fixef(mod2))

# Retrieve all random effect estimates (mean, credible intervals) 
# simultaneously
coef_ranef_mod2 <- 
  lapply(ranef(mod2_sim), function(x) c(mean(apply(x[, , 1], 1, var)),  
                                        quantile(apply(x[, , 1], 1, var), 
                                                 prob = c(0.025, 0.975))))

# Transpose the random effects table
ranefTable <- 
  as.data.frame(t(as.data.frame(coef_ranef_mod2))) %>%
  rownames_to_column("coefName")

# Retrieve all fixed effect estimates (mean, credible intervals) 
# simultaneously 
coef_fixef_mod2 <- 
  rbind(apply(mod2_sim@fixef, 2, mean), 
        apply(mod2_sim@fixef, 2, quantile, 
              prob = c(0.025, 0.975)))

# Transpose the fixed effects table
fixefTable <- 
  as.data.frame(t(as.data.frame(coef_fixef_mod2))) %>%
  rownames_to_column("coefName")

# Calculate adjusted repeatabilities
# Retrieve residual variance estimate (mean, credible intervals)
coef_res_mod2 <- 
  c(mean(mod2_sim@sigma^2), quantile(mod2_sim@sigma^2, c(0.025, 0.975)))

# Transpose the residual effects table
resTable <- 
  as.data.frame(t(as.data.frame(coef_res_mod2))) %>%
  mutate(coefName = "residual")

# Calculate total phenotypic variance not explained by fixed effects
ranefAndResidualAseggdf <- 
  cbind(as.data.frame(lapply(ranef(mod2_sim), 
                             function(x) apply(x[, , 1], 1, var))), 
        residual = mod2_sim@sigma^2)

ranefAndResidualAseggdf$varTotal <- 
  rowSums(ranefAndResidualAseggdf)

# Express each random effect as proportion of total (rpt)
rpt_each <- 
  ranefAndResidualAseggdf %>%
  mutate_at(vars( -varTotal), funs(./varTotal)) %>% 
  suppressWarnings()

coef_rpt_all <- 
  apply(rpt_each, 2, 
        function(x) c(mean (x), quantile (x, prob = c(0.025, 0.975))))

coefRptTable <- 
  as.data.frame(t(coef_rpt_all)) %>%
  rownames_to_column("coefName") %>%
  filter(!coefName %in% c("varTotal"))

#### Table of effect sizes ----
# Retrieve sample sizes
sample_sizes <-
  ceuta_egg_chick_female_data %>% 
  summarise(Year = n_distinct(year),
            Individual = n_distinct(ring),
            Nests = n_distinct(ID),
            Observations = nrow(.))

sample_sizes <- 
  as.data.frame(t(as.data.frame(sample_sizes))) %>%
  rownames_to_column("term") %>% 
  rename(estimate = V1) %>% 
  mutate(stat = "n")


# clean model component names
mod_comp_names <- 
  data.frame(comp_name = c("Linear age",
                           "Quadratic age",
                           "First breeding age",
                           "Last breeding age",
                           "Mother tarsus",
                           "Linear lay date",
                           "Quadratic lay date",
                           "Total Marginal \U1D479\U00B2",
                           "Senescence",
                           "Seasonality",
                           "First breeding age",
                           "Last breeding age",
                           "Mother tarsus",
                           "Total Conditional \U1D479\U00B2",
                           "Nest / Individual",
                           "Individual",
                           "Year",
                           "Residual",
                           "Nest / Individual",
                           "Individual",
                           "Year",
                           "Residual",
                           "Years",
                           "Individuals",
                           "Nests",
                           "Observations (i.e., Eggs)"))

fixefTable <- 
  stats_eggv_age_date_tarsi$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed")

fixef_bw_Table <- 
  stats_eggv_age_date_tarsi$partR2m$BW %>% 
  # dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

R2Table <- 
  bind_rows(stats_eggv_age_date_tarsi$partR2m$R2,
            stats_eggv_age_date_tarsi$partR2c$R2[1,]) %>% 
  # dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

ranefTable <- 
  stats_eggv_age_date_tarsi$tidy %>% 
  dplyr::filter(effect == "ran_pars") %>% 
  dplyr::select(group, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "rand") %>% 
  rename(term = group) %>% 
  mutate(estimate = estimate^2,
         conf.high = conf.high^2,
         conf.low = conf.low^2)

coefRptTable <- 
  stats_eggv_age_date_tarsi$rptR$R_boot %>% 
  dplyr::select(-Fixed) %>% 
  mutate(residual = 1 - rowSums(.)) %>% 
  apply(., 2, 
        function(x) c(mean (x), quantile (x, prob = c(0.025, 0.975)))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("term") %>% 
  rename(estimate = V1,
         conf.low = `2.5%`,
         conf.high = `97.5%`) %>% 
  mutate(stat = "RptR")

# Store all parameters into a single table and clean it up
allCoefs_mod <- 
  bind_rows(fixef_bw_Table,
            R2Table,
            ranefTable, 
            coefRptTable, 
            sample_sizes) %>% 
  bind_cols(.,
            mod_comp_names) %>%
  mutate(coefString = ifelse(!is.na(conf.low),
                             paste0("[", 
                                    round(conf.low, 2), ", ", 
                                    round(conf.high, 2), "]"),
                             NA),
         effect = c(rep("Fixed effects \U1D6FD (standardized)", nrow(fixef_bw_Table)),
                    rep("Partitioned \U1D479\U00B2", nrow(R2Table)),
                    rep("Random effects \U1D70E\U00B2", nrow(ranefTable)),
                    rep("Adjusted repeatability \U1D45F", nrow(coefRptTable)),
                    rep("Sample sizes \U1D45B", nrow(sample_sizes)))) %>%
  dplyr::select(effect, everything())

# re-organize model components for table
allCoefs_mod <-
  allCoefs_mod[c(5, 1:4, 6:8, 14, 13, 9, 11:12, 10, 15:26), ]

eggv_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Egg volume</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:22,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 23:26,
             decimals = 0,
             use_seps = FALSE) %>% 
  fmt_missing(columns = 1:4,
              missing_text = "") %>% 
  cols_align(align = "left",
             columns = vars(comp_name)) %>% 
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = brewer.pal(9,"Greys")[3],
              table.font.size = 12,
              data_row.padding = 3,
              row_group.padding = 4,
              summary_row.padding = 2,
              column_labels.font.size = 14,
              row_group.font.size = 12,
              table.width = pct(60))

eggv_mod_table

eggv_mod_table %>%
  gtsave("eggv_mod_table.rtf", path = "products/tables/rtf/")

eggv_mod_table %>%
  gtsave("eggv_mod_table.png", path = "products/tables/png/")

#### Forest plot of results ----
col_all <- "#2E3440"

eggv_mod_forest_plot_fixef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Fixed") & 
           term != "(Intercept)") %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Quadratic lay date", "Linear lay date", 
                                 "Last breeding age", "First breeding age", 
                                 "Quadratic age", "Linear age",
                                 "Mother tarsus")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Fixed effects") +
  xlab(expression(italic(paste("Standardized effect size (", beta,")" %+-% "95% CI", sep = ""))))

eggv_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Seasonality",
                                 "Last breeding age",
                                 "First breeding age",
                                 "Senescence",
                                 "Mother tarsus",
                                 "Total Conditional \U1D479\U00B2",
                                 "Total Marginal \U1D479\U00B2")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  scale_y_discrete(labels = c("Seasonality" = expression("Seasonality"),
                              "Last breeding age" = expression("Last breeding age"),
                              "First breeding age" = expression("First breeding age"),
                              "Senescence" = expression("Senescence"),
                              "Mother tarsus" = expression("Mother tarsus"),
                              "Total Marginal \U1D479\U00B2" = expression(paste("Total marginal ", italic("R"), ''^{2}, sep = "")))) +
  ylab(expression(paste("Semi-partial ", italic("R"),''^{2}, sep = ""))) +
  xlab(expression(italic(paste("Variance explained (R", ''^{2}, ")" %+-% "95% CI", sep = ""))))

eggv_mod_forest_plot_randef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Random")) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Residual",
                                 "Year",
                                 "Individual",
                                 "Nest / Individual")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Random\neffects") +
  xlab(expression(italic(paste("Variance (", sigma, ''^{2}, ")" %+-% "95% CI", sep = ""))))

eggv_mod_forest_plot_rptR <-
  allCoefs_mod %>%
  filter(str_detect(effect, "repeat")) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Residual",
                                 "Year",
                                 "Individual",
                                 "Nest / Individual")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Intra-class\ncorrelation") +
  xlab(expression(italic(paste("Adjusted repeatability (r)" %+-% "95% CI", sep = ""))))

eggv_mod_forest_plot_combo <-
  (eggv_mod_forest_plot_fixef / eggv_mod_forest_plot_partR2 / 
     eggv_mod_forest_plot_randef / eggv_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Egg volume model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(4.5, 4, 2.5, 2.5), c('cm', 'cm', 'cm', 'cm')))

eggv_mod_forest_plot_combo

ggsave(plot = eggv_mod_forest_plot_combo,
       filename = "products/figures/jpg/eggv_mod_forest_plot.jpg",
       width = 4.5,
       height = 8.75, units = "in")

ggsave(plot = eggv_mod_forest_plot_combo,
       filename = "products/figures/svg/eggv_mod_forest_plot.svg",
       width = 4.5,
       height = 8.75, units = "in")

#### Old Polyandry modelling stuff ----

# Marginal R2
stats_poly_date$partR2$R2

# Parameter estimates
stats_poly_date$partR2$Ests

# Repeatabilities
bind_cols(as.data.frame(t(stats_poly_date$rptR$R)), 
          stats_poly_date$rptR$CI_emp$CI_org) %>% 
  dplyr::select(-2)

# model summary a diagnostics
summary(mod_poly_date)
plot(allEffects(mod_poly_date))
coefplot2(mod_poly_date)
summary(glht(mod_poly_date))

#### Plotting of Figure ----
# extract fitted values
poly_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(jul_lay_date_std_num - offs) + (1| ring) + (1 | year), 
                       data = first_nests_data, family = binomial)
  
  ests <- summary(model)$coefficients[1,1:2]
  
  # backlink the coefficients to the probability scale
  return(c(offs, ests, invlogit(ests[1] + c(-1, 0, 1) * 1.96 * ests[2])))
}

# specify the offs (i.e., vector of numbers from min to max dates stepped by 1)
offs_jul_lay_date_std2 <- 
  seq(min(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 
      max(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
poly_fits <- sapply(offs_jul_lay_date_std2, poly_mod_fits)

# transpose the matrix
poly_fits <- t(poly_fits)

# convert the matrix to a data.frame
poly_fits <- data.frame(poly_fits)

# define the column names
colnames(poly_fits) <- 
  c("jul_lay_date_std_num", "Estimate", "Std. Error", "Upper", "Mean", "Lower")

polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_data, 
               aes(x = jul_lay_date_std_num, y = poly_plot, 
                   group = polyandry, fill = polyandry), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = first_nests_data, 
              aes(x = jul_lay_date_std_num, y = poly, 
                  group = polyandry, 
                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = poly_fits, 
              aes(x = jul_lay_date_std_num, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = poly_fits, 
            aes(x = jul_lay_date_std_num, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-60, 60)) +
  ylab("Probabilty of polyandry ± 95% CI") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 1.2, x = -58,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# plot the posterior age at peak distribution
polyandry_date_dist_plot <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  dplyr::filter(jul_lay_date_std_num > -50) %>%
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std_num)) %>% 
  ggplot(data = ., aes(x = jul_lay_date_std_num, y = 1, group = polyandry)) + 
  geom_violin(data = . %>% dplyr::filter(polyandry == "mono"), 
              alpha = 0.5, fill = plot_palette_polyandry[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(polyandry == "poly"), 
              alpha = 0.5, fill = plot_palette_polyandry[1], color = "grey50",
              trim = FALSE) +
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  annotate(geom = "text", y = 1.7, x = -58,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))

#### Plotting of Figure ----
# extract fitted values
poly_mod_fits <- function(offs) {
  model <- lme4::glmer(cbind(poly, mono) ~ 
                         I(jul_lay_date_std_num - offs) + (1| ring) + (1 | year), 
                       data = first_nests_data, family = binomial)
  
  ests <- summary(model)$coefficients[1,1:2]
  
  # backlink the coefficients to the probability scale
  return(c(offs, ests, invlogit(ests[1] + c(-1, 0, 1) * 1.96 * ests[2])))
}

# specify the offs (i.e., vector of numbers from min to max dates stepped by 1)
offs_jul_lay_date_std2 <- 
  seq(min(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 
      max(first_nests_data$jul_lay_date_std_num, na.rm = TRUE), 1)

# apply the offs vector to the function (retuning a matrix)
poly_fits <- sapply(offs_jul_lay_date_std2, poly_mod_fits)

# transpose the matrix
poly_fits <- t(poly_fits)

# convert the matrix to a data.frame
poly_fits <- data.frame(poly_fits)

# define the column names
colnames(poly_fits) <- 
  c("jul_lay_date_std_num", "Estimate", "Std. Error", "Upper", "Mean", "Lower")

polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = first_nests_data, 
               aes(x = jul_lay_date_std_num, y = poly_plot, 
                   group = polyandry, fill = polyandry), 
               color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = first_nests_data, 
              aes(x = jul_lay_date_std_num, y = poly, 
                  group = polyandry, 
                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = poly_fits, 
              aes(x = jul_lay_date_std_num, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = poly_fits, 
            aes(x = jul_lay_date_std_num, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-60, 60)) +
  ylab("Probabilty of polyandry ± 95% CI") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 1.2, x = -58,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# plot the posterior age at peak distribution
polyandry_date_dist_plot <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  dplyr::filter(jul_lay_date_std_num > -50) %>%
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std_num)) %>% 
  ggplot(data = ., aes(x = jul_lay_date_std_num, y = 1, group = polyandry)) + 
  geom_violin(data = . %>% dplyr::filter(polyandry == "mono"), 
              alpha = 0.5, fill = plot_palette_polyandry[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(polyandry == "poly"), 
              alpha = 0.5, fill = plot_palette_polyandry[1], color = "grey50",
              trim = FALSE) +
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  annotate(geom = "text", y = 1.7, x = -58,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))

#### Laydate modelling ----
#### Modeling ----
# Procedure:
# mixed effects regression of laydate ~ senescence with mother ID as random
# effects
mod_date_age_tarsi_rec <-
  lmer(jul_lay_date_std_num ~ est_age_trans + I(est_age_trans^2) + 
         firstage + lastage + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

# run tidy bootstrap to obtain model diagnostics
tidy_date_age_tarsi_rec <-
  tidy(mod_date_age_tarsi_rec, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# run rptR to obtain repeatabilities of random effects
rpt_date_age_tarsi_rec <-
  rpt(jul_lay_date_std_num ~ est_age_trans + I(est_age_trans^2) + 
        firstage + lastage + avg_ad_tarsi + age_first_cap +
        (1|ring) + (1|year),
      grname = c("ring", "year", "Fixed"),
      data = filter(first_nests_age_data, year != "2006"),
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# run partR2 to obtain marginal R2, parameter estimates, and beta weights
mod_date_age_tarsi_rec_ <-
  lmer(jul_lay_date_std_num ~ poly(est_age_trans, 2) + 
         firstage + lastage + avg_ad_tarsi + age_first_cap +
         (1|ring) + (1|year),
       data = filter(first_nests_age_data, year != "2006"))

R2m_date_age_tarsi_rec <-
  partR2(mod_date_age_tarsi_rec_,
         partvars = c("poly(est_age_trans, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi",
                      "age_first_cap"),
         R2_type = "marginal",
         nboot = 1000, CI = 0.95, max_level = 1)

R2c_date_age_tarsi_rec <-
  partR2(mod_date_age_tarsi_rec_,
         partvars = c("poly(est_age_trans, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi",
                      "age_first_cap"),
         R2_type = "conditional",
         nboot = 1000, CI = 0.95, max_level = 1)

# save model, tidy, rptR, and partR2 output as a list
stats_date_age_tarsi_rec <-
  list(mod_I = mod_date_age_tarsi_rec,
       mod_poly = mod_date_age_tarsi_rec_,
       tidy = tidy_date_age_tarsi_rec,
       rptR = rpt_date_age_tarsi_rec,
       partR2m = R2m_date_age_tarsi_rec,
       partR2c = R2c_date_age_tarsi_rec)

save(stats_date_age_tarsi,
     file = "output/stats_date_age_tarsi_.rds")

load(file = "output/stats_date_age_tarsi.rds")

stats_date_age_tarsi$tidy


#### Quick model diagnostics ----
plot(allEffects(stats_date_age_tarsi$mod))
plot(allEffects(mod_date_age_tarsi))

coefplot2(stats_date_age_tarsi$mod)
coefplot2(mod_date_age_tarsi)


summary(glht(stats_date_age_tarsi$mod))
summary(glht(mod_date_age_tarsi))

summary(stats_date_age_tarsi$mod)
summary(mod_date_age_tarsi_rec)

#### First nest of local breeding history ----

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_life_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, jul_lay_date_std_num, est_age_trans, year,
                firstage, lastage, nest_order, n_years_obs, avg_ad_tarsi,
                age_first_cap) %>% 
  dplyr::filter(nest_order == 1 & est_age_trans == firstage) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_trans))

first_nests_life_data %>% 
  group_by(age_first_cap) %>% 
  summarise(n_ind = n_distinct(ring),
            n_nests = n())

mod_date_life_tarsi_rec <-
  lm(jul_lay_date_std_num ~ firstage + avg_ad_tarsi + age_first_cap,
     data = first_nests_age_data)

plot(allEffects(mod_date_life_tarsi_rec))
coefplot2(mod_date_life_tarsi_rec)
summary(glht(mod_date_life_tarsi_rec))
summary(mod_date_life_tarsi_rec)

#### Quick model diagnostics ----
plot(allEffects(stats_date_age_tarsi$mod_poly))
coefplot2(stats_date_age_tarsi$mod_poly)
summary(glht(stats_date_age_tarsi$mod_poly))
summary(stats_date_age_tarsi$mod_poly)

#### Repeatabilities ----
date_age_mod_rpt_R <- 
  cbind(t(stats_date_age_tarsi$rptR$R), stats_date_age_tarsi$rptR$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(stats_date_age_tarsi$rptR$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = c("Laydate", "Marginal_R2"))

#### Forest plots of Marginal R2 output ----
date_age_mod_out_R2 = stats_date_age_tarsi$partR2$R2
col_all <- "#2E3440"
date_age_mod_out_R2[date_age_mod_out_R2$term == "Full", 1] <- "Model"
names(date_age_mod_out_R2) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
date_age_mod_out_R2 <- 
  date_age_mod_out_R2 %>% 
  mutate(combs = ifelse(combs == "poly(est_age_trans, 2)", "Senescence", 
                        ifelse(combs == "firstage", "First age breeding",
                               ifelse(combs == "lastage", "Last age breeding",
                                      ifelse(combs == "avg_ad_tarsi", "Tarsus length", "Model")))))
date_age_mod_out_R2$combs <- 
  factor(date_age_mod_out_R2$combs, 
         levels = rev(date_age_mod_out_R2$combs))

date_age_mod_R2_plot <- 
  stats_date_age_tarsi$partR2$R2 %>% 
  mutate(term = ifelse(term == "poly(est_age_trans, 2)", "Senescence", 
                       ifelse(term == "firstage", "First-age breeding",
                              ifelse(term == "lastage", "Last-age breeding",
                                     ifelse(term == "avg_ad_tarsi", "Tarsus length", "Model"))))) %>% 
  mutate(term = factor(term, levels = rev(c("Model", "Senescence", "First-age breeding", "Last-age breeding", "Tarsus length")))) %>% 
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
date_age_mod_out_ests = stats_date_age_tarsi$partR2$Ests
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
  stats_date_age_tarsi$partR2$Ests %>% 
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
date_age_mod_out_BW = stats_date_age_tarsi$partR2$BW
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
  stats_date_age_tarsi$partR2$BW %>% 
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

#### Table of effect sizes ----
# Retrieve sample sizes
sample_sizes <-
  first_nests_age_data %>% 
  summarise(Year = n_distinct(year),
            Individual = n_distinct(ring),
            Nests = n_distinct(ID))

sample_sizes <- 
  as.data.frame(t(as.data.frame(sample_sizes))) %>%
  rownames_to_column("term") %>% 
  rename(estimate = V1) %>% 
  mutate(stat = "n")

# clean model component names
mod_comp_names <- 
  data.frame(comp_name = c("Linear age",
                           "Quadratic age",
                           "First breeding age",
                           "Last breeding age",
                           "Mother tarsus",
                           "Total Marginal \U1D479\U00B2",
                           "Senescence",
                           "First breeding age",
                           "Last breeding age",
                           "Mother tarsus",
                           "Total Conditional \U1D479\U00B2",
                           "Individual",
                           "Year",
                           "Residual",
                           "Individual",
                           "Year",
                           "Residual",
                           "Years",
                           "Individuals",
                           "Observations (i.e., Nests)"))

# Fixed effect sizes (non-standardized)
fixefTable <- 
  stats_laydate_mod$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed")

# Fixed effect sizes (standardized)
fixef_bw_Table <- 
  stats_laydate_mod$partR2m$BW %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
R2Table <- 
  bind_rows(stats_laydate_mod$partR2m$R2,
            stats_laydate_mod$partR2c$R2[1,]) %>% 
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Random effects variances
ranefTable <- 
  stats_laydate_mod$tidy %>% 
  dplyr::filter(effect == "ran_pars") %>% 
  dplyr::select(group, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "rand") %>% 
  rename(term = group) %>% 
  mutate(estimate = estimate^2,
         conf.high = conf.high^2,
         conf.low = conf.low^2)

# Adjusted repeatabilities
coefRptTable <- 
  stats_laydate_mod$rptR$R_boot %>% 
  dplyr::select(-Fixed) %>% 
  mutate(residual = 1 - rowSums(.)) %>% 
  apply(., 2, 
        function(x) c(mean (x), quantile (x, prob = c(0.025, 0.975)))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("term") %>% 
  rename(estimate = V1,
         conf.low = `2.5%`,
         conf.high = `97.5%`) %>% 
  mutate(stat = "RptR")

# Store all parameters into a single table and clean it up
allCoefs_mod <- 
  bind_rows(fixef_bw_Table,
            R2Table,
            ranefTable, 
            coefRptTable, 
            sample_sizes) %>% 
  bind_cols(.,
            mod_comp_names) %>%
  mutate(coefString = ifelse(!is.na(conf.low),
                             paste0("[", 
                                    round(conf.low, 2), ", ", 
                                    round(conf.high, 2), "]"),
                             NA),
         effect = c(rep("Fixed effects \U1D6FD (standardized)", nrow(fixef_bw_Table)),
                    rep("Partitioned \U1D479\U00B2", nrow(R2Table)),
                    rep("Random effects \U1D70E\U00B2", nrow(ranefTable)),
                    rep("Adjusted repeatability \U1D45F", nrow(coefRptTable)),
                    rep("Sample sizes \U1D45B", nrow(sample_sizes)))) %>%
  dplyr::select(effect, everything())

# re-organize model components for table
allCoefs_mod <-
  allCoefs_mod[c(5, 1:4, 6, 11, 10, 7:9, 12:20), ]

# draw gt table
laydate_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Lay date of first nest</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:17,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 18:20,
             decimals = 0,
             use_seps = FALSE) %>% 
  fmt_missing(columns = 1:4,
              missing_text = "") %>% 
  cols_align(align = "left",
             columns = vars(comp_name)) %>% 
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = brewer.pal(9,"Greys")[3],
              table.font.size = 12,
              data_row.padding = 3,
              row_group.padding = 4,
              summary_row.padding = 2,
              column_labels.font.size = 14,
              row_group.font.size = 12,
              table.width = pct(60))

laydate_mod_table

# export table to disk
laydate_mod_table %>% 
  gtsave("laydate_mod_table.rtf", path = "products/tables/rtf/")

laydate_mod_table %>% 
  gtsave("laydate_mod_table.png", path = "products/tables/png/")

#### Forest plot of results ----

# color of mean estimates
col_all <- "#2E3440"

# Standardized fixed effects
laydate_mod_forest_plot_fixef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Fixed") & 
           term != "(Intercept)") %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Last breeding age", "First breeding age", 
                                 "Quadratic age", "Linear age",
                                 "Mother tarsus")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Fixed effects") +
  xlab(expression(italic(paste("Standardized effect size (", beta,")" %+-% "95% CI", sep = ""))))

# Semi-partial R2 estimates
laydate_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 # "Seasonality",
                                 "Last breeding age",
                                 "First breeding age",
                                 "Senescence",
                                 "Mother tarsus",
                                 "Total Conditional \U1D479\U00B2",
                                 "Total Marginal \U1D479\U00B2")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  scale_y_discrete(labels = c(#"Seasonality" = expression("Seasonality"),
    "Last breeding age" = expression("Last breeding age"),
    "First breeding age" = expression("First breeding age"),
    "Senescence" = expression("Senescence"),
    "Mother tarsus" = expression("Mother tarsus"),
    "Total Marginal \U1D479\U00B2" = expression(paste("Total marginal ", italic("R"), ''^{2}, sep = "")))) +
  ylab(expression(paste("Semi-partial ", italic("R"),''^{2}, sep = ""))) +
  xlab(expression(italic(paste("Variance explained (R", ''^{2}, ")" %+-% "95% CI", sep = ""))))

# Random effect variances
laydate_mod_forest_plot_randef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Random")) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Residual",
                                 "Year",
                                 "Individual")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Random\neffects") +
  xlab(expression(italic(paste("Variance (", sigma, ''^{2}, ")" %+-% "95% CI", sep = ""))))

# Adjusted repeatabilities
laydate_mod_forest_plot_rptR <-
  allCoefs_mod %>%
  filter(str_detect(effect, "repeat")) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Residual",
                                 "Year",
                                 "Individual")) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     y = comp_name),
                 alpha = 1, color = col_all, 
                 size = 0.5,
                 height = 0) +
  geom_point(aes(y = comp_name, x = estimate),
             size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, 
             alpha = 1, stroke = 0.5) +
  luke_theme +
  theme(axis.title.x = element_text(size = 10)) +
  ylab("Intra-class\ncorrelation") +
  xlab(expression(italic(paste("Adjusted repeatability (r)" %+-% "95% CI", sep = ""))))

# Patchwork plot
laydate_mod_forest_plot_combo <-
  (laydate_mod_forest_plot_fixef / laydate_mod_forest_plot_partR2 / 
     laydate_mod_forest_plot_randef / laydate_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Laydate model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(4, 4, 2.5, 2.5), c('cm', 'cm', 'cm', 'cm')))

laydate_mod_forest_plot_combo

ggsave(plot = laydate_mod_forest_plot_combo,
       filename = "products/figures/svg/laydate_mod_forest_plot.svg",
       width = 4.5,
       height = 8.4, units = "in")

ggsave(plot = laydate_mod_forest_plot_combo,
       filename = "products/figures/jpg/laydate_mod_forest_plot.jpg",
       width = 4.5,
       height = 8.4, units = "in")

#### combo plot with within individual links between sequential nests ----
#### Trend plot of egg volume over season (van de Pol method) ----
# extract the fitted values of the polynomial season effect
eggv_mod_date_fits_vdp_b <- 
  as.data.frame(effect("poly(firstdate_b, 2)", stats_eggv_van_de_pol$mod_poly, 
                       xlevels = list(firstdate_b = seq(min(ceuta_egg_chick_female_data_mc$firstdate_b), 
                                                        max(ceuta_egg_chick_female_data_mc$firstdate_b), 1))))
eggv_mod_date_fits_vdp_w <- 
  as.data.frame(effect("date_deviation_w", stats_eggv_van_de_pol$mod_poly, 
                       xlevels = list(date_deviation_w = seq(min(ceuta_egg_chick_female_data_mc$date_deviation_w), 
                                                             max(ceuta_egg_chick_female_data_mc$date_deviation_w), 1)))) %>%
  mutate(date_deviation_w_adj = date_deviation_w - max(date_deviation_w)/2)

# summary of fitted trend
eggv_mod_date_fits %>% 
  summarise(min_eggv_fit = min(fit),
            max_eggv_fit = max(fit),
            min_eggv_date = jul_lay_date_std_num[which.min(fit)],
            max_eggv_date = jul_lay_date_std_num[which.max(fit)],
            min_eggv_lower = lower[which.min(fit)],
            min_eggv_upper = upper[which.min(fit)],
            max_eggv_lower = lower[which.max(fit)],
            max_eggv_upper = upper[which.max(fit)])

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ceuta_egg_chick_female_data_mc %>% 
  group_by(ID) %>% 
  mutate(avg_egg_volume = mean(volume_cm)) %>% 
  ggplot(.) +
  geom_point(data = ceuta_egg_chick_female_data_mc, alpha = 0.2,
             aes(x = firstdate_b, y = volume_cm), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(aes(x = jul_lay_date_std_num, y = avg_egg_volume, group = ring_year),
            alpha = 0.2) +
  geom_line(data = eggv_mod_date_fits_vdp_b, aes(x = firstdate_b, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits_vdp_b, aes(x = firstdate_b, 
                                                   ymax = upper, ymin = lower),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  geom_line(data = eggv_mod_date_fits_vdp_w, aes(x = date_deviation_w_adj, y = fit),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = eggv_mod_date_fits_vdp_w, aes(x = date_deviation_w_adj,
                                                   ymax = upper, ymin = lower),
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

#### more season combo plot code ----
polyandry_date_mod_plot <- 
  ggplot2::ggplot() + 
  geom_boxplot(data = eggs_mb, aes(x = jul_std_date, y = poly_plot, group = polyandry, 
                                   fill = polyandry), color = "grey50",
               width = 0.05, alpha = 0.5,
               position = position_dodge(width = 0)) +
  geom_jitter(data = eggs_mb, aes(x = jul_std_date, y = poly, group = polyandry, 
                                  fill = polyandry, color = polyandry), 
              height = 0.02, alpha = 0.4, shape = 19) +
  geom_ribbon(data = poly_fits, 
              aes(x = lay_date_std, y = Mean, ymin = Lower, ymax = Upper), 
              fill = "grey50", alpha = 0.25) +
  geom_line(data = poly_fits, 
            aes(x = lay_date_std, y = Mean), lwd = 0.5, colour = "grey20") +
  luke_theme +
  theme(legend.position = c(0.5, -0.04),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 1.2),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(-50, 50)) +
  ylab("Probabilty of polyandry ± 95% CI") +
  # xlab("Standardized lay date (of first nest)") +
  scale_color_manual(values = rev(plot_palette_polyandry),
                     guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                     labels = c("Monogamous", "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry),
                    guide = guide_legend(title.position = "top", nrow = 1, ncol = 2),
                    labels = c("Monogamous", "Polyandrous")) +
  annotate(geom = "text", y = 1.2, x = -48,
           label = "Lay dates for first nests of the season",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# plot the laydate distribution
polyandry_date_dist_plot <-
  eggs_50 %>% 
  dplyr::select(ID, polyandry, jul_std_date) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = jul_std_date, y = 1, group = polyandry)) + 
  geom_violin(data = . %>% dplyr::filter(polyandry == "mono"), 
              alpha = 0.5, fill = plot_palette_polyandry[2], color = "grey50",
              trim = FALSE) +
  geom_violin(data = . %>% dplyr::filter(polyandry == "poly"), 
              alpha = 0.5, fill = plot_palette_polyandry[1], color = "grey50",
              trim = FALSE) +
  
  # alpha = 0.5, aes(x = jul_std_date, y = 1, fill = polyandry)) + 
  # geom_density(alpha = 0.5, aes(jul_std_date, fill = polyandry)) + 
  theme_void() +
  theme(legend.position = c(0.85, 0.2),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5,"cm"),
        panel.grid.major.x = element_line(colour = "grey70", size = 0.25)) +
  scale_x_continuous(limits = c(-50, 50)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  # geom_hline(yintercept = 0, colour = "white", size = 1) +
  annotate(geom = "text", y = 1.7, x = -48,
           label = "Lay date distributions for all nests",
           color = "black", size = 3, fontface = 'italic', hjust = 0) +
  scale_fill_manual(values = plot_palette_polyandry,
                    guide = guide_legend(title.position = "top", nrow = 2),
                    labels = c("Monogamous", "Polyandrous"))# +
# labs(fill = "Number of mates observed")

# plot the quadratic trend, pre- and post-peak trend, and raw data
eggv_date_mod_plot <-
  ggplot() +
  geom_point(data = eggs_50, alpha = 0.4,
             aes(x = jul_std_date, y = eggv/1000), #color = "grey50", shape = 16,
             shape = 19, color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod6a_season_fits, aes(x = jul_std_date, y = fit/1000),
            lwd = 0.5, colour = "grey20") +
  geom_ribbon(data = mod6a_season_fits, aes(x = jul_std_date, ymax = upper/1000, ymin = lower/1000),
              lwd = 1, fill = "grey20", alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Egg volume (cm", ''^{3}, ")" %+-% "95% CI", sep = ""))) +
  xlab("Standardized lay date") +
  scale_x_continuous(limits = c(-50, 50)) +
  theme(legend.position = "none") +
  annotate(geom = "text", y = 9, x = -48,
           label = "Lay dates for all eggs",
           color = "black", size = 3, fontface = 'italic', hjust = 0)# +
# scale_color_manual(values = polyandry_colors)

Season_plot <-
  (polyandry_date_mod_plot / polyandry_date_dist_plot / eggv_date_mod_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = unit(c(7, 2, 7), c('cm', 'cm', 'cm')))

ggsave(plot = Season_plot,
       filename = "products/figures/Fig_3.jpg",
       width = 10,
       height = 24,
       units = "cm", dpi = 600)

#### main dataframe wrangle code ---- 
# check if there are some laydate outliers
lay_dates <- 
  eggs_2006_2020 %>% 
  dplyr::select(ID, jul_lay_date_std_num) %>% 
  distinct()
boxplot.stats(lay_dates$jul_lay_date_std_num)

# check for nests with more than 4 eggs
eggs_2006_2020 %>% 
  group_by(ID) %>%
  dplyr::summarize(n_eggs = n()) %>%
  arrange(desc(n_eggs))

# check for nests with more than 1 female
eggs_2006_2020 %>% 
  group_by(ID) %>%
  summarise(n_rings = n_distinct(ring)) %>%
  arrange(desc(n_rings))

# check for nests with more than 1 egg1, egg2, or egg3
eggs_2006_2020 %>% 
  group_by(ID, egg) %>%
  summarise(n_obs = n()) %>%
  arrange(desc(n_obs))

# check for nests with more than 1 lay date
eggs_2006_2020 %>% 
  group_by(ID) %>%
  summarise(n_dates = n_distinct(jul_lay_date)) %>%
  arrange(desc(n_dates))

#### 2020_D_1 with two females ----
dplyr::filter(nest_caps_F, ID == "2020_D_1") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

# 2020_D_1 nest in birdref
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2020_D_1")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CA3318")

# 2020_D_1 nest in nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_1")

dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_H_8")

# CN0063 female in Captures
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring %in% c("CN0063", "CN0606"))

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("BX.RM|WX.LX"))

#### Remove CN0116 ----
# two females with the same nest ID (CN0116 and CN0118)
dplyr::filter(nest_caps_F, ID == "2018_C_1") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

# CN0118 female in birdref
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2018_C_1")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2018_C_1")

# CN0118 is MX.RW|LX.RX but there is a mistake in 2018 (two rows one for CN0118 and one for CN0116) NEED TO FIX IN DATABASE
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code == "MX.RW|LX.RX" & year == 2018)

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0116" & ID == "2018_C_1")

######### Remove CN0424
# two females with the same nest ID (CN0215 and CN0424)
dplyr::filter(nest_caps_F, ID == "2018_E_301") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

dplyr::filter(nest_caps_F, ring %in% c("CN0215", "CN0424")) %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male) %>% 
  arrange(ID) %>% 
  distinct

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2018_E_301")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2018_E_301")

# CN0118 is MX.RW|LX.RX but there is a mistake in 2018 (two rows one for CN0118 and one for CN0116) NEED TO FIX IN DATABASE
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("LX.RM|BX.RX", "WX.RM|LX.YX"))

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0424")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0215")

########## Remove 2020_C_4 measured by Diego
# two females with the same nest ID (CN0215 and CN0424)
dplyr::filter(nest_caps_F, ID == "2020_C_4") 
dplyr::select(ID, ring, lay_date, no_chicks, length, width, egg)

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_C_4") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ID == "2020_C_4") %>% 
  plover_date_convert(input = "Rdate")

#### Check the super early lay date of 2020_D_101 (looks fine)
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_101") %>% 
  plover_date_convert(input = "Rdate")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_104")

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_104") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ID == "2020_D_104") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("OX.RM|RX.RX", "MX.RY|OX.BX") & year == "2020" & species == "SNPL") %>% 
  plover_date_convert(input = "Rdate")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_12")

eggdf %>%
  dplyr::filter(ID == "2020_D_12" & length %in% c(31.5, 31.4)) %>% 
  dplyr::filter(ID == "2020_D_12")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_201")

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_201") %>% 
  plover_date_convert(input = "Rdate")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("OX.RM|GX.GX", "WX.RM|GX.YX") & year == "2020" & species == "SNPL") %>% 
  plover_date_convert(input = "Rdate")

eggdf_2006_2020_cleaned %>% 
  dplyr::filter(ring %in% c("CN0155", "CN0379", "CN0478"))

dbReadTable(CeutaCLOSED,"Captures") %>% 
  dplyr::filter(ring %in% c("CN0155", "CN0379", "CN0478") & year == "2019")

eggdf_2006_2020 %>% 
  dplyr::select(ring, ID, est_age) %>% 
  distinct()

boxplot(y = eggs_2006_2020$volume, x = eggs_2006_2020$year, boxwex = 0.1)
boxplot.stats(eggs_2006_2020$volume)$out

eggs_2006_2020 %>% 
  group_by(year) %>%
  dplyr::filter(!volume %in% boxplot.stats(volume)$out) %>%
  ggplot(., aes(year, volume)) +
  geom_boxplot()

eggs_2006_2020 %>% 
  ggplot(., aes(year, volume)) +
  geom_jitter()

eggs_2006_2020 %>% 
  # group_by(year) %>%
  # dplyr::filter(!eggv %in% boxplot.stats(eggv)$out) %>%
  ggplot(., aes(year, volume)) +
  geom_boxplot()

#### peak-performace of laydate ----
# create data that contains all factor levels and covariate means to use for
# calculating predictions
new_data <- expand.grid(est_age_t_deviation = seq(0:max(first_nests_age_data$est_age_t_deviation)) - 1,
                        PrePeak_mod = c("1","0"),
                        first_age_t = mean(first_nests_age_data$first_age_t),
                        last_age_t = mean(first_nests_age_data$last_age_t),
                        avg_ad_tarsi = mean(first_nests_age_data$avg_ad_tarsi),
                        age_first_cap = c("A", "J"))

# create an empty list to store for-loop output
bs.predictions <- list(
  
  # peak values from previous simulation
  peaks = xpeak_mod,
  
  # age-specific predictions
  predictions = matrix(ncol = n_sim, nrow = nrow(new_data)),
  
  # slope of pre-peak effect
  PrePeak_age_effect = matrix(ncol = n_sim, nrow = 1),
  
  # slope of post-peak effect
  PostPeak_age_effect = matrix(ncol = n_sim, nrow = 1))

i=2
# 
# first_nests_age_data %>% 
#   arrange(desc(est_age_t_deviation))

# for-loop to run peak analysis on all simulated posteriors above
for (i in 1:n_sim) {
  
  # start loop with mod_laydate_I_peak as NULL and attempt as 0
  mod_laydate_I_peak <- NULL
  attempt <- 0
  
  # store model output and predictions only if mod_laydate_I_peak converged (i.e., is not
  # NULL) and it's less than 100 attempts
  while(is.null(mod_laydate_I_peak) && attempt <= 100) {
    
    # next attempt
    attempt <- attempt + 1
    
    # set peak based on the mean estimate from the previous simulation
    first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation < (ceiling(bs.predictions$peaks[i]) + 1)] <- "0"
    first_nests_age_data$PrePeak_mod[first_nests_age_data$est_age_t_deviation > ceiling(bs.predictions$peaks[i])] <- "1"
    
    sum(as.numeric(first_nests_age_data$PrePeak_mod))
    
    try(
      # run peak analysis (i.e., now the quadratic effect is broken up into two
      # pieces reflective of the pre- and post-peak sections of the curve)
      mod_laydate_I_peak <-
        lmer(first_laydate ~ PrePeak_mod + est_age_t_deviation * PrePeak_mod + 
               first_age_t + last_age_t + avg_ad_tarsi + age_first_cap +
               (1|ring) + (1|year),
             data = filter(first_nests_age_data, year != "2006"))
    )
    
    try(
      # calculate predictions from model
      bs.predictions$predictions[, i] <- 
        predict(mod_laydate_I_peak, new_data, re.form = NA)
    )
    
  }
  
  # calculate the slope of the pre-peak age effect (i.e., because pre-peak is 
  # set as the baseline level of the factor, this is simply the baseline age 
  # slope)
  bs.predictions$PrePeak_age_effect[i] <- 
    ifelse("PrePeak_mod1:est_age_t_deviation" %in% 
             row.names(summary(mod_laydate_I_peak)$coefficients), 
           summary(mod_laydate_I_peak)$coefficients["est_age_t_deviation","Estimate"],
           NA)
  
  # calculate the slope of the post-peak age effect (i.e., because pre-peak is 
  # set as the baseline level of the factor, this is calculated as the baseline 
  # age slope plus the interaction slope)
  bs.predictions$PostPeak_age_effect[i] <- 
    ifelse("PrePeak_mod1:est_age_t_deviation" %in% 
             row.names(summary(mod_laydate_I_peak)$coefficients), 
           summary(mod_laydate_I_peak)$coefficients["est_age_t_deviation","Estimate"] + 
             summary(mod_laydate_I_peak)$coefficients["PrePeak_mod1:est_age_t_deviation","Estimate"],
           NA)
  
}

# Retrieve pre-peak age estimate (mean, credible intervals)
coefPrePeakAgeTable <- 
  data.frame(coefName = "Pre-peak age effect",
             mean_estimate = mean(bs.predictions$PrePeak_age_effect, 
                                  na.rm = TRUE),
             lower95 = quantile(bs.predictions$PrePeak_age_effect, 
                                prob = c(0.025), na.rm = TRUE),
             upper95 = quantile(bs.predictions$PrePeak_age_effect, 
                                prob = c(0.975), na.rm = TRUE), 
             row.names = 1)

# Retrieve post-peak age estimate (mean, credible intervals)
coefPostPeakAgeTable <- 
  data.frame(coefName = "Post-peak age effect",
             mean_estimate = mean(bs.predictions$PostPeak_age_effect, 
                                  na.rm = TRUE),
             lower95 = quantile(bs.predictions$PostPeak_age_effect, 
                                prob = c(0.025), na.rm = TRUE),
             upper95 = quantile(bs.predictions$PostPeak_age_effect, 
                                prob = c(0.975), na.rm = TRUE), 
             row.names = 1)
