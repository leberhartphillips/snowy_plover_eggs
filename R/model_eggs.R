# Model seasonal- and age-dependent variation in egg volume

# Product: stats and figure of the chick egg volume ~ senescence + season model

#### Libraries and data ----
source("R/001_libraries.R")
source("R/002_functions.R")

load("data/raw/ceuta_egg_chick_female_data.rds")

#### Egg dimension plots ----
egg_widths_plot <- 
  ceuta_egg_chick_female_data %>% 
  ggplot() + 
  geom_boxplot(aes(x = width/10, y = 465), 
               fill = brewer.pal(8, "Set1")[c(2)], 
               color = brewer.pal(8, "Set1")[c(2)],
               width = 18, alpha = 0.5) +
  geom_jitter(aes(x = width/10, y = 428), 
              fill = brewer.pal(8, "Set1")[c(2)], 
              color = brewer.pal(8, "Set1")[c(2)],
              height = 8, alpha = 0.1) +
  geom_histogram(alpha = 0.5, aes(width/10), 
                 fill = brewer.pal(8, "Set1")[c(2)], 
                 # color = "white",
                 binwidth = 0.02) +
  luke_theme +
  theme(axis.title.x = element_text(hjust = 0.4),
        panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0.5, "cm")) +
  ylab("Number of eggs") +
  xlab("Egg width (cm)") +
  scale_x_reverse(limits = c(25/10, 20/10),
                  breaks = c(seq(from = 2, to = 2.4, by = 0.1))) +
  scale_y_continuous(limits = c(0, 475),
                     breaks = c(0, 100, 200, 300), position = "right") +
  coord_flip()

egg_lengths_plot <- 
  ceuta_egg_chick_female_data %>% 
  ggplot() + 
  geom_boxplot(aes(x = length/10, y = 275), 
               fill = brewer.pal(8, "Set1")[c(2)], 
               color = brewer.pal(8, "Set1")[c(2)],
               width = 10, alpha = 0.5) +
  geom_jitter(aes(x = length/10, y = 255), 
              fill = brewer.pal(8, "Set1")[c(2)], 
              color = brewer.pal(8, "Set1")[c(2)],
              height = 4, alpha = 0.1) +
  geom_histogram(alpha = 0.5, aes(length/10), 
                 fill = brewer.pal(8, "Set1")[c(2)], 
                 # color = "white",
                 binwidth = 0.02) +
  luke_theme +
  theme(axis.title.y = element_text(hjust = 0.7),
        axis.title.x = element_text(vjust = -1),
        panel.border = element_blank(),
        plot.margin = margin(0, 0, 0.5, 0, "cm")) +
  ylab("Number of eggs") +
  xlab("Egg length (cm)") +
  scale_x_continuous(limits = c(27/10, 35/10),
                     breaks = c(2.8, 3, 3.2, 3.4)) +
  scale_y_reverse(limits = c(280, 0),
                  breaks = c(0, 50, 100, 150, 200))

#### Modeling egg volume ----
eggv_age_date_tarsi_mod <- 
  lmer(volume_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
         poly(jul_lay_date_std_num, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

eggv_age_date_tarsi_mod_tidy <-
  tidy(eggv_age_date_tarsi_mod, conf.int = TRUE, conf.method = "boot", nsim = 1000)
save(eggv_age_date_tarsi_mod_tidy, file = "data/out/eggv_age_date_tarsi_mod_tidy.rds")

eggv_age_date_tarsi_mod_R2 <-
  partR2(eggv_age_date_tarsi_mod,
         partvars = c("poly(est_age, 2)",
                      "poly(jul_lay_date_std_num, 2)",
                      "firstage",
                      "lastage",
                      "avg_ad_tarsi"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)
save(eggv_age_date_tarsi_mod_R2, file = "data/out/eggv_age_date_tarsi_mod_R2.rds")

eggv_age_date_tarsi_mod_rpt <-
  rpt(volume_cm ~ poly(est_age, 2) + firstage + lastage + avg_ad_tarsi +
        poly(jul_lay_date_std_num, 2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ID", "ring", "year", "Fixed"),
      data = ceuta_egg_chick_female_data,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = FALSE, ncores = 4, parallel = TRUE)

save(eggv_age_date_tarsi_mod_rpt, file = "data/out/eggv_age_date_tarsi_mod_rpt.rds")

# model summary a diagnostics
summary(eggv_age_date_tarsi_mod)
plot(allEffects(eggv_age_date_tarsi_mod))
coefplot2(eggv_age_date_tarsi_mod)
summary(glht(eggv_age_date_tarsi_mod))

###
eggv_age_date_mod <- 
  lmer(volume_cm ~ poly(est_age, 2) + firstage + lastage + 
         poly(jul_lay_date_std_num, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = ceuta_egg_chick_female_data)

# model summary a diagnostics
summary(eggv_age_date_mod)
plot(allEffects(eggv_age_date_mod))
coefplot2(eggv_age_date_mod)
summary(glht(eggv_age_date_mod))

eggv_mod_tidy <-
  tidy(eggv_age_date_mod, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# save(eggv_mod_tidy, file = "data/out/eggv_mod_tidy.rds")

eggv_mod_R2 <-
  partR2(eggv_age_date_mod,
         partvars = c("poly(est_age, 2)",
                      "poly(jul_std_date, 2)",
                      "firstage",
                      "lastage"),
         R2_type = "marginal",
         nboot = 1000,
         CI = 0.95,
         max_level = 1)

# save(eggv_mod_R2, file = "results/eggv_mod_R2.rds")

eggv_mod_rpt <-
  rpt(eggv ~ poly(est_age, 2) + firstage + lastage +
        poly(jul_std_date, 2) +
        (1|ID) + (1|ring) + (1|year),
      grname = c("ring", "ID", "year", "Fixed"),
      data = eggdf_2006_2020,
      datatype = "Gaussian",
      nboot = 1000, npermut = 1000, ratio = TRUE,
      adjusted = TRUE, ncores = 4, parallel = TRUE)

# save(eggv_mod_rpt, file = "results/eggv_mod_rpt.rds")

#### Modeling egg width ----
eggw_mod <-
  lmer(width ~ poly(est_age, 2) + firstage + lastage +
         poly(jul_std_date, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = eggdf_2006_2020)

# eggw_mod_tidy <- 
#   tidy(eggw_mod, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# save(eggw_mod_tidy, file = "results/eggw_mod_tidy.rds")

# eggw_mod_R2 <- 
#   partR2(eggw_mod,  
#          partvars = c("poly(est_age, 2)", 
#                       "poly(jul_std_date, 2)", 
#                       "firstage", 
#                       "lastage"),
#          R2_type = "marginal", 
#          nboot = 1000, 
#          CI = 0.95, 
#          max_level = 1)

# save(eggw_mod_R2, file = "results/eggw_mod_R2.rds")

# eggw_mod_rpt <-
#   rpt(width ~ poly(est_age, 2) + firstage + lastage +
#         poly(jul_std_date, 2) +
#         (1|ID) + (1|ring) + (1|year),
#       grname = c("ring", "ID", "year", "Fixed"), 
#       data = eggdf_2006_2020, 
#       datatype = "Gaussian", 
#       nboot = 1000, npermut = 1000, ratio = TRUE,
#       adjusted = TRUE, ncores = 4, parallel = TRUE)

# save(eggw_mod_rpt, file = "results/eggw_mod_rpt.rds")

#### Modeling egg length ----
eggl_mod <- 
  lmer(length ~ poly(est_age, 2) + firstage + lastage + 
         poly(jul_std_date, 2) +
         (1|ID) + (1|ring) + (1|year),
       data = eggdf_2006_2020)

# eggl_mod_tidy <- 
#   tidy(eggl_mod, conf.int = TRUE, conf.method = "boot", nsim = 1000)

# save(eggl_mod_tidy, file = "results/eggl_mod_tidy.rds")

# eggl_mod_R2 <- 
#   partR2(eggl_mod,  
#          partvars = c("poly(est_age, 2)", 
#                       "poly(jul_std_date, 2)", 
#                       "firstage", 
#                       "lastage"),
#          R2_type = "marginal", 
#          nboot = 1000, 
#          CI = 0.95, 
#          max_level = 1)

# save(eggl_mod_R2, file = "results/eggl_mod_R2.rds")

# eggl_mod_rpt <-
#   rpt(length ~ poly(est_age, 2) + firstage + lastage +
#         poly(jul_std_date, 2) +
#         (1|ID) + (1|ring) + (1|year),
#       grname = c("ring", "ID", "year", "Fixed"),
#       data = eggdf_2006_2020,
#       datatype = "Gaussian",
#       nboot = 1000, npermut = 1000, ratio = TRUE,
#       adjusted = TRUE, ncores = 4, parallel = TRUE)

# save(eggl_mod_rpt, file = "results/eggl_mod_rpt.rds")

#### Repeatabilities of egg morphometrics (Table) ----
## ---- load_rpt_out --------
load("results/eggv_mod_rpt.rds")
load("results/eggw_mod_rpt.rds")
load("results/eggl_mod_rpt.rds")

eggv_mod_rpt_R <- 
  cbind(t(eggv_mod_rpt$R), eggv_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggv_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Volume")

eggw_mod_rpt_R <- 
  cbind(t(eggw_mod_rpt$R), eggw_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggw_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Width")

eggl_mod_rpt_R <- 
  cbind(t(eggl_mod_rpt$R), eggl_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggl_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Length")

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

#### Forest Plots of Marginal R2 egg morphometrics models ####
## ---- load_R2_out --------
load("results/eggv_mod_R2.rds")
load("results/eggw_mod_R2.rds")
load("results/eggl_mod_R2.rds")

## ---- eggv model ---------
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

## ---- eggl model ---------
eggl_mod_out = eggl_mod_R2[["R2"]]
col_all <- "#2E3440"

eggl_mod_out[eggl_mod_out$term == "Full", 1] <- "Model"
names(eggl_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggl_mod_out <- 
  eggl_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggl_mod_out$combs <- factor(eggl_mod_out$combs, levels = rev(eggl_mod_out$combs))

eggl_mod_R2_plot <- 
  eggl_mod_R2[["R2"]] %>% 
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
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab(expression(paste("R", ''^{2}, sep = ""))) +
  ylab("Length model") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

## ---- eggw model ---------
eggw_mod_out = eggw_mod_R2[["R2"]]
col_all <- "#2E3440"

eggw_mod_out[eggw_mod_out$term == "Full", 1] <- "Model"
names(eggw_mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
eggw_mod_out <- 
  eggw_mod_out %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)", "Quadratic age", 
                        ifelse(combs == "poly(jul_std_date, 2)", "Quadratic lay date",
                               ifelse(combs == "firstage", "First age breeding",
                                      ifelse(combs == "lastage", "Last age breeding", "Model")))))
eggw_mod_out$combs <- factor(eggw_mod_out$combs, levels = rev(eggw_mod_out$combs))

eggw_mod_R2_plot <- 
  eggw_mod_R2[["R2"]] %>% 
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
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11),
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Variance explained") +
  ylab("Width model")  +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(0, 0.11))

## ---- combo R2 plot --------
egg_shape_R2_plot <- 
  eggv_mod_R2_plot / eggl_mod_R2_plot / eggw_mod_R2_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_R2_plot

#### Forest Plots of egg morphometrics model estimates ####
## ---- load_R2_out --------
load("results/eggv_mod_R2.rds")
load("results/eggw_mod_R2.rds")
load("results/eggl_mod_R2.rds")

## ---- eggv model ---------
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

## ---- eggl model ---------
eggl_mod_out_ests = eggl_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggl_mod_out_ests[eggl_mod_out_ests$term == "Full", 1] <- "Model"
names(eggl_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggl_mod_out_ests <- 
  eggl_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggl_mod_out_ests$combs <- factor(eggl_mod_out_ests$combs, levels = rev(eggl_mod_out_ests$combs))

eggl_mod_ests_plot <- 
  eggl_mod_R2[["Ests"]] %>% 
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
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11)) + 
  xlab("Length (mm)") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
# scale_x_continuous(limits = c(0, 0.11))

## ---- eggw model ---------
eggw_mod_out_ests = eggw_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggw_mod_out_ests[eggw_mod_out_ests$term == "Full", 1] <- "Model"
names(eggw_mod_out_ests) <- c("combs", "pe", "CI_lower", "CI_upper")
eggw_mod_out_ests <- 
  eggw_mod_out_ests %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggw_mod_out_ests$combs <- factor(eggw_mod_out_ests$combs, levels = rev(eggw_mod_out_ests$combs))

eggw_mod_ests_plot <- 
  eggw_mod_R2[["Ests"]] %>% 
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
        axis.title.x = element_text(margin = margin(t = 8), color = col_all, size = 11)) + 
  xlab("Width (mm)") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) #+
# scale_x_continuous(limits = c(0, 0.11))

## ---- combo ests plot --------
egg_shape_ests_plot <- 
  eggv_mod_ests_plot / eggl_mod_ests_plot / eggw_mod_ests_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_ests_plot

#### Forest Plots of egg morphometrics beta weights (standardized estimates) ####
## ---- load_R2_out --------
load("results/eggv_mod_R2.rds")
load("results/eggw_mod_R2.rds")
load("results/eggl_mod_R2.rds")

## ---- eggv model ---------
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

## ---- eggl model ---------
eggl_mod_out_BW = eggl_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggl_mod_out_BW[eggl_mod_out_BW$term == "Full", 1] <- "Model"
names(eggl_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggl_mod_out_BW <- 
  eggl_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggl_mod_out_BW$combs <- factor(eggl_mod_out_BW$combs, levels = rev(eggl_mod_out_BW$combs))

eggl_mod_BW_plot <- 
  eggl_mod_R2[["BW"]] %>% 
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
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Standardized\nlength") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
  scale_x_continuous(limits = c(-0.17, 0.22))

## ---- eggw model ---------
eggw_mod_out_BW = eggw_mod_R2[["Ests"]]
col_all <- "#2E3440"

eggw_mod_out_BW[eggw_mod_out_BW$term == "Full", 1] <- "Model"
names(eggw_mod_out_BW) <- c("combs", "pe", "CI_lower", "CI_upper")
eggw_mod_out_BW <- 
  eggw_mod_out_BW %>% 
  mutate(combs = ifelse(combs == "poly(est_age, 2)1", "Linear age", 
                        ifelse(combs == "poly(est_age, 2)2", "Quadratic age", 
                               ifelse(combs == "poly(jul_std_date, 2)1", "Linear lay date",
                                      ifelse(combs == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                             ifelse(combs == "firstage", "First-age breeding",
                                                    ifelse(combs == "lastage", "Last-age breeding", "Model")))))))
eggw_mod_out_BW$combs <- factor(eggw_mod_out_BW$combs, levels = rev(eggw_mod_out_BW$combs))

eggw_mod_BW_plot <- 
  eggw_mod_R2[["BW"]] %>% 
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
        panel.grid.major.x = element_line(colour = "grey70", size=0.25)) + 
  xlab("Standardized\nwidth") +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5) +
scale_x_continuous(limits = c(-0.17, 0.22))

## ---- combo BW plot --------
egg_shape_BW_plot <- 
  eggv_mod_BW_plot / eggl_mod_BW_plot / eggw_mod_BW_plot + 
  plot_annotation(tag_levels = "A")
egg_shape_BW_plot

#### Full forest plot of model results ---- 
egg_shape_plot <- 
  (eggv_mod_R2_plot + eggv_mod_ests_plot + eggv_mod_BW_plot) /
  (eggl_mod_R2_plot + eggl_mod_ests_plot + eggl_mod_BW_plot) /
  (eggw_mod_R2_plot + eggw_mod_ests_plot + eggw_mod_BW_plot) +
  plot_annotation(tag_levels = "A")
egg_shape_plot

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

#### JUNK CODE ####
# plot with the polynomial term broken into two 
# eggv_mod_R2_plot <- 
eggv_mod_R2[["BW"]] %>% 
  mutate(term = ifelse(term == "poly(est_age, 2)1", "Linear age", 
                       ifelse(term == "poly(est_age, 2)2", "Quadratic age", 
                              ifelse(term == "poly(jul_std_date, 2)1", "Linear lay date",
                                     ifelse(term == "poly(jul_std_date, 2)2", "Quadratic lay date",
                                            ifelse(term == "firstage", "First-age breeding",
                                                   ifelse(term == "lastage", "Last-age breeding", "Model"))))))) %>% 
  dplyr::select(.data$term, .data$estimate, .data$CI_lower, .data$CI_upper) %>% 
  # dplyr::rename(Predictor = .data$term, 
  #               BW = .data$estimate) %>% 
  dplyr::filter(!(.data$term == "(Intercept)")) %>%
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
        axis.title.x = element_text(margin = margin(t = 8), color = col_all)) + 
  xlab(expression(paste("R", ''^{2}, "", sep = ""))) +
  geom_errorbarh(alpha = 1, color = col_all, height = 0, size = 0.5) +
  geom_point(size = 3, shape = 21, 
             fill = "#ECEFF4", col = col_all, alpha = 1, stroke = 0.5)


p1 <- forestplot(eggv_mod_R2, type = "R2", text_size = 10)
p2 <- forestplot(eggv_mod_R2, type = "Ests", text_size = 10)
p3 <- forestplot(eggv_mod_R2, type = "SC", text_size = 10)
p4 <- forestplot(eggv_mod_R2, type = "BW", text_size = 10)
(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

eggv_mod_rpt_R <- 
  cbind(t(eggv_mod_rpt$R), eggv_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggv_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Volume")

eggw_mod_rpt_R <- 
  cbind(t(eggw_mod_rpt$R), eggw_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggw_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Width")

eggl_mod_rpt_R <- 
  cbind(t(eggl_mod_rpt$R), eggl_mod_rpt$CI_emp) %>% 
  mutate(group = row.names(.)) %>% 
  rename(mean_estimate = `t(eggl_mod_rpt$R)`,
         lower95 = `2.5%`,
         upper95 = `97.5%`) %>% 
  mutate(trait = "Length")

## ---- load_tidy_out --------
load("results/eggv_mod_tidy.rds")
load("results/eggw_mod_tidy.rds")
load("results/eggl_mod_tidy.rds")