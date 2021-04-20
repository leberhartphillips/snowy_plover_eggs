#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")

#### Results ----
load("output/stats_date_age_tarsi.rds")
load("data/ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, jul_lay_date_std_num, est_age_trans, year,
                firstage, lastage, nest_order, n_years_obs, avg_ad_tarsi) %>% 
  dplyr::filter(nest_order == 1) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_trans))

#### Quick model diagnostics ----
plot(allEffects(mod_date_age_tarsi))
coefplot2(mod_date_age_tarsi)
summary(glht(mod_date_age_tarsi))
summary(mod_date_age_tarsi)

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

#### Plot of trend ----
# extract fitted values
date_age_mod_fits <- 
  as.data.frame(effect(term = "poly(est_age, 2)", mod = stats_date_age_tarsi$mod, 
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
  data.frame(comp_name = c("Intercept",
                           "Linear age",
                           "Quadratic age",
                           "First age",
                           "Last age",
                           "Tarsus",
                           "Total Marginal \U1D479\U00B2",
                           "Senescence",
                           "First age",
                           "Last age",
                           "Tarsus",
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

stats_date_age_tarsi$mod

fixefTable <- 
  stats_date_age_tarsi$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed")

R2Table <- 
  bind_rows(stats_date_age_tarsi$partR2m$R2,
            stats_date_age_tarsi$partR2c$R2[1,]) %>% 
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

ranefTable <- 
  stats_date_age_tarsi$tidy %>% 
  dplyr::filter(effect == "ran_pars") %>% 
  dplyr::select(group, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "rand") %>% 
  rename(term = group) %>% 
  mutate(estimate = estimate^2,
         conf.high = conf.high^2,
         conf.low = conf.low^2)

coefRptTable <- 
  stats_date_age_tarsi$rptR$R_boot %>% 
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
  bind_rows(fixefTable,
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
         effect = c(rep("Fixed effects \U1D6FD (days in season)", nrow(fixefTable)),
                    rep("Partitioned \U1D479\U00B2", nrow(R2Table)),
                    rep("Random effects \U1D70E\U00B2", nrow(ranefTable)),
                    rep("Repeatability \U1D479", nrow(coefRptTable)),
                    rep("Sample sizes \U1D45B", nrow(sample_sizes)))) %>%
  dplyr::select(effect, everything())

# re-organize model components for table
allCoefs_mod <-
  allCoefs_mod[c(1, 6, 2:5, 7, 12, 11, 8:10, 13:21), ]

laydate_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = "",
             estimate = "Parameter estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:18,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 19:21,
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

laydate_mod_table %>% 
  gtsave("laydate_mod_table.rtf", path = "products/tables/")
laydate_mod_table %>% 
  gtsave("laydate_mod_table.png", path = "products/tables/")

image_read(path = "results/tables/table_S2.png")
