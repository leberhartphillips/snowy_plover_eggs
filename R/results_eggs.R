#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results and data----
load("output/stats_eggv_mod.rds")
load("data/ceuta_egg_chick_female_data.rds")

#### Table of effect sizes (van de Pol method) ----
# Retrieve sample sizes
sample_sizes <-
  ceuta_egg_chick_female_data %>% 
  ungroup() %>% 
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
  data.frame(comp_name = c("Within ind. linear age",
                           "Within ind. quadratic age",
                           "Between ind. first breeding age",
                           "Between ind. last breeding age",
                           "Mother tarsus length",
                           "Within ind. lay date",
                           "Between ind. linear lay date",
                           "Between ind. quadratic lay date",
                           "Total Marginal \U1D479\U00B2",
                           "Senescence",
                           "Selective appearance",
                           "Selective disappearance", 
                           "Between ind. seasonality",
                           "Within ind. seasonality",
                           "Mother tarsus length",
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

# Fixed effect sizes (non-standardized)
fixefTable <- 
  stats_eggv_van_de_pol$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed")

# Fixed effect sizes (standardized)
fixef_bw_Table <- 
  stats_eggv_van_de_pol$partR2m$BW %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
R2Table <- 
  bind_rows(stats_eggv_van_de_pol$partR2m$R2,
            stats_eggv_van_de_pol$partR2c$R2[1,]) %>% 
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Random effects variances
ranefTable <- 
  stats_eggv_van_de_pol$tidy %>% 
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
  stats_eggv_van_de_pol$rptR$R_boot %>% 
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
  allCoefs_mod[c(5, 1:4, 6:9, 16, 15, 10:14, 17:28), ]

# draw gt table
eggv_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Egg volume</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:24,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 25:28,
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

# export table to disk
eggv_mod_table %>%
  gtsave("eggv_mod_table.rtf", path = "products/tables/rtf/")

eggv_mod_table %>%
  gtsave("eggv_mod_table.png", path = "products/tables/png/")

#### Forest plot of results ----
# Standardized fixed effects
eggv_mod_forest_plot_fixef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Fixed") & 
           term != "(Intercept)") %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Between ind. quadratic lay date", 
                                 "Between ind. linear lay date", 
                                 "Within ind. lay date",
                                 "Between ind. last breeding age", "Between ind. first breeding age", 
                                 "Within ind. quadratic age", "Within ind. linear age",
                                 "Mother tarsus length")) %>%
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
eggv_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Between ind. seasonality",
                                 "Within ind. seasonality",
                                 "Selective disappearance",
                                 "Selective appearance",
                                 "Senescence",
                                 "Mother tarsus length",
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
  scale_y_discrete(labels = c("Between ind. seasonality" = expression("Between ind. seasonality"),
                              "Within ind. seasonality" = expression("Within ind. seasonality"),
                              "Selective disappearance" = expression("Selective disappearance"),
                              "Selective appearance" = expression("Selective appearance"),
                              "Senescence" = expression("Senescence"),
                              "Mother tarsus length" = expression("Mother tarsus length"),
                              "Total Marginal \U1D479\U00B2" = expression(paste("Total marginal ", italic("R"), ''^{2}, sep = "")))) +
  ylab(expression(paste("Semi-partial ", italic("R"),''^{2}, sep = ""))) +
  xlab(expression(italic(paste("Variance explained (R", ''^{2}, ")" %+-% "95% CI", sep = ""))))

# Random effect variances
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

# Adjusted repeatabilities
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

# Patchwork plot
eggv_mod_forest_plot_combo <-
  (eggv_mod_forest_plot_fixef / eggv_mod_forest_plot_partR2 / 
     # eggv_mod_forest_plot_randef / 
     eggv_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Egg volume model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(4.5, 4, 
                               # 2.5, 
                               2.5), c('cm', 'cm', 
                                       # 'cm', 
                                       'cm')))

eggv_mod_forest_plot_combo

# export plot to disk
ggsave(plot = eggv_mod_forest_plot_combo,
       filename = "products/figures/jpg/eggv_mod_forest.jpg",
       width = 5,
       height = 9, units = "in")

ggsave(plot = eggv_mod_forest_plot_combo,
       filename = "products/figures/svg/eggv_mod_forest.svg",
       width = 5,
       height = 9, units = "in")
