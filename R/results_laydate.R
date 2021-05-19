#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results ----
load("output/stats_laydate_mod2.rds")
load("data/ceuta_egg_chick_female_data.rds")

#### Data wrangle ----
# subset to nest level and first nest attempts of the season for each female
first_nests_age_data <-
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, ID, first_laydate, est_age_t_deviation, year,
                first_age_t, last_age_t, n_years_obs, avg_ad_tarsi,
                age_first_cap, nest_order, est_age_t) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(est_age_t_deviation) & 
                  nest_order == 1 &
                  year != "2006") %>%
  mutate(age_first_cap_plot = ifelse(age_first_cap == "J", 2.2, 0.8))

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
  data.frame(comp_name = c("Within ind. linear age",
                           "Within ind. quadratic age",
                           "Between ind. first breeding age",
                           "Between ind. last breeding age",
                           "Mother tarsus length",
                           "Local recruit",
                           "Total Marginal \U1D479\U00B2",
                           "Senescence",
                           "Selective appearance",
                           "Selective disappearance",
                           "Mother tarsus length",
                           "Local recruit",
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


str(model_parameters(stats_laydate_mod$mod_poly, standardize = "refit"))
model_parameters(stats_laydate_mod$mod_poly, standardize = "refit")$Coefficient
model_parameters(stats_laydate_mod$mod_I, standardize = "refit")$Coefficient
model_parameters(stats_laydate_mod$mod_I, standardize = "refit")$CI_low
model_parameters(stats_laydate_mod$mod_I, standardize = "refit")$CI_high

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
  # dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
fixef_bw_Table <-
  model_parameters(stats_laydate_mod$mod_I, standardize = "refit") %>%
  select(Parameter, Coefficient, CI_low, CI_high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_low,
         conf.high = CI_high,
         term = Parameter,
         estimate = Coefficient) %>% 
  filter(term != "(Intercept)")

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
  allCoefs_mod[c(5, 6, 1:4, 7, 13, 11, 12, 8:10, 14:22), ]

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
             rows = 1:19,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 20:22,
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
# Standardized fixed effects
laydate_mod_forest_plot_fixef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Fixed") & 
           term != "(Intercept)") %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Between ind. last breeding age", "Between ind. first breeding age", 
                                 "Within ind. quadratic age", "Within ind. linear age",
                                 "Mother tarsus length", "Local recruit")) %>%
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
                                 "Selective disappearance",
                                 "Selective appearance",
                                 "Senescence",
                                 "Mother tarsus length", "Local recruit",
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
    "Selective disappearance" = expression("Selective disappearance"),
    "Selective appearance" = expression("Selective appearance"),
    "Senescence" = expression("Senescence"),
    "Mother tarsus length" = expression("Mother tarsus length"),
    "Recruit status" = expression("Recruit status"),
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
     # laydate_mod_forest_plot_randef / 
     laydate_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Lay date model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(4, 4, 
                               # 2.5, 
                               2.5), c('cm', 'cm', 
                                       # 'cm',
                                       'cm')))

laydate_mod_forest_plot_combo

# export plot to disk
ggsave(plot = laydate_mod_forest_plot_combo,
       filename = "products/figures/svg/laydate_mod_forest_plot.svg",
       width = 5,
       height = 9, units = "in")

ggsave(plot = laydate_mod_forest_plot_combo,
       filename = "products/figures/jpg/laydate_mod_forest_plot.jpg",
       width = 5,
       height = 9, units = "in")
