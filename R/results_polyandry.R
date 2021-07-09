#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

# load the saved results
load("output/stats_polyandry_age_mod.rds")
ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::filter(nest_order == 1) %>% 
  dplyr::select(polyandry, year, ring, first_laydate, n_nests, ID) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

first_nests_data

#### Find peaks for bimodal laydate distribution ---- 
set.seed(42)

m1 <- FLXMRglm(family = "gaussian")
m2 <- FLXMRglm(family = "gaussian")

poly_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(polyandry == "poly") %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring) %>%
  distinct()

polyd <- density(poly_data$jul_lay_date_std_num)

polyd_fit <- flexmix(jul_lay_date_std_num ~ 1, data = poly_data, k = 2, model = list(m1, m2))
poly_peak1 <- modeltools::parameters(polyd_fit, component=1)[[1]]
poly_peak2 <- modeltools::parameters(polyd_fit, component=2)[[1]]

plot(polyd)
abline(v=poly_peak1[[1]], lty=2, col='blue')
abline(v=poly_peak2[[1]], lty=2, col='red')

mono_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(polyandry == "mono") %>% 
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring) %>%
  distinct()

monod <- density(mono_data$jul_lay_date_std_num)

monod_fit <- flexmix(jul_lay_date_std_num ~ 1, data = mono_data, k = 2, model = list(m1, m2))
mono_peak1 <- modeltools::parameters(monod_fit, component=1)[[1]]
mono_peak2 <- modeltools::parameters(monod_fit, component=2)[[1]]

plot(monod)
abline(v=mono_peak1[[1]], lty=2, col='blue')
abline(v=mono_peak2[[1]], lty=2, col='red')

poly_peak2[[1]] - mono_peak1[[1]]
poly_peak1[[1]] - mono_peak1[[1]]

#### Table of effect sizes ----
# Retrieve sample sizes
sample_sizes <-
  first_nests_data %>% 
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
  data.frame(comp_name = c("First nest lay date",
                           "Age since first breeding",
                           "First breeding age",
                           "Total Marginal \U1D479\U00B2",
                           "First nest lay date",
                           "Age since first breeding",
                           "First breeding age",
                           "Total Conditional \U1D479\U00B2",
                           "Individual",
                           "Year",
                           "Individual",
                           "Year",
                           "Years",
                           "Individuals",
                           "Observations (i.e., Nests)"))

# Fixed effect sizes (non-standardized)
fixefTable <- 
  stats_polyandry_age_mod$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed") %>% 
  mutate_at(c("estimate", "conf.low", "conf.high"), invlogit)

# Fixed effect sizes (standardized)
fixef_bw_Table <- 
  stats_polyandry_age_mod$partR2m$BW %>% 
  # dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
ranefTable <- 
  stats_polyandry_age_mod$tidy %>% 
  dplyr::filter(effect == "ran_pars") %>% 
  dplyr::select(group, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "rand") %>% 
  rename(term = group) %>% 
  mutate(estimate = estimate^2,
         conf.high = conf.high^2,
         conf.low = conf.low^2)

# Random effects variances
R2Table <- 
  bind_rows(stats_polyandry_age_mod$partR2m$R2,
            stats_polyandry_age_mod$partR2c$R2[1,]) %>%   
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Adjusted repeatabilities
coefRptTable <- 
  stats_polyandry_age_mod$rptR$R["R_org", ] %>% 
  dplyr::select(-Fixed) %>%
  t() %>% 
  as.data.frame() %>% 
  bind_cols(stats_polyandry_age_mod$rptR$CI_emp$CI_org[c("ring", "year"),]) %>% 
  rownames_to_column("term") %>% 
  rename(estimate = R_org,
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
         effect = c(rep("Fixed effects \U1D6FD (logit standardized)", nrow(fixef_bw_Table)),
                    rep("Partitioned \U1D479\U00B2", nrow(R2Table)),
                    rep("Random effects \U1D70E\U00B2", nrow(ranefTable)),
                    rep("Adjusted repeatability \U1D45F", nrow(coefRptTable)),
                    rep("Sample sizes \U1D45B", nrow(sample_sizes)))) %>%
  dplyr::select(effect, everything())

# draw gt table
polyandry_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Polyandry probability</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:12,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 13:15,
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

polyandry_mod_table

# export table to disk
polyandry_mod_table %>% 
  gtsave("polyandry_mod_table.rtf", path = "products/tables/rtf/")

polyandry_mod_table %>% 
  gtsave("Table_S4.png", path = "products/tables/png/")

#### Forest plot of results ----
# Standardized fixed effects
poly_mod_forest_plot_fixef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Fixed") & 
           term != "(Intercept)") %>%
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
  ylab("Fixed\neffects") +
  xlab(expression(italic(paste("Standardized effect size (logit", beta,")" %+-% "95% CI", sep = ""))))

# Semi-partial R2 estimates
poly_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Age since first breeding",
                                 "First breeding age",
                                 "First nest lay date",
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
  scale_y_discrete(labels = c("Total Marginal \U1D479\U00B2" = expression(paste("Marginal ", italic("R"), ''^{2}, sep = "")))) +
  ylab(expression(paste("Semi-partial ", italic("R"),''^{2}, sep = ""))) +
  xlab(expression(italic(paste("Variance explained (R", ''^{2}, ")" %+-% "95% CI", sep = ""))))

# Random effect variances
poly_mod_forest_plot_randef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Random")) %>%
  mutate(comp_name = fct_relevel(comp_name,
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
poly_mod_forest_plot_rptR <-
  allCoefs_mod %>%
  filter(str_detect(effect, "repeat")) %>%
  mutate(comp_name = fct_relevel(comp_name,
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
poly_mod_forest_plot_combo <-
  (poly_mod_forest_plot_fixef / poly_mod_forest_plot_partR2 / 
     # poly_mod_forest_plot_randef / 
     poly_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Polyandry model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(2.00, 2.25, 
                               # 1.5, 
                               1.5), c('cm', 'cm', 
                                       # 'cm', 
                                       'cm')))

poly_mod_forest_plot_combo

# export plot to disk
ggsave(plot = poly_mod_forest_plot_combo,
       filename = "products/figures/svg/poly_mod_forest_plot.svg",
       width = 5,
       height = 9, units = "in")

ggsave(plot = poly_mod_forest_plot_combo,
       filename = "products/figures/jpg/Figure_S4.jpg",
       width = 5,
       height = 9, units = "in")
