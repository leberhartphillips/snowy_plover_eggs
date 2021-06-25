#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

# load the saved results
load("output/stats_renesting_mod.rds")
ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")

# wrangle data
renesting_data <-
  ceuta_egg_chick_female_data %>% 
  # 1) subset to only cases in which the first nest failed
  dplyr::filter(nest_order == 1) %>% 
  select(ID, ring, year, nest_1_fate, first_laydate, n_mates, n_nests, polyandry, multiclutch) %>% 
  distinct() %>% 
  # 2) subset to nests that are confirmed failed
  filter(nest_1_fate != "Hatch" & nest_1_fate != "Unknown") %>% 
  mutate(multiclutch = as.factor(multiclutch)) %>%
  mutate(multi = ifelse(multiclutch == "multi", 1, 0),
         single = ifelse(multiclutch == "single", 1, 0)) %>%
  mutate(multi_plot = ifelse(multi == 1, multi + 0.1, multi - 0.1))

#### Find peaks for bimodal laydate distribution ---- 
set.seed(42)

m1 <- FLXMRglm(family = "gaussian")
m2 <- FLXMRglm(family = "gaussian")

renest_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(multiclutch == "multi") %>% 
  dplyr::select(multiclutch, jul_lay_date_std_num, ID, year, ring) %>%
  distinct()

rened <- density(renest_data$jul_lay_date_std_num)

rened_fit <- flexmix(jul_lay_date_std_num ~ 1, data = renest_data, k = 2, model = list(m1, m2))
rene_peak1 <- modeltools::parameters(rened_fit, component=1)[[1]]
rene_peak2 <- modeltools::parameters(rened_fit, component=2)[[1]]

plot(rened)
abline(v=rene_peak1[[1]], lty=2, col='blue')
abline(v=rene_peak2[[1]], lty=2, col='red')

single_data <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(multiclutch == "single") %>% 
  dplyr::select(multiclutch, jul_lay_date_std_num, ID, year, ring) %>%
  distinct()

singd <- density(single_data$jul_lay_date_std_num)

singd_fit <- flexmix(jul_lay_date_std_num ~ 1, data = single_data, k = 2, model = list(m1, m2))
sing_peak1 <- modeltools::parameters(singd_fit, component=1)[[1]]
sing_peak2 <- modeltools::parameters(singd_fit, component=2)[[1]]

plot(singd)
abline(v=sing_peak1[[1]], lty=2, col='blue')
abline(v=sing_peak2[[1]], lty=2, col='red')

rene_peak2[[1]] - sing_peak1[[1]]
rene_peak1[[1]] - sing_peak1[[1]]

#### Table of effect sizes ----
# Retrieve sample sizes
sample_sizes <-
  renesting_data %>% 
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
                           "Total Marginal \U1D479\U00B2",
                           "First nest lay date",
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
  stats_renesting_mod$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed") %>% 
  mutate_at(c("estimate", "conf.low", "conf.high"), invlogit)

# Fixed effect sizes (standardized)
fixef_bw_Table <- 
  stats_renesting_mod$partR2m$BW %>% 
  # dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
ranefTable <- 
  stats_renesting_mod$tidy %>% 
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
  bind_rows(stats_renesting_mod$partR2m$R2,
            stats_renesting_mod$partR2c$R2[1,]) %>%   
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Adjusted repeatabilities
coefRptTable <- 
  stats_renesting_mod$rptR$R["R_org", ] %>% 
  dplyr::select(-Fixed) %>%
  t() %>% 
  as.data.frame() %>% 
  bind_cols(stats_renesting_mod$rptR$CI_emp$CI_org[c("ring", "year"),]) %>% 
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
renesting_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Re-nesting probability</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:8,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 9:11,
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

renesting_mod_table

# export table to disk
renesting_mod_table %>% 
  gtsave("renesting_mod_table.rtf", path = "products/tables/rtf/")

renesting_mod_table %>% 
  gtsave("Table_S5.png", path = "products/tables/png/")

#### Forest plot of results ----
# Standardized fixed effects
renesting_mod_forest_plot_fixef <-
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
renesting_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
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
renesting_mod_forest_plot_randef <-
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
renesting_mod_forest_plot_rptR <-
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
renesting_mod_forest_plot_combo <-
  (renesting_mod_forest_plot_fixef / renesting_mod_forest_plot_partR2 / 
     # poly_mod_forest_plot_randef / 
     renesting_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'Re-nesting model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(2.00, 2.25, 
                               # 1.5, 
                               1.5), c('cm', 'cm', 
                                       # 'cm', 
                                       'cm')))

renesting_mod_forest_plot_combo

# export plot to disk
ggsave(plot = renesting_mod_forest_plot_combo,
       filename = "products/figures/svg/renesting_mod_forest_plot.svg",
       width = 5,
       height = 9, units = "in")

ggsave(plot = renesting_mod_forest_plot_combo,
       filename = "products/figures/jpg/Figure_S5.jpg",
       width = 5,
       height = 9, units = "in")
