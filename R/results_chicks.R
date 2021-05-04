#### Libraries and data ----
source("R/project_libraries.R")
source("R/project_functions.R")
source("R/project_plotting.R")

#### Results and data----
load("output/stats_chick_mod.rds")
load("data/ceuta_egg_chick_female_data.rds")

# extract fitted values of chick weight v egg volume model
mod_chickw_eggv_fits <- 
  as.data.frame(effect(term = "avg_egg_volume", mod = stats_chick_mod$mod, 
                       xlevels = list(avg_chick_weight = seq(min(eggs_and_chicks_nest_summary[, "avg_chick_weight"], na.rm = TRUE),
                                                             max(eggs_and_chicks_nest_summary[, "avg_chick_weight"], na.rm = TRUE), 0.01))))
# model summary a diagnostics
summary(mod_chickw_eggv)
plot(allEffects(mod_chickw_eggv))
coefplot2(mod_chickw_eggv)
summary(glht(mod_chickw_eggv))

#### Manuscript plot: chick weight v egg volume ----
chickw_eggv_plot <-
  ggplot() +
  geom_errorbarh(data = eggs_and_chicks_nest_summary,
                 aes(y = avg_chick_weight, x = avg_egg_volume, 
                     xmin = avg_egg_volume - sd_egg_volume, 
                     xmax = avg_egg_volume + sd_egg_volume), 
                 alpha = 0.3, size = 0.5, linetype = "solid",
                 color = brewer.pal(8, "Set1")[c(2)]) +
  geom_errorbar(data = eggs_and_chicks_nest_summary,
                aes(y = avg_chick_weight, x = avg_egg_volume, 
                    ymin = avg_chick_weight - sd_chick_weight, 
                    ymax = avg_chick_weight + sd_chick_weight), 
                alpha = 0.2, size = 0.5, linetype = "solid",
                color = brewer.pal(8, "Set1")[c(2)]) +
  geom_point(data = eggs_and_chicks_nest_summary,
             aes(x = avg_egg_volume, y = avg_chick_weight),
             alpha = 0.4,
             shape = 19, #21, 
             color = brewer.pal(8, "Set1")[c(2)]) +
  geom_line(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume, y = fit),
            lwd = 0.5) +
  geom_ribbon(data = mod_chickw_eggv_fits, aes(x = avg_egg_volume, 
                                               ymax = upper, ymin = lower),
              lwd = 1, alpha = 0.25) +
  luke_theme +
  theme(panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0.5, "cm"),
        axis.title.y = element_text(vjust = 5)) +
  scale_y_continuous(limits = c(min(eggs_and_chicks_nest_summary$avg_chick_weight, na.rm = TRUE), 
                                max(eggs_and_chicks_nest_summary$avg_chick_weight, na.rm = TRUE) * 1.05)) +
  ylab("Avg. chick weight at hatch (g)") +
  xlab(expression(paste("Avg. egg volume (cm", ''^{3}, ")", sep = ""))) 

ggsave(plot = chickw_eggv_plot,
       filename = "products/figures/chickw_eggv_plot.svg",
       width = 5.29*2,
       height = 5.29*2, units = "cm")

#### Table of effect sizes ----
# Retrieve sample sizes
sample_sizes <-
  eggs_and_chicks_nest_summary %>% 
  ungroup() %>% 
  summarise(Year = n_distinct(year),
            Individual = n_distinct(mother_ring),
            Nests = n_distinct(ID))

sample_sizes <- 
  as.data.frame(t(as.data.frame(sample_sizes))) %>%
  rownames_to_column("term") %>% 
  rename(estimate = V1) %>% 
  mutate(stat = "n")


# clean model component names
mod_comp_names <- 
  data.frame(comp_name = c("Average egg volume of clutch",
                           "Total Marginal \U1D479\U00B2",
                           "Total Conditional \U1D479\U00B2",
                           "Mother identity",
                           "Year",
                           "Residual",
                           "Mother identity",
                           "Year",
                           "Residual",
                           "Year",
                           "Individuals",
                           "Observations (i.e., Nests)"))

# Fixed effect sizes (non-standardized)
fixefTable <- 
  stats_chick_mod$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed") %>% 
  mutate_at(c("estimate", "conf.low", "conf.high"), invlogit)

# Fixed effect sizes (standardized)
fixef_bw_Table <- 
  stats_chick_mod$partR2m$BW %>% 
  # dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed_bw") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Semi-partial R2 estimates
ranefTable <- 
  stats_chick_mod$tidy %>% 
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
  bind_rows(stats_chick_mod$partR2m$R2[1,],
            stats_chick_mod$partR2c$R2[1,]) %>%   
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# Adjusted repeatabilities
coefRptTable <- 
  stats_chick_mod$rptR$R_boot %>% 
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

# draw gt table
chick_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = html("<i>Chick hatch weight</i>"),
             estimate = "Mean estimate",
             coefString = "95% confidence interval") %>% 
  fmt_number(columns = vars(estimate),
             rows = 1:9,
             decimals = 2,
             use_seps = FALSE) %>% 
  fmt_number(columns = vars(estimate),
             rows = 10:12,
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

chick_mod_table

# export table to disk
chick_mod_table %>% 
  gtsave("chick_mod_table.rtf", path = "products/tables/rtf/")

chick_mod_table %>% 
  gtsave("chick_mod_table.png", path = "products/tables/png/")

#### Forest plot of results ----
# Standardized fixed effects
chick_mod_forest_plot_fixef <-
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
  xlab(expression(italic(paste("Standardized effect size (", beta,")" %+-% "95% CI", sep = ""))))

# Semi-partial R2 estimates
chick_mod_forest_plot_partR2 <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Partitioned") & str_detect(comp_name, "Conditional", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
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
chick_mod_forest_plot_randef <-
  allCoefs_mod %>%
  filter(str_detect(effect, "Random") & str_detect(comp_name, "Residual", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Year",
                                 "Mother identity")) %>%
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
chick_mod_forest_plot_rptR <-
  allCoefs_mod %>%
  filter(str_detect(effect, "repeat") & str_detect(comp_name, "Residual", negate = TRUE)) %>%
  mutate(comp_name = fct_relevel(comp_name,
                                 "Year",
                                 "Mother identity")) %>%
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
chick_mod_forest_plot_combo <-
  (chick_mod_forest_plot_fixef / chick_mod_forest_plot_partR2 / 
     # chick_mod_forest_plot_randef / 
     chick_mod_forest_plot_rptR) + 
  plot_annotation(tag_levels = 'A', title = 'chick model', theme = theme(plot.title = element_text(face = 'italic'))) +
  plot_layout(heights = unit(c(0.75, 0.75, 
                               # 1.5, 
                               1.5), c('cm', 'cm', 
                                       # 'cm', 
                                       'cm')))

chick_mod_forest_plot_combo

# export plot to disk
ggsave(plot = chick_mod_forest_plot_combo,
       filename = "products/figures/svg/chick_mod_forest_plot.svg",
       width = 5,
       height = 9, units = "in")

ggsave(plot = chick_mod_forest_plot_combo,
       filename = "products/figures/jpg/chick_mod_forest_plot.jpg",
       width = 5,
       height = 9, units = "in")
