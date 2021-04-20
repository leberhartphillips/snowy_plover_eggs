#### Libraries and data ----
source("R/001_libraries.R")
source("R/002_functions.R")

# load the saved results
load("output/stats_poly_date.rds")
load("data/ceuta_egg_chick_female_data.rds")

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::filter(nest_order == 1) %>%
  dplyr::select(polyandry, jul_lay_date_std_num, ID, year, ring, avg_ad_tarsi) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

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
  data.frame(comp_name = c("Intercept",
                           "First nest lay date",
                           "Total Marginal \U1D479\U00B2",
                           "First nest lay date",
                           "Total Conditional \U1D479\U00B2",
                           "Individual",
                           "Year",
                           # "Residual",
                           "Individual",
                           "Year",
                           # "Residual",
                           "Years",
                           "Individuals",
                           "Observations (i.e., Nests)"))

fixefTable <- 
  stats_poly_date$tidy %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::select(term, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "fixed") %>% 
  mutate_at(c("estimate", "conf.low", "conf.high"), invlogit)

ranefTable <- 
  stats_poly_date$tidy %>% 
  dplyr::filter(effect == "ran_pars") %>% 
  dplyr::select(group, estimate, conf.low, conf.high) %>% 
  as.data.frame() %>% 
  mutate(stat = "rand") %>% 
  rename(term = group) %>% 
  mutate(estimate = estimate^2,
         conf.high = conf.high^2,
         conf.low = conf.low^2)

R2Table <- 
  bind_rows(stats_poly_date$partR2m$R2,
            stats_poly_date$partR2c$R2[1,]) %>%   
  dplyr::select(term, estimate, CI_lower, CI_upper) %>% 
  as.data.frame() %>% 
  mutate(stat = "partR2") %>% 
  rename(conf.low = CI_lower,
         conf.high = CI_upper)

# coefRptTable <- 
# rbind(c(mean(stats_poly_date$rptR$R_boot_org$ring),
#         quantile(stats_poly_date$rptR$R_boot_org$ring, prob = c(0.025, 0.975))),
#       c(mean(stats_poly_date$rptR$R_boot_org$year),
#         quantile(stats_poly_date$rptR$R_boot_org$ring, prob = c(0.025, 0.975))))
#   
#     
#   sapply(., 
#         function(x) c(mean (x), quantile(x, prob = c(0.025, 0.975)))) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   rownames_to_column("term") %>% 
#   rename(estimate = V1,
#          conf.low = `2.5%`,
#          conf.high = `97.5%`) %>% 
#   mutate(stat = "RptR")

coefRptTable <- 
  stats_poly_date$rptR$R["R_org", ] %>% 
  dplyr::select(-Fixed) %>%
  t() %>% 
  as.data.frame() %>% 
  bind_cols(stats_poly_date$rptR$CI_emp$CI_org[c("ring", "year"),]) %>% 
  rownames_to_column("term") %>% 
  rename(estimate = R_org,
         conf.low = `2.5%`,
         conf.high = `97.5%`) %>% 
  mutate(stat = "RptR")

# coefRptTable <- 
#   as.data.frame(do.call(cbind, stats_poly_date$rptR$R_boot_link)) %>% 
#   dplyr::select(-Fixed) %>%
#   # mutate(residual = 1 - rowSums(.)) %>% 
#   apply(., 2, 
#         function(x) c(mean (x), quantile (x, prob = c(0.025, 0.975)))) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   rownames_to_column("term") %>% 
#   rename(estimate = V1,
#          conf.low = `2.5%`,
#          conf.high = `97.5%`) %>% 
#   mutate(stat = "RptR")

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
         effect = c(rep("Fixed effects \U1D6FD (polyandry probability)", nrow(fixefTable)),
                    rep("Partitioned \U1D479\U00B2", nrow(R2Table)),
                    rep("Random effects \U1D70E\U00B2", nrow(ranefTable)),
                    rep("Repeatability \U1D479", nrow(coefRptTable)),
                    rep("Sample sizes \U1D45B", nrow(sample_sizes)))) %>%
  dplyr::select(effect, everything())

# re-organize model components for table
allCoefs_mod <-
  allCoefs_mod[c(1:3, 5, 4, 6:12), ]

polyandry_mod_table <- 
  allCoefs_mod %>% 
  dplyr::select(effect, comp_name, estimate, coefString) %>% 
  gt(rowname_col = "row",
     groupname_col = "effect") %>% 
  cols_label(comp_name = "",
             estimate = "Parameter estimate",
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

polyandry_mod_table

polyandry_mod_table %>% 
  gtsave("polyandry_mod_table.rtf", path = "products/tables/")
polyandry_mod_table %>% 
  gtsave("polyandry_mod_table.png", path = "products/tables/")

image_read(path = "results/tables/table_S2.png")