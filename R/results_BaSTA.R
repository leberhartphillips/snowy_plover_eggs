source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

load(file = "output/multibasta_output_females_min_age_1_2006-2020.rds")
load(file = "data/raw_life_table_females_2006_2020.rds")

# DIC model selection shows that the Logistic mortality model with bathtub 
# shape fits our data the best
BaSTA_DIC_table <- 
  multiout_females$DICs %>% 
  as.data.frame() %>% 
  mutate(model = ifelse(model == "LO", "Logistic",
                        ifelse(model == "WE", "Weibull",
                               ifelse(model == "GO", "Gompertz", "Exponential"))),
         shape = paste0(toupper(substr(shape, 1, 1)), 
                        substr(shape, 2, nchar(.)))) %>% 
  mutate(shape = ifelse(shape == "Simp", "Simple", shape)) %>% 
  dplyr::select(model, shape, k, DICdiff) %>% 
  gt() %>% 
  cols_label(model = "Mortality function",
             shape = "Shape",
             k = md("*k*"),
             # DIC = md("*DIC*"),
             DICdiff = md("\U0394*DIC*")) %>% 
  fmt_number(columns = vars(DICdiff),
             decimals = 2,
             use_seps = FALSE) %>% 
  # tab_options(column_labels.font.weight = "bold",
  #             table.width = pct(50),
  #             column_labels.font.size = 14,
  #             table.font.size = 12,
  #             data_row.padding = 5, table.align = "left") %>% 
  fmt_missing(columns = 3,
              missing_text = "") %>% 
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = brewer.pal(9,"Greys")[3],
              table.font.size = 12,
              data_row.padding = 3,
              row_group.padding = 4,
              summary_row.padding = 2,
              column_labels.font.size = 14,
              row_group.font.size = 12,
              table.width = pct(60))

BaSTA_DIC_table

# save as rtf for manuscript
BaSTA_DIC_table %>% 
  gtsave("BaSTA_DIC_table.rtf", path = "products/tables/rtf/", expand = 1)

BaSTA_DIC_table %>% 
  gtsave("BaSTA_DIC_table.png", path = "products/tables/png/", expand = 0.5)

# Extract top model
plover_survival_model <- 
  multiout_females$runs[[1]]

# Check the diagnostics to assess the simulation performance
BaSTA_autocorr_plot <- 
  plover_survival_model$coefficients %>% 
  as.data.frame() %>% 
  dplyr::select(SerAutocor) %>% 
  mutate(coeffs = rownames(.)) %>% 
  ggplot() +
  geom_col(aes(x = coeffs, y = SerAutocor)) +
  scale_y_continuous(limits = c(-1, 1)) +
  ylab("Serial autocorrelation within chain") +
  luke_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

BaSTA_chain_converg_plot <- 
  plover_survival_model$convergence %>% 
  as.data.frame() %>% 
  dplyr::select(Rhat) %>% 
  mutate(coeffs = rownames(.)) %>% 
  ggplot() +
  geom_hline(yintercept = 1, color = "red") +
  geom_point(aes(x = coeffs, y = Rhat), size = 2) +
  scale_y_continuous(limits = c(0.9, 1.1)) +
  ylab("Chain convergence (R-hat)") +
  xlab("Coefficient") +
  luke_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Serial autcorrelation: values should hover around zero...good!
# Convergence: values should hover around one...good!
BaSTA_model_diag_plot <-
  BaSTA_autocorr_plot + BaSTA_chain_converg_plot + 
  plot_layout(heights = c(0.45, 0.55)) +
  plot_annotation(tag_levels = "A")

BaSTA_model_diag_plot

ggsave(plot = BaSTA_model_diag_plot,
       filename = "products/figures/jpg/BaSTA_model_diag_plot.jpg",
       width = 4.5,
       height = 7, 
       units = "in")

# Trace plot: should look like a hairy catepillar instead of a snake...good!
plot(plover_survival_model)

BaSTA_trace_plot <-
  pdf("products/figures/BaSTA_trace_plot.pdf",
      width = 6,
      height = 8)
plot(plover_survival_model)
dev.off()

# Plot age-specific female survival probability and mortality rate for the Logistic mortality model with bathtub shape.
female_survival_plot <-
  t(plover_survival_model$survQuant$noCov) %>% 
  as.data.frame() %>% 
  mutate(age = as.numeric(rownames(.))) %>% 
  ggplot() +
  geom_line(aes(x = age, y = `50%`), linetype = "dashed", color = "#7570B3") +
  geom_ribbon(aes(x = age, ymin = `2.5%`, ymax = `97.5%`), 
              color = "#7570B3", fill = "#7570B3", alpha = 0.4) +
  scale_y_continuous(limits = c(0, 1)) +
  ylab("Survival probability ± 95% CI") +
  xlab("Age (years)") +
  scale_x_continuous(limits = c(0.5, 12.5), breaks = c(1:12)) +
  luke_theme +
  # scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

female_mortality_plot <-
  t(plover_survival_model$mortQuant$noCov) %>% 
  as.data.frame() %>% 
  mutate(age = as.numeric(rownames(.))) %>% 
  ggplot() +
  geom_line(aes(x = age, y = `50%`), linetype = "dashed", color = "#7570B3") +
  geom_ribbon(aes(x = age, ymin = `2.5%`, ymax = `97.5%`), 
              color = "#7570B3", fill = "#7570B3", alpha = 0.4) +
  ylab("Mortality hazard ± 95% CI") +
  xlab("Age (years)") +
  luke_theme +
  # scale_x_continuous(limits = c(1, 12), breaks = c(1:12))
  scale_x_continuous(limits = c(0.5, 12.5), breaks = c(1:12))

recruit_ages_obs <-
  raw_life_table_females_2006_2020 %>% 
  dplyr::filter(birth != 0) %>%
  unite(ch, `2006`:`2020`) %>% 
  mutate(ch = str_remove_all(ch, "_")) %>% 
  mutate(ch2 = gsub("(?<![0-9])0+", "", ch, perl = TRUE))

recruit_ages_obs$ch3 <- 
  sapply(strsplit(recruit_ages_obs$ch2, split = ""), 
         function(str) {paste(rev(str), collapse = "")})

recruit_ages_obs <- 
  recruit_ages_obs %>% 
  mutate(ch4 = gsub("(?<![0-9])0+", "", ch3, perl = TRUE))

recruit_ages_obs <- 
  recruit_ages_obs %>% 
  mutate(age_obs = nchar(ch4)-1) %>% 
  dplyr::select(ring, age_obs) %>%
  arrange(desc(age_obs))

recruit_ages_freq <- 
  expand.grid(as.character(raw_life_table_females_2006_2020[which(raw_life_table_females_2006_2020$birth != 0), 
                                      "ring"]), c(0:12)) %>% 
  rename(ring = Var1,
         age = Var2) %>% 
  left_join(recruit_ages_obs, by = "ring") %>% 
  dplyr::filter(age <= age_obs) %>% 
  arrange(ring)

recruit_ages_freq_totals <- 
  recruit_ages_freq %>%
  group_by(age) %>%
  dplyr::summarize(total = n()) %>% 
  bind_rows(., data.frame(age = 12, total = 0))

recruit_ages_freq_plot <-
  recruit_ages_freq %>%
  dplyr::filter(age != 0) %>% 
  ggplot() +
  geom_histogram(aes(age), alpha = 0.3, color = "grey40", fill = "#7570B3", 
                 binwidth = 1, position = "identity") +
  geom_text(aes(x = age, y = total + 5, label = total, fill = NULL),
            data = recruit_ages_freq_totals,
            size = 3, family = "Franklin Gothic Book", fontface = "italic") +
  luke_theme +
  theme(panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_continuous(limits = c(0.5, 12.5), breaks = c(1:12)) +
  ylab("Frequency") +
  annotate(geom = "text", y = 30, x = 8,
           label = "Frequency distribution of observations\n of known-aged females",
           color = "black", size = 3, fontface = 'italic')

# draw the three panels together for Fig S1
# Fig_1 <-
max_dims <- get_max_dim(recruit_ages_freq_plot, female_survival_plot, female_mortality_plot)
set_dim(recruit_ages_freq_plot, max_dims)

BaSTA_curve_plot <- 
  recruit_ages_freq_plot / female_mortality_plot +
  plot_layout(heights = c(0.4, 0.6)) +
  plot_annotation(tag_levels = "A")

BaSTA_curve_plot

ggsave(plot = BaSTA_curve_plot,
       filename = "products/figures/jpg/BaSTA_curve_plot.jpg",
       width = 4.5,
       height = 5, 
       units = "in")

# Wrangle the estimated birth year and age-at-death for each individual given 
# the BaSTA output
BaSTA_births <- 
  t(plover_survival_model$birthQuant) %>% 
  as.data.frame() %>% 
  mutate(ID = row.names(.)) %>% 
  rename(est_b = `50%`,
         upper_b = `97.5%`,
         lower_b = `2.5%`)

BaSTA_death_ages <- 
  t(plover_survival_model$agesQuant) %>% 
  as.data.frame() %>% 
  mutate(ID = row.names(.)) %>% 
  rename(est_d = V1,
         upper_d = `97.5%`,
         lower_d = `2.5%`)

BaSTA_ages <-
  raw_life_table_females_2006_2020 %>% 
  dplyr::select(ID, ring) %>%
  mutate(ID = as.character(ID)) %>% 
  left_join(., BaSTA_births, by = "ID") %>% 
  left_join(., BaSTA_death_ages, by = "ID") %>% 
  dplyr::select(-ID)

BaSTA_ages <- 
  BaSTA_ages %>% 
  
  # manually add the known births for CA2036 and CA1526 (first ringed as chicks 
  # during a small study in 2004)
  mutate(est_b = ifelse(ring %in% c("CA2036", "CA1526"), 2004, est_b),
         lower_b = ifelse(ring %in% c("CA2036", "CA1526"), 2004, lower_b),
         upper_b = ifelse(ring %in% c("CA2036", "CA1526"), 2004, upper_b)) %>% 
  
  # mutate(est_b = ifelse(ring %in% c("CA245", "CA241"), 2005, est_b),
  #        lower_b = ifelse(ring %in% c("CA245", "CA241"), 2005, lower_b),
  #        upper_b = ifelse(ring %in% c("CA245", "CA241"), 2005, upper_b)) %>% 
  
  # manually change the estimated birth and death of CA1579 by one year (this
  # individual was first ringed as an adult during the small study in 2004)
  mutate(est_b = ifelse(ring == "CA1579", est_b - 1, est_b),
         lower_b = ifelse(ring == "CA1579", lower_b - 1, lower_b),
         upper_b = ifelse(ring == "CA1579", upper_b - 1, upper_b),
         est_d = ifelse(ring == "CA1579", est_d + 1, est_d),
         lower_d = ifelse(ring == "CA1579", lower_d + 1, lower_d),
         upper_d = ifelse(ring == "CA1579", upper_d + 1, upper_d))

save(BaSTA_ages, file = "output/BaSTA_age_estimates_2006-2020.rds")
