#### Libraries and data ----
source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

ceuta_egg_chick_female_data <- 
  readRDS("data/Ceuta_egg_chick_female_data.rds")
load("data/raw_encounter_histories_females_2006_2020.rds")
load("output/BaSTA_age_estimates_2006-2020.rds")
load("data/BaSTA_checked_life_table_females_2006-2020.rds")

# extract the first and last observations of each individual
ceuta_egg_chick_female_data_3_year <-  
  ceuta_egg_chick_female_data %>% 
  dplyr::filter(n_years_obs > 2)

max_min_obs <-
  encounter_histories %>% 
  dplyr::filter(ring %in% ceuta_egg_chick_female_data_3_year$ring) %>% 
  left_join(., dplyr::select(BaSTA_ages, ring, est_b), by = "ring") %>%
  mutate(est_age = as.numeric(year) - as.numeric(est_b)) %>% 
  group_by(ring) %>% 
  summarise(min_age = min(est_age),
            max_age = max(est_age),
            obs_span = max(est_age) - min(est_age)) %>% 
  as.data.frame() %>% 
  left_join(., dplyr::select(ceuta_egg_chick_female_data_3_year, ring, age_first_cap)) %>% 
  distinct()

# manually adjust the ages of CA1526 and CA2036 to reflect their first encounter
# during the 2004 pilot study as chicks (i.e., aged 0)
max_min_obs[which(max_min_obs$ring %in% c("CA1526", "CA2036")), "min_age"] <-
  c(0, 0)

# manually adjust the min age estimate of CA1579 to relect that it was first
# encountered as an adult during the 2004 pilot study (i.e., subtract 2 years
# from BaSTA estimate of age at first encounter)
max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"] <-
  max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"] - 2
max_min_obs[which(max_min_obs$ring == "CA1579"), "obs_span"] <-
  max_min_obs[which(max_min_obs$ring == "CA1579"), "max_age"] -
  max_min_obs[which(max_min_obs$ring == "CA1579"), "min_age"]

# specify the factor levels according to the rank (for plotting)
max_min_obs$ring_ordered <- 
  factor(max_min_obs$ring, 
         levels = unique(max_min_obs$ring[order(max_min_obs$min_age, 
                                                max_min_obs$obs_span, 
                                                decreasing = TRUE)]), 
         ordered = TRUE)

# calculate the number of clutches per year for each individual and join back to 
# ranked dataframe
eggdf_clutches <- 
  ceuta_egg_chick_female_data_3_year %>% 
  dplyr::group_by(ring, year) %>%
  dplyr::summarise(n_clutches = n_distinct(ID)) %>% 
  dplyr::right_join(dplyr::select(max_min_obs, 
                                  c(ring, min_age, obs_span, age_first_cap)), 
                    by = "ring") %>% 
  dplyr::right_join(dplyr::select(ceuta_egg_chick_female_data_3_year, c(ring, year, est_age, polyandry)), 
                    by = c("ring", "year")) %>% 
  distinct() %>% 
  mutate(polyandry = ifelse(polyandry == "poly", "2", "1"))

# specify the factor levels according to the rank (for plotting)
eggdf_clutches$ring_ordered <- 
  factor(eggdf_clutches$ring, 
         levels = unique(eggdf_clutches$ring[order(eggdf_clutches$min_age, 
                                                   eggdf_clutches$obs_span, 
                                                   decreasing = TRUE)]), 
         ordered = TRUE)

# create a ranking variable and confidence interval limits of age estimate
eggdf_n_upper <-
  ceuta_egg_chick_female_data_3_year %>%
  dplyr::group_by(ring) %>%
  dplyr::summarise(CI_age = max(est_death_age))


eggdf_age_CI <-
  ceuta_egg_chick_female_data_3_year %>%
  dplyr::group_by(ring) %>%
  dplyr::summarise(CI_age = 1) %>%
  dplyr::bind_rows(eggdf_n_upper) %>%
  mutate(ring = as.factor(ring)) %>% 
  arrange(ring) %>% 
  dplyr::left_join(., dplyr::select(max_min_obs, 
                                    c(ring, min_age, obs_span, age_first_cap)))

# specify the factor levels according to the rank (for plotting)
eggdf_age_CI$ring_ordered <- 
  factor(eggdf_age_CI$ring, 
         levels = unique(eggdf_age_CI$ring[order(eggdf_age_CI$min_age, 
                                                 eggdf_age_CI$obs_span,
                                                 decreasing = TRUE)]), 
         ordered = TRUE)

# # join polyandry information
# eggdf_mating_sys_all <-
#   dbReadTable(Ceuta_OPEN, "BirdRef") %>% 
#   dplyr::select(year, ID, male, female) %>%
#   filter(female %in% eggdf$ring) %>% 
#   arrange(female) %>%
#   group_by(female, year) %>%
#   summarise(n_mates = n_distinct(male, na.rm = TRUE)) %>% 
#   mutate(polyandry = ifelse(n_mates > 1, "poly", "mono"),
#          year = as.integer(year),
#          n_mates = ifelse(n_mates == 0, 1, n_mates)) %>% 
#   rename(ring = female)
# 
# matings_without_egg_obs <- 
#   anti_join(eggdf_mating_sys_all, eggdf, 
#             by = c("ring", "year")) %>% 
#   left_join(., dplyr::select(eggdf, ring, age_first_cap, est_b), by = "ring") %>% 
#   mutate(est_age = year - est_b) %>% 
#   distinct()

# # specify the factor levels according to the rank (for plotting)
# matings_without_egg_obs_plot <- 
#   matings_without_egg_obs %>% 
#   dplyr::filter(ring %in% ceuta_egg_chick_female_data_3_year$ring)
# 
# matings_without_egg_obs_plot$ring_ordered <- 
#   factor(matings_without_egg_obs_plot$ring, 
#          levels = levels(eggdf_age_CI$ring_ordered),
#          ordered = TRUE)
# 
# dplyr::filter(ceuta_egg_chick_female_data_3_year, is.na(volume) & !is.na(polyandry))

# specify point size for number of clutches per year
point_size <- c(1, 2, 3, 4)
point_colors <- c("black", "#f03b20")

# create the first captured as adult plot
Imm_plot <-
  ggplot2::ggplot() +
  geom_line(data = dplyr::filter(eggdf_age_CI, age_first_cap == "A"),
            aes(x = CI_age, y = ring_ordered), color = "grey90",
            size = 2.5, lineend = "round") +
  geom_linerange(data = dplyr::filter(max_min_obs, age_first_cap == "A"),
                 orientation = "y", aes(y = ring_ordered, x = min_age,
                                        xmin = min_age, xmax = max_age),
                 size = 0.85, color = "grey60") +
  geom_point(data = dplyr::filter(eggdf_clutches, age_first_cap == "A"), 
             aes(x = est_age, y = ring_ordered, size = as.factor(n_clutches), 
                 fill = as.factor(polyandry)), shape = 21) +
  # geom_point(data = dplyr::filter(matings_without_egg_obs_plot, age_first_cap == "A"),
  #            aes(x = est_age, y = ring_ordered), size = 2, shape = 4) +
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24, by = 2)) +
  ylab("First captured as adults") +
  xlab("Age ± 95% CrI") +
  luke_theme +
  theme(legend.position = c(0.75, 0.8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  scale_size_manual(values = point_size) +
  scale_fill_manual(values = point_colors) +
  labs(size = "Number of\nclutches observed",
       fill = "Number of\nmates observed") +
  annotate(geom = "text", y = 28, x = 16,
           label = "For visual purposes,\nonly individuals\nwith more than 2 years of\nobservations are shown",
           color = "black", size = 3, fontface = 'italic', hjust = 0)

# create the recruit plot
Rec_plot <- 
  ggplot2::ggplot() +
  geom_linerange(data = dplyr::filter(max_min_obs, age_first_cap == "J"), 
                 orientation = "y", aes(y = ring_ordered, x = min_age,
                                        xmin = min_age, xmax = max_age), 
                 size = 0.85, color = "grey60") +
  geom_point(data = dplyr::filter(eggdf_clutches, age_first_cap == "J"), 
             aes(x = est_age, y = ring_ordered, size = as.factor(n_clutches), 
                 fill = as.factor(polyandry)), shape = 21) +
  
  # geom_point(data = dplyr::filter(matings_without_egg_obs_plot, age_first_cap == "J"),
  #            aes(x = est_age, y = ring_ordered), size = 2, shape = 4) +
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24, by = 2)) +
  ylab("Locally recruited") +
  xlab("Age") +
  luke_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_size_manual(values = point_size) +
  scale_fill_manual(values = point_colors)

# draw the two panels together for Fig 1
plot_of_sample_population <- 
  ggarrange(Rec_plot, Imm_plot, nrow = 2, align = "v",
            # heights = c(0.5, 0.5))
            heights = c(0.19, 0.81))

plot_of_sample_population2 <-
  (Rec_plot / Imm_plot) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = c(0.19, 0.81))

plot_of_sample_population

ggsave(plot = plot_of_sample_population2,
       filename = "products/figures/jpg/Figure_2.jpg",
       width = 6,
       height = 10, units = "in")

#### Sample size summaries ----
# reports values listed chronologicall in the manuscript

# tally number of nests with various lay date methods
ceuta_egg_chick_female_data %>%
  select(ID, lay_date_method) %>% 
  distinct() %>% 
  group_by(lay_date_method) %>% 
  summarise(n_nests = n_distinct(ID)) %>% 
  mutate(prop_nests = as.numeric(as.character(n_nests))/sum(n_nests))

# number of females molecularly sex-typed
dbReadTable(CeutaCLOSED,"Captures") %>% 
  dplyr::filter(ring %in% ceuta_egg_chick_female_data$ring) %>% 
  dplyr::select(ring, mol_sex, sex) %>%
  distinct %>% 
  group_by(ring) %>% 
  arrange(mol_sex) %>% 
  slice(1) %>% 
  group_by(mol_sex) %>% 
  summarise(n = n()) %>% 
  mutate(prop_sexed = as.numeric(as.character(n))/sum(n))

#### Chick age wrangle ----
# Extract chick measurements from the capture data
chicks_measurements <-
  # read the Captures table
  dbReadTable(CeutaCLOSED, "Captures") %>% 
  # subset to SNPL
  dplyr::filter(species == "SNPL") %>% 
  # average the left and right tarsus length measurements in to one value
  mutate(tarsus = rowMeans(cbind(as.numeric(left_tarsus), 
                                 as.numeric(right_tarsus)), na.rm = TRUE)) %>% 
  # calculate Body Mass Index based on weight and structural size
  mutate(BMI = weight/(tarsus^2)) %>% 
  # subset to juvenile captures
  dplyr::filter(age == "J",
                # remove observations that are missing information for weight and tarsus
                !is.na(weight) & !is.na(tarsus) & !is.na(date)) %>% 
  # convert date columns to the %Y-%m-%d format
  plover_date_convert(input = "Rdate") %>% 
  # group by bird identity
  group_by(ring) %>%
  # specify the biological nest ID as the ID of the earliest capture (i.e.,
  # brood mixing can occur and create multiple nest IDs for a chick in the
  # capture data)
  dplyr::mutate(bio_ID = ID[which.min(date)]) %>% 
  # filter(bio_ID != ID) %>% 
  # convert to dataframe
  data.frame() %>% 
  # select the relevant columns
  dplyr::select(ring, year, bio_ID, age, sex, date, time, weight, tarsus, bill, BMI) %>% 
  # extract only chicks that are in the egg data
  dplyr::filter(bio_ID %in% eggs_2006_2020$ID) %>% 
  arrange(ring)

# Extract hatch dates
hatch_dates <-
  # read the Nests table
  dbReadTable(CeutaCLOSED, "Nests") %>%
  # extract only the nests that contain chicks in the previous capture subset
  dplyr::filter(ID %in% chicks_measurements$bio_ID) %>%
  # subset the nests that have hatch date information
  dplyr::filter(fate == "Hatch") %>%
  # define as a dataframe
  data.frame() %>%
  # classify date columns in the appropriate format
  plover_date_convert(input = "Rdate") %>% 
  # subset the result as simply the nest ID and their respective hatch dates
  dplyr::select(ID, end_date) %>%
  # rename the columns
  rename(bio_ID = ID,
         hatch_date = end_date)

# combine chick measurements with hatch dates
chicks_2006_2020 <- 
  # join the hatch dates to the chick captures
  left_join(x = chicks_measurements, y = hatch_dates, by = "bio_ID") %>%
  # classify variables as factor or numeric and calculate age at capture
  ungroup() %>% 
  mutate(age = as.numeric(date - hatch_date),
         year = as.factor(year),
         weight = as.numeric(weight),
         tarsus = as.numeric(tarsus),
         ring = as.factor(ring),
         sex = as.factor(sex)) %>% 
  rename(chick_ring = ring,
         ID = bio_ID) %>% 
  # specify ages less than 0 as 0 (i.e., hatch dates represent the average hatch
  # date of a brood and thus the earliest chick to hatch in a nest could be up
  # 2 days earlier than the nest's hatch date)
  mutate(age = ifelse(age < 0, 0, age)) %>% 
  # scale the numeric variables in preparation for modeling
  mutate(age.z = scale(age),
         weight.z = scale(weight),
         tarsus.z = scale(tarsus),
         BMI.z = scale(BMI)) %>% 
  # remove individuals that don't have an age value (i.e., hatch date was NA)
  dplyr::filter(!is.na(age)) %>% 
  left_join(., dplyr::select(eggs_2006_2020, ID, ring)) %>% 
  rename(mother_ring = ring)# chick measuring age
chicks_2006_2020 %>% 
  arrange(chick_ring, age) %>% 
  group_by(chick_ring) %>% 
  slice(1) %>% 
  dplyr::filter(age %in% c(0, 1)) %>% 
  group_by(age) %>% 
  summarise(n = n()) %>% 
  mutate(prop_age = as.numeric(as.character(n))/sum(n))

# capture-mark-recapture summary
BaSTA_checked_life_table_females_2006_2020$newData %>% 
  mutate(first_age = ifelse(birth == 0, "A", "J")) %>% 
  group_by(first_age) %>% 
  summarise(n_ind = n_distinct(idnames))

# detection summary
BaSTA_checked_life_table_females_2006_2020$newData %>% 
  rowwise(idnames) %>% 
  mutate(total_detections = sum(c_across(X2006:X2020))) %>% 
  ungroup() %>% 
  summarise(sum(total_detections))

BaSTA_checked_life_table_females_2006_2020$newData %>% 
  rowwise(idnames) %>% 
  mutate(total_detections = sum(c_across(X2006:X2020))) %>% 
  ungroup() %>% 
  summarise(mean_detections = mean(total_detections),
            sd_detections = sd(total_detections),
            median_detections = median(total_detections))

# number of observations in egg model
ceuta_egg_chick_female_data %>% 
  summarise(Years = n_distinct(year),  # N = 14 years
            Individuals = n_distinct(ring),    # N = 426 females
            Nests = n_distinct(ID),    # N = 841 nests
            Eggs = nrow(.)) %>%  # N = 2392 eggs
  t(.) %>% 
  as.data.frame() %>% 
  rename(n = V1) %>% 
  collect() %>%
  kable(col.names = c("Sample size")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# tally number of individuals with 3, 4, etc. years of observations
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(n_years = n_distinct(year)) %>% 
  mutate(n_years = as.factor(n_years)) %>% 
  group_by(n_years) %>% 
  tally() %>% 
  collect() %>%
  kable(col.names = c("Number of years",
                      "Frequency of individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

(34+9+7+4+1+1)/(34+9+7+4+1+1+83+287)

83/(34+9+7+4+1+1+83+287)

287/(34+9+7+4+1+1+83+287)

# tally number of recruits vs immigrants
ceuta_egg_chick_female_data %>% 
  group_by(age_first_cap) %>% 
  summarise(n_inds = n_distinct(ring)) %>% 
  mutate(prop = n_inds/sum(n_inds))
mutate(n_years = as.factor(n_years)) %>% 
  group_by(n_years) %>% 
  tally() %>% 
  collect() %>%
  kable(col.names = c("Number of years",
                      "Frequency of individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# female tarsus measurements summary
ceuta_egg_chick_female_data %>% 
  summarise(mean_avg_ad_tarsi = mean(avg_ad_tarsi, na.rm = TRUE),
            sd_avg_ad_tarsi = sd(avg_ad_tarsi, na.rm = TRUE),
            mean_sd_ad_tarsi = mean(sd_ad_tarsi, na.rm = TRUE),
            sd_sd_ad_tarsi = sd(sd_ad_tarsi, na.rm = TRUE)) %>% 
  as.data.frame()

# wrangle data to include only first nests
first_nests_data <-
  ceuta_egg_chick_female_data %>%
  dplyr::filter(nest_order == 1) %>% 
  dplyr::select(polyandry, year, ring, first_laydate, n_nests, ID,
                est_age_t_deviation, first_age_t, last_age_t) %>%
  distinct() %>%
  mutate(polyandry = as.factor(polyandry)) %>%
  mutate(poly = ifelse(polyandry == "poly", 1, 0),
         mono = ifelse(polyandry == "mono", 1, 0)) %>%
  mutate(poly_plot = ifelse(poly == 1, poly + 0.1, poly - 0.1))

# sample size summary
first_nests_data %>% 
  summarise(n_inds = n_distinct(ring),
            n_nests = n_distinct(ID))

first_nests_data %>% 
  mutate(multinest = ifelse(n_nests > 1, "multi", "single")) %>% 
  group_by(polyandry, multinest) %>% 
  summarise(n_cases = n()) %>% 
  group_by(polyandry) %>% 
  mutate(prop_cases = n_cases/sum(n_cases))

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

# sample sizes
renesting_data %>% 
  summarise(Years = n_distinct(year),  # N = 14 years
            Individuals = n_distinct(ring),    # N = 430 females
            Nests = n_distinct(ID),    # N = 850 nests
            Eggs = nrow(.))  # N = 2451 eggs

# number of re-nesting attempts that were polyandrous vs. monogamous
renesting_data %>% 
  group_by(polyandry) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

renesting_data %>% 
  mutate(multinest = ifelse(n_nests > 1, "multi", "single")) %>% 
  group_by(polyandry, multinest) %>% 
  summarise(n_cases = n()) %>% 
  group_by(polyandry) %>% 
  mutate(prop_cases = n_cases/sum(n_cases))

renesting_data %>% 
  summarise(max_date = max(first_laydate, na.rm = TRUE),
            min_date = min(first_laydate, na.rm = TRUE))

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

first_nests_age_data %>% 
  summarise(n_ind = n_distinct(ring),
            n_nests = n_distinct(ID))

# number of 3, 2, or 1 egg nests
ceuta_egg_chick_female_data %>% 
  mutate(ID = as.factor(ID)) %>% 
  group_by(ID) %>% 
  summarise(n_eggs = n()) %>% 
  mutate(n_eggs = as.factor(n_eggs)) %>% 
  group_by(n_eggs) %>% 
  tally() %>% 
  mutate(prop = as.numeric(as.character(n))/sum(n)) %>% 
  collect() %>%
  kable(col.names = c("Clutch size",
                      "Frequency of nests",
                      "Proportion of nests")) %>%
  kable_styling() %>%
  scroll_box(width = "80%")

# summarize egg morphometric data
ceuta_egg_chick_female_data %>% 
  summarise(avg_egg_length = mean(length_cm, na.rm = TRUE),
            sd_egg_length = sd(length_cm, na.rm = TRUE),
            avg_egg_width = mean(width_cm, na.rm = TRUE),
            sd_egg_width = sd(width_cm, na.rm = TRUE),
            avg_egg_volume = mean(volume_cm, na.rm = TRUE),
            sd_egg_volume = sd(volume_cm, na.rm = TRUE)) %>% 
  as.data.frame(.) %>%
  t() %>% 
  as.data.frame(.) %>%
  rownames_to_column("Statistic / Trait") %>% 
  rename(estimate = V1) %>% 
  collect() %>%
  kable() %>%
  kable_styling() %>%
  scroll_box(width = "80%")

# effect size of chick weight ~ egg volume model
load("output/stats_chickw_eggv.rds")
stats_chickw_eggv$tidy
stats_chickw_eggv$partR2

# tally first age distribution of unknown and known-aged individuals
ceuta_egg_chick_female_data %>% 
  group_by(age_first_cap, first_age_t) %>% 
  summarise(n_ind = n_distinct(ring)) %>% 
  group_by(age_first_cap) %>% 
  mutate(prop = n_ind/sum(n_ind),
         first_age_t = first_age_t + 1) %>% 
  collect() %>%
  kable(col.names = c("Age first encountered",
                      "First breeding age",
                      "Frequency of individuals",
                      "Proportion of age group")) %>%
  kable_styling() %>%
  scroll_box(width = "100%")

3/43

ceuta_egg_chick_female_data %>% 
  group_by(age_first_cap) %>% 
  summarise(n = n_distinct(ring))

# summarize tenure and age span
encounter_histories %>% 
  dplyr::filter(ring %in% ceuta_egg_chick_female_data$ring) %>% 
  left_join(., dplyr::select(BaSTA_ages, ring, est_b), by = "ring") %>%
  mutate(est_age = as.numeric(year) - as.numeric(est_b)) %>% 
  group_by(ring) %>% 
  summarise(min_age = min(est_age),
            max_age = max(est_age),
            obs_span = max(est_age) - min(est_age)) %>% 
  as.data.frame() %>% 
  left_join(., dplyr::select(ceuta_egg_chick_female_data, ring, age_first_cap)) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(mean_tenure = mean(obs_span, na.rm = TRUE),
            sd_tenure = sd(obs_span, na.rm = TRUE),
            mean_age_span = mean(max_age, na.rm = TRUE),
            sd_age_span = sd(max_age, na.rm = TRUE),
            median_age_span = median(max_age, na.rm = TRUE),
            min_age_span = min(max_age, na.rm = TRUE),
            max_age_span = max(max_age, na.rm = TRUE))

# summarize age obs per individual
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(n_ages = n_distinct(est_age)) %>% 
  ungroup() %>% 
  summarise(mean_age_obs = mean(n_ages, na.rm = TRUE),
            sd_age_obs = sd(n_ages, na.rm = TRUE),
            median_age_obs = median(n_ages, na.rm = TRUE),
            min_age_obs = min(n_ages, na.rm = TRUE),
            max_age_obs = max(n_ages, na.rm = TRUE))

# yearly nesting interval
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(obs_period = max(est_age) - min(est_age) + 1,
            n_years = n_distinct(year)) %>% 
  mutate(obs_interval = obs_period/n_years) %>% 
  ungroup() %>% 
  summarise(avg_interval = mean(obs_interval),
            sd_interval = sd(obs_interval),
            med_interval = median(obs_interval)) %>% 
  collect() %>%
  kable(col.names = c("Average interval",
                      "SD interval",
                      "Median interval")) %>%
  kable_styling() %>%
  scroll_box(width = "80%")

# summary of seasonal nesting trends
ceuta_egg_chick_female_data %>% 
  summarise(mean_n_nests = mean(n_nests, na.rm = TRUE),
            sd_n_nests = sd(n_nests, na.rm = TRUE),
            median_n_nests = median(n_nests, na.rm = TRUE),
            max_n_nests = max(n_nests, na.rm = TRUE),
            min_n_nests = min(n_nests, na.rm = TRUE))

# sample size summary of chick ~ egg model
eggs_and_chicks_nest_summary <- 
  ceuta_egg_chick_female_data %>% 
  group_by(ID, ring, year, jul_lay_date_std_num, 
           avg_chick_tarsus, sd_chick_tarsus, avg_chick_bill, 
           sd_chick_bill, avg_chick_weight, sd_chick_weight, avg_chick_BMI, 
           sd_chick_BMI) %>% 
  summarise(avg_egg_length = mean(length_cm, na.rm = TRUE),
            sd_egg_length = sd(length_cm, na.rm = TRUE),
            avg_egg_width = mean(width_cm, na.rm = TRUE),
            sd_egg_width = sd(width_cm, na.rm = TRUE),
            avg_egg_volume = mean(volume_cm, na.rm = TRUE),
            sd_egg_volume = sd(volume_cm, na.rm = TRUE),
  ) %>% 
  rename(mother_ring = ring)

eggs_and_chicks_nest_summary %>% 
  dplyr::filter(!is.na(avg_chick_weight)) %>% 
  ungroup() %>% 
  summarise(n_nests = n_distinct(ID),
            n_females = n_distinct(mother_ring))

ceuta_egg_chick_female_data %>% 
  group_by(polyandry) %>% 
  summarise(mean_n_nests = mean(n_nests, na.rm = TRUE),
            sd_n_nests = sd(n_nests, na.rm = TRUE),
            median_n_nests = median(n_nests, na.rm = TRUE),
            max_n_nests = max(n_nests, na.rm = TRUE),
            min_n_nests = min(n_nests, na.rm = TRUE))

 
# number of nests: polyandrous v monogamous
mating_behaviour_nest_n_plot <- 
  ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, year, n_nests, polyandry) %>% 
  distinct() %>% 
  Rmisc::summarySEwithin(.,
                         measurevar = "n_nests",
                         withinvars = "polyandry",
                         idvar = "year",
                         conf.interval = 0.95) %>% 
  mutate(lcl = n_nests - ci,
         ucl = n_nests + ci) %>% 
  ggplot(.) +
  geom_bar(aes(x = polyandry, y = n_nests, fill = polyandry),
           colour = "grey40", stat = "identity", alpha = 0.4,
           width = 0.5) +
  geom_errorbar(width = 0.1, aes(x = polyandry, y = n_nests, 
                                 ymin = lcl, ymax = ucl)) +
  ylab("Number of nests per female per year (mean ± 95% CI)") +
  xlab("Observed mating behaviour\nof a given female in a given year") +
  scale_x_discrete(labels = c("mono" = "Monogamous",
                              "poly" = "Polyandrous")) +
  scale_fill_manual(values = rev(plot_palette_polyandry)) +
  luke_theme +
  theme(legend.position = "none")

ggsave(plot = mating_behaviour_nest_n_plot,
       filename = "products/figures/jpg/mating_behaviour_nest_n_plot.jpg",
       width = 4.5,
       height = 4.5, units = "in")

# number of polyandrous females
ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, year, n_nests, polyandry) %>% 
  distinct() %>% 
  group_by(ring, polyandry) %>% 
  summarise(n_poly = n_distinct(year)) %>% 
  ungroup() %>% 
  group_by(polyandry) %>% 
  tally()

# number of polyandrous females per year
ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, year, polyandry) %>% 
  distinct() %>% 
  group_by(year, polyandry) %>% 
  summarise(n_poly = n_distinct(ring)) %>% 
  pivot_wider(names_from = polyandry, values_from = n_poly) %>% 
  mutate(poly = ifelse(is.na(poly), 0, poly),
         mono = ifelse(is.na(mono), 0, mono)) %>% 
  mutate(poly_incidence = poly/(mono + poly)) %>% 
  ungroup() %>% 
  summarise(max_poly_incidence = max(poly_incidence),
            min_poly_incidence = min(poly_incidence),
            avg_poly_incidence = mean(poly_incidence),
            total_poly_cases = sum(poly))

ceuta_egg_chick_female_data %>% 
  summarise(n_distinct(ring))

76/425

# number of multinest females
ceuta_egg_chick_female_data %>% 
  dplyr::select(ring, year, n_nests, polyandry) %>% 
  distinct() %>% 
  mutate(multinest = ifelse(n_nests > 1, "multi", "single")) %>% 
  group_by(ring, multinest) %>% 
  summarise(n_multinest = n_distinct(year)) %>% 
  arrange(desc(ring)) %>% 
  ungroup() %>% 
  group_by(multinest) %>% 
  tally()

127/425

ceuta_egg_chick_female_data %>% 
  summarise(n_distinct(ring))

76/425

# number of multinest females
ceuta_egg_chick_female_data %>% 
  # dplyr::filter(nest_order == 1) %>% 
  select(ID, ring, year, nest_1_fate, first_laydate, n_mates, n_nests, polyandry, multiclutch) %>% 
  distinct() %>% 
  filter(nest_1_fate != "Hatch" & nest_1_fate != "Unknown") %>% 
  group_by(ring, multiclutch) %>% 
  summarise(n_multinest = n_distinct(year)) %>% 
  arrange(desc(ring)) %>% 
  ungroup() %>% 
  group_by(multiclutch) %>% 
  tally()

55/(55+134)

# number of renesting females per year
ceuta_egg_chick_female_data %>% 
  select(ID, ring, year, nest_1_fate, first_laydate, n_mates, n_nests, polyandry, multiclutch) %>% 
  distinct() %>% 
  filter(nest_1_fate != "Hatch" & nest_1_fate != "Unknown") %>% 
  group_by(year, multiclutch) %>% 
  summarise(n_multi = n_distinct(ring)) %>% 
  pivot_wider(names_from = multiclutch, values_from = n_multi) %>% 
  mutate(multi = ifelse(is.na(multi), 0, multi),
         single = ifelse(is.na(single), 0, single)) %>% 
  mutate(renest_incidence = multi/(single + multi)) %>% 
  ungroup() %>% 
  summarise(max_renest_incidence = max(renest_incidence),
            min_renest_incidence = min(renest_incidence),
            avg_renest_incidence = mean(renest_incidence),
            total_poly_cases = sum(multi))

# tally number of individuals with a total of x nests in the sample (should have minimum of 3)
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(n_measures = n_distinct(ID)) %>% 
  mutate(n_measures = as.factor(n_measures)) %>% 
  group_by(n_measures) %>% 
  tally() %>% 
  collect() %>%
  kable(col.names = c("Number of nests",
                      "Frequency of individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# tally egg observations over age groups
ceuta_egg_chick_female_data %>% 
  group_by(est_age) %>% 
  summarise(n_eggs = n(),
            n_nests = n_distinct(ID),
            n_individuals = n_distinct(ring)) %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  mutate(est_age = as.numeric(est_age) + 1) %>% 
  collect() %>%
  kable(col.names = c("Age",
                      "Observations (i.e., Eggs)",
                      "Nests",
                      "Individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "70%")

# age distribution
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(max_age = max(est_age) + 1,
            min_age = min(est_age) + 1,
            avg_age = mean(est_age + 1)) %>% 
  mutate(age_span = max_age - min_age) %>% 
  ungroup() %>% 
  summarise(max_age = round(max(max_age)),
            min_age = min(min_age),
            grand_avg_max_age = mean(max_age),
            grand_avg_min_age = mean(min_age),
            grand_avg_avg_age = mean(avg_age),
            grand_median_age_span = median(age_span + 1),
            grand_avg_age_span = mean(age_span + 1),
            max_age_span = max(age_span) + 1,
            min_age_span = min(age_span) + 1,
            sd_age_span = sd(age_span)) %>% 
  t() %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  rename(value = V1) %>%
  collect() %>%
  kable() %>% 
  kable_styling() %>%
  scroll_box(width = "70%")

# tally egg observations over age groups
ceuta_egg_chick_female_data %>% 
  group_by(ring) %>% 
  summarise(n_years = n_distinct(year)) %>% 
  ungroup() %>% 
  summarise(avg_years_per_individual = mean(n_years),
            median_years_per_individual = median(n_years),
            max_years_per_individual = max(n_years),
            min_years_per_individual = min(n_years),
            sd_years_per_individual = sd(n_years)) %>% 
  t() %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  collect() %>%
  rename(value = V1) %>%
  kable() %>% 
  kable_styling() %>%
  scroll_box(width = "70%")

ceuta_egg_chick_female_data %>% 
  summarise(max_date = max(jul_lay_date_std_num),
            min_date = min(jul_lay_date_std_num),
            n_days = max(jul_lay_date_std_num) - min(jul_lay_date_std_num))


