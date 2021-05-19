source("R/project_libraries.R")
source("R/project_functions.R")

# Here we extract all egg morphometric data for individually banded females 
# and merge the estimated ages of the BaSTA analysis.

load("output/BaSTA_age_estimates_2006-2020.rds")

#### extract females and their nests ####
nest_caps_F <-
  
  # extract snowy plover captures of adult females
  dbReadTable(CeutaCLOSED,"Captures") %>% 
  dplyr::filter(species == "SNPL" & sex == "F" & age == "A") %>% 
  
  # summarise body morphometric data with the earliest capture of each nest
  group_by(ID, ring, code, sex, age) %>% 
  summarise(ad_cap_date = date[which.min(date)],
            ad_cap_time = time[which.min(date)],
            ad_weight = weight[which.min(date)],
            left_tarsus = mean(left_tarsus, na.rm = TRUE),
            right_tarsus = mean(right_tarsus, na.rm = TRUE),
            ad_bill = mean(bill, na.rm = TRUE),
            ad_fat = fat[which.min(date)]) %>% 
  mutate(ad_tarsi = mean(c(left_tarsus, right_tarsus), na.rm = TRUE)) %>% 
  group_by(ring) %>% 
  mutate(avg_ad_tarsi = mean(ad_tarsi, na.rm = TRUE),
         sd_ad_tarsi = sd(ad_tarsi, na.rm = TRUE),
         n_ad_tarsi = sum(!is.na(ad_tarsi)),
         avg_ad_weight = mean(ad_weight, na.rm = TRUE),
         sd_ad_weight = sd(ad_weight, na.rm = TRUE),
         n_ad_weight = sum(!is.na(ad_weight))) %>% 
  
  # join the nest data
  left_join(., dplyr::filter(dbReadTable(CeutaCLOSED, "Nests"), species == "SNPL"), 
            by = "ID") %>% 
  
  # remove repeated observations of nests 2020_C_4 and 2020_D_201 that were done
  # for training purposes in the field
  dplyr::filter(ID != "2020_C_4" | observer != "DVR") %>% 
  dplyr::filter(ID != "2020_D_201" | observer != "MC") %>%
  
  # remove duplicated rows
  distinct() %>% 
  
  # set dates as numeric
  mutate(nest_initiation_date = as.numeric(nest_initiation_date),
         end_date = as.numeric(end_date),
         found_date = as.numeric(found_date),
         year = as.factor(year)) %>% 
  
  # change to as.Date format
  plover_date_convert(input = "Rdate") %>%
  
  # specify the lay date as the nest initiation date. If this is unknown, then
  # subtract 25 days from the end date if the nest hatched. If this is not the case,
  # then subtract 11 days from the found date if the float score of the first 
  # egg is "F". If this isn't the case, then take the found date.
  mutate(lay_date = 
           as.Date(
             ifelse(!is.na(nest_initiation_date), nest_initiation_date,
                    ifelse((!is.na(end_date) & fate == "Hatch"), end_date - 25, 
                           ifelse((!is.na(found_date) & float1 == "F"), found_date - 11,
                                  ifelse(!is.na(found_date), found_date + 17, NA)))), 
             origin = "1970-01-01"),
         lay_date_method = 
           ifelse(!is.na(nest_initiation_date), "nest_initiation_date",
                    ifelse((!is.na(end_date) & fate == "Hatch"), "end_date - 25", 
                           ifelse((!is.na(found_date) & float1 == "F"), "found_date - 11",
                                  ifelse(!is.na(found_date), "found_date", NA))))) %>% 
  
  # create a julian lay date
  mutate(jul_lay_date = as.numeric(format(lay_date, "%j"))) %>% 
  
  # remove any duplicated rows resulting from joining above
  distinct() %>% 
  
  # mutate(
  #   # scale the julian lay date by year
  #   jul_std_date = scale_by(jul_lay_date ~ year, ., scale = 0)) %>%
  
  # remove any rows without lay date information
  dplyr::filter(!is.na(jul_lay_date)) %>% 
  
  # remove rows with NA egg dimensions
  dplyr::filter(!is.na(width1) & !is.na(length1))

# determine how many years of observations each female has
n_years <- 
  nest_caps_F %>% 
  group_by(ring) %>% 
  summarise(n_years_obs = n_distinct(year))

# join repeated meausres info and consolidate dataframe
nest_caps_F <-
  left_join(nest_caps_F, n_years, by = "ring") %>% 
  dplyr::select(-left_tarsus, -right_tarsus, -species, -population, 
                -site, -nest, -found_date, -found_time, -nest_initiation_date, 
                -photo, -observer, -comments)

# extract dimensions of egg 1
nest_caps_F_egg1 <- 
  nest_caps_F %>% 
  dplyr::select(-width2, -width3, 
                -length2, -length3,
                -float2, -float3) %>%
  rename(width = width1,
         length = length1,
         float = float1)

# extract dimensions of egg 2
nest_caps_F_egg2 <- 
  nest_caps_F %>% 
  dplyr::select(-width1, -width3, 
                -length1, -length3,
                -float1, -float3) %>%
  rename(width = width2,
         length = length2,
         float = float2)

# extract dimensions of egg 3
nest_caps_F_egg3 <- 
  nest_caps_F %>% 
  dplyr::select(-width2, -width1, 
                -length2, -length1,
                -float2, -float1) %>%
  rename(width = width3,
         length = length3,
         float = float3)

# bind all egg measurements and remove NA observations
nest_caps_F <- 
  bind_rows(nest_caps_F_egg1, 
            nest_caps_F_egg2, 
            nest_caps_F_egg3) %>% 
  dplyr::filter(!is.na(width) | !is.na(length)) %>% 
  
  # calculate egg volume
  mutate(volume = 0.486 * length * width^2,
         year = as.integer(as.character(year))) %>% 
  
  # transform measures to cm
  mutate(length_cm = length/10,
         width_cm = width/10,
         volume_cm = volume/1000) %>% 
  
  # sort by ring and nest ID and remove duplicated rows
  arrange(ring, ID) %>% 
  distinct() %>% 
  
  # merge with basta age estimates
  left_join(., BaSTA_ages, by = "ring") %>% 
  
  mutate(
    # calculate the estimated age at a given year
    est_age = year - est_b,
    
    # calculate upper and lower 95% CI for age at a given year
    est_age_lower = year - upper_b,
    est_age_upper = year - lower_b,
    
    est_death_age = upper_d) %>% 
  
  # remove duplicate rows and extraneous columns
  distinct()

# extract egg laying order from float scores
first_egg <- 
  nest_caps_F %>% 
  arrange(ID, desc(float)) %>% 
  group_by(ID) %>% 
  slice(1) %>% 
  mutate(egg = "egg1")

second_egg <-
  nest_caps_F %>% 
  arrange(ID, desc(float)) %>% 
  group_by(ID) %>% 
  slice(2) %>%
  mutate(egg = "egg2")

third_egg <-
  nest_caps_F %>% 
  arrange(ID, desc(float)) %>% 
  group_by(ID) %>% 
  slice(3) %>%
  mutate(egg = "egg3")

nest_caps_F <- 
  bind_rows(first_egg, 
            second_egg, 
            third_egg)

distinct_first_eggs <- 
  nest_caps_F %>% 
  dplyr::filter(!is.na(float)) %>% 
  dplyr::filter(egg %in% c("egg1", "egg2")) %>% 
  group_by(ID) %>% 
  summarise(n_float_scores = n_distinct(float),
            first_egg = nth(float, 1),
            second_egg = nth(float, 2)) %>% 
  arrange(desc(n_float_scores))

nest_caps_F <- 
  left_join(nest_caps_F, dplyr::select(distinct_first_eggs, 
                                       ID, n_float_scores), by = "ID") %>% 
  rename(old_egg1 = n_float_scores) %>% 
  mutate(old_egg1 = ifelse(old_egg1 == 2, 1, 0))

# extract the first, second, third, and fourth nests for each individual in a
# given year
first_nests <- 
  nest_caps_F %>% 
  dplyr::select(ring, year, ID, jul_lay_date) %>%
  arrange(ring, year, jul_lay_date) %>%
  distinct() %>% 
  group_by(ring, year) %>% 
  slice(1) %>% 
  mutate(nest_order = 1) %>% 
  dplyr::select(-jul_lay_date) %>% 
  mutate(ring_year = paste(ring, year, sep = "_"))

second_nests <- 
  nest_caps_F %>% 
  dplyr::select(ring, year, ID, jul_lay_date) %>%
  arrange(ring, year, jul_lay_date) %>%
  distinct() %>% 
  group_by(ring, year) %>% 
  slice(2) %>% 
  mutate(nest_order = 2) %>% 
  dplyr::select(-jul_lay_date) %>% 
  mutate(ring_year = paste(ring, year, sep = "_"))

third_nests <- 
  nest_caps_F %>% 
  dplyr::select(ring, year, ID, jul_lay_date) %>%
  arrange(ring, year, jul_lay_date) %>%
  distinct() %>% 
  group_by(ring, year) %>% 
  slice(3) %>% 
  mutate(nest_order = 3) %>% 
  dplyr::select(-jul_lay_date) %>% 
  mutate(ring_year = paste(ring, year, sep = "_"))

fourth_nests <- 
  nest_caps_F %>% 
  dplyr::select(ring, year, ID, jul_lay_date) %>%
  arrange(ring, year, jul_lay_date) %>%
  distinct() %>% 
  group_by(ring, year) %>% 
  slice(4) %>% 
  mutate(nest_order = 4) %>% 
  dplyr::select(-jul_lay_date) %>% 
  mutate(ring_year = paste(ring, year, sep = "_"))

# bind all together into a single dataframe
nest_caps_F <- 
  bind_rows(first_nests, second_nests, third_nests, fourth_nests) %>% 
  left_join(nest_caps_F, ., by = c("ring", "year", "ID"))

# determine the fate of the first nest of the season for each female
nest_caps_F <- 
  nest_caps_F %>% 
  group_by(ring_year) %>% 
  summarise(nest_1_fate = fate[which.min(jul_lay_date)]) %>% 
  left_join(nest_caps_F, ., by = "ring_year")

# Determine the age at first capture for females in the data
age_at_first_cap_info <- 
  dbReadTable(CeutaCLOSED, "Captures") %>%
  dplyr::filter(ring %in% nest_caps_F$ring) %>%
  plover_date_convert(input = "Rdate") %>%
  group_by(ring) %>%
  summarise(age_first_cap = age[which.min(year)],
            year_first_cap = as.numeric(year[which.min(year)])) %>%
  collect() %>% 
  
  # manually assign J to CA2036 and CA1526 which were captured as chicks during 
  # the small study in 2004
  mutate(age_first_cap = ifelse(ring %in% c("CA2036", "CA1526"), "J", age_first_cap))

# join the age at first capture info to the data
nest_caps_F <- 
  left_join(nest_caps_F, age_at_first_cap_info, 
            by = "ring") %>% 
  mutate(year_first_cap = ifelse(age_first_cap == "J", est_b, year_first_cap)) %>% 
  mutate(conservative_age = year - year_first_cap)

# Need to check these four nests
nest_caps_F %>% 
  dplyr::filter(code != female) %>%
  dplyr::select(year, ID, male, female, ring, code) %>%  
  distinct
nest_caps_F %>% 
  dplyr::filter(code == male) %>%
  dplyr::select(year, ID, male, female, ring, code) %>%  
  distinct

# check tarsus measurements
nest_caps_F %>% 
  # dplyr::filter(sd_ad_tarsi > 1) %>% 
  dplyr::select(ring, avg_ad_tarsi, n_ad_tarsi, sd_ad_tarsi) %>% 
  distinct() %>% 
  arrange(desc(sd_ad_tarsi))

# extract polyandry and multiclutching information
nest_caps_F <-
  nest_caps_F %>% 
  dplyr::select(year, ID, male, female, ring) %>%
  arrange(female) %>%
  group_by(female, ring, year) %>%
  summarise(n_mates = n_distinct(male, na.rm = TRUE),
            n_nests = n_distinct(ID, na.rm = TRUE)) %>% 
  mutate(polyandry = ifelse(n_mates > 1, "poly", "mono"),
         multiclutch = ifelse(n_nests > 1, "multi", "single"),
         year = as.integer(year),
         n_mates = ifelse(n_mates == 0, 1, n_mates)) %>% 
  left_join(nest_caps_F, ., 
            by = c("year", "ring", "female")) %>% 
  distinct

# add average, first, and last age information to each ring
age_summary <- 
  function(df){
    
    # extract the average, first, and last age for each individual
    ring_Age <- 
      df %>%
      dplyr::select(ring, est_age, conservative_age, 
                    est_age_lower, est_age_upper) %>%
      distinct() %>% 
      group_by(ring) %>% 
      summarise(first_age_t = min(est_age) - 1,
                conservative_first_age = min(conservative_age),
                first_age_lower = min(est_age_lower),
                first_age_upper = min(est_age_upper),
                last_age_t = max(est_age) - 1,
                conservative_last_age = max(conservative_age),
                last_age_lower = max(est_age_lower),
                last_age_upper = max(est_age_upper))
    
    # merge with dataframe
    df2 <- 
      left_join(df, ring_Age, by = "ring") %>% 
      mutate(est_age_t = est_age - 1,
             conservative_age = ifelse(age_first_cap == "J", 
                                       conservative_age - 1, 
                                       conservative_age),
             conservative_first_age_t = ifelse(age_first_cap == "J", 
                                               conservative_first_age - 1, 
                                               conservative_first_age),
             conservative_last_age_t = ifelse(age_first_cap == "J", 
                                              conservative_last_age - 1, 
                                              conservative_last_age)) %>% 
      mutate(est_age_t_deviation = est_age_t - first_age_t)
    
    
    return(df2)
    
  }

# apply age summary to data
nest_caps_F <-
  nest_caps_F %>% 
  age_summary(df = .) %>% 
  mutate(year = as.factor(year),
         ID = as.factor(ID),
         ring = as.factor(ring)) %>% 
  distinct()

eggs_2006_2020 <-
  nest_caps_F %>% 
  ungroup() %>% 
  mutate(ID = as.character(ID),
         ring = as.character(ring)) %>%
  
  # remove CN0424 since sex is unclear
  dplyr::filter(ring != "CN0424") %>% 
  
  # remove outlier egg measurements below 6000 cubic mm
  dplyr::filter(volume > 6000) #%>%

eggs_2006_2020 <- 
  eggs_2006_2020 %>% 
  dplyr::select(ID, jul_lay_date, year, ring_year) %>% 
  distinct() %>% 
  mutate(
    # scale the julian lay date by year
    jul_lay_date_std = scale_by(jul_lay_date ~ year, ., scale = 0)) %>% 
  
  # make the scaled date variable numeric class
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std)) %>% 
  group_by(ring_year) %>% 
  arrange(ring_year, jul_lay_date) %>% 
  mutate(laydate_deviation = jul_lay_date_std_num - jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         first_laydate = jul_lay_date_std_num[which.min(jul_lay_date_std_num)],
         last_laydate = jul_lay_date_std_num[which.max(jul_lay_date_std_num)]) %>% 
  left_join(eggs_2006_2020, ., 
            by = c("ID", "jul_lay_date", "year", "ring_year")) %>% 
  distinct()

#### Chick data wrangle ----
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
  rename(mother_ring = ring)

# summarize chick morphometric data by nest and filter to only include chicks
# measured at age 0 or 1
chick_size_summary <- 
  chicks_2006_2020 %>% 
  arrange(chick_ring, age) %>% 
  group_by(chick_ring) %>% 
  slice(1) %>% 
  dplyr::filter(age %in% c(0, 1)) %>% 
  group_by(ID) %>% 
  summarise(avg_chick_tarsus = mean(tarsus, na.rm = TRUE),
            sd_chick_tarsus = sd(tarsus, na.rm = TRUE),
            avg_chick_bill = mean(bill, na.rm = TRUE),
            sd_chick_bill = sd(bill, na.rm = TRUE),
            avg_chick_weight = mean(weight, na.rm = TRUE),
            sd_chick_weight = sd(weight, na.rm = TRUE),
            avg_chick_BMI = mean(BMI, na.rm = TRUE),
            sd_chick_BMI = sd(BMI, na.rm = TRUE)) %>% 
  
  # remove questionable outlier chick measurements (hatch date likely incorrect)
  dplyr::filter(ID != "2020_H_6")

# join the summarised data together by nest
ceuta_egg_chick_female_data <- 
  left_join(eggs_2006_2020, chick_size_summary, by = "ID")

# save(ceuta_egg_chick_female_data,
#      file = "data/ceuta_egg_chick_female_data.rds")