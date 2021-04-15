source("R/project_libraries.R")
source("R/project_functions.R")

# Here we extract all egg morphometric data for individually banded females 
# and merge the estimated ages of the BaSTA analysis.

# #### connect to CeutaCLOSED ####
# CeutaCLOSED <- 
#   dbConnect(SQLite(), dbname = "../Ceuta_Open/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v2-0-0.sqlite")

#### extract females and their nests ####
nest_caps_F <-
  
  # extract snowy plover captures
  dbReadTable(CeutaCLOSED,"Captures") %>% 
  dplyr::filter(species == "SNPL") %>% 
  # dplyr::select(year, ID, ring, sex, age, date, time, weight, bill, left_tarsus, 
  #               right_tarsus, left_wing, right_wing, fat, observer)  %>% 
  
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
  
  # subset to female adults
  dplyr::filter(sex == "F" & age == "A") %>% 
  
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
                                  ifelse(!is.na(found_date), found_date, NA)))), 
             origin = "1970-01-01")) %>% 
  
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
      summarise(firstage = min(est_age) - 1,
                conservative_firstage = min(conservative_age),
                firstage_lower = min(est_age_lower),
                firstage_upper = min(est_age_upper),
                lastage = max(est_age) - 1,
                conservative_lastage = max(conservative_age),
                lastage_lower = max(est_age_lower),
                lastage_upper = max(est_age_upper))
    
    # merge with dataframe
    df2 <- 
      left_join(df, ring_Age, by = "ring") %>% 
      mutate(est_age_trans = est_age - 1,
             
             conservative_age = ifelse(age_first_cap == "J", 
                                       conservative_age - 1, 
                                       conservative_age),
             conservative_firstage = ifelse(age_first_cap == "J", 
                                            conservative_firstage - 1, 
                                            conservative_firstage),
             conservative_lastage = ifelse(age_first_cap == "J", 
                                           conservative_lastage - 1, 
                                           conservative_lastage))
    
    
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
  dplyr::select(ID, jul_lay_date, year) %>% 
  distinct() %>% 
  mutate(
    # scale the julian lay date by year
    jul_lay_date_std = scale_by(jul_lay_date ~ year, ., scale = 0)) %>% 
  
  # make the scaled date variable numeric class
  mutate(jul_lay_date_std_num = as.numeric(jul_lay_date_std)) %>% 
  left_join(eggs_2006_2020, ., 
            by = c("ID", "jul_lay_date", "year")) %>% 
  distinct()

# check if there are some laydate outliers
lay_dates <- 
  eggs_2006_2020 %>% 
  dplyr::select(ID, jul_lay_date_std_num) %>% 
  distinct()
boxplot.stats(lay_dates$jul_lay_date_std_num)
  
# check for nests with more than 4 eggs
eggs_2006_2020 %>% 
  group_by(ID) %>%
  dplyr::summarize(n_eggs = n()) %>%
  arrange(desc(n_eggs))

# check for nests with more than 1 female
eggs_2006_2020 %>% 
  group_by(ID) %>%
  summarise(n_rings = n_distinct(ring)) %>%
  arrange(desc(n_rings))

# check for nests with more than 1 egg1, egg2, or egg3
eggs_2006_2020 %>% 
  group_by(ID, egg) %>%
  summarise(n_obs = n()) %>%
  arrange(desc(n_obs))

# check for nests with more than 1 lay date
eggs_2006_2020 %>% 
  group_by(ID) %>%
  summarise(n_dates = n_distinct(jul_lay_date)) %>%
  arrange(desc(n_dates))

#### 2020_D_1 with two females ----
dplyr::filter(nest_caps_F, ID == "2020_D_1") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

# 2020_D_1 nest in birdref
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2020_D_1")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CA3318")

# 2020_D_1 nest in nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_1")

dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_H_8")

# CN0063 female in Captures
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring %in% c("CN0063", "CN0606"))

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("BX.RM|WX.LX"))

#### Remove CN0116 ----
# two females with the same nest ID (CN0116 and CN0118)
dplyr::filter(nest_caps_F, ID == "2018_C_1") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

# CN0118 female in birdref
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2018_C_1")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2018_C_1")

# CN0118 is MX.RW|LX.RX but there is a mistake in 2018 (two rows one for CN0118 and one for CN0116) NEED TO FIX IN DATABASE
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code == "MX.RW|LX.RX" & year == 2018)

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0116" & ID == "2018_C_1")

######### Remove CN0424
# two females with the same nest ID (CN0215 and CN0424)
dplyr::filter(nest_caps_F, ID == "2018_E_301") %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male)

dplyr::filter(nest_caps_F, ring %in% c("CN0215", "CN0424")) %>% 
  dplyr::select(ID, ring, code, sex, ad_cap_date, lay_date, male) %>% 
  arrange(ID) %>% 
  distinct

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"BirdRef") %>%
  dplyr::filter(ID == "2018_E_301")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2018_E_301")

# CN0118 is MX.RW|LX.RX but there is a mistake in 2018 (two rows one for CN0118 and one for CN0116) NEED TO FIX IN DATABASE
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("LX.RM|BX.RX", "WX.RM|LX.YX"))

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0424")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ring == "CN0215")

########## Remove 2020_C_4 measured by Diego
# two females with the same nest ID (CN0215 and CN0424)
dplyr::filter(nest_caps_F, ID == "2020_C_4") %>% 
  View()
dplyr::select(ID, ring, lay_date, no_chicks, length, width, egg)

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_C_4") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ID == "2020_C_4") %>% 
  plover_date_convert(input = "Rdate")

#### Check the super early lay date of 2020_D_101 (looks fine)
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_101") %>% 
  plover_date_convert(input = "Rdate")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_104")

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_104") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(ID == "2020_D_104") %>% 
  plover_date_convert(input = "Rdate")

# MX.RW|LX.RX female in Nests
dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("OX.RM|RX.RX", "MX.RY|OX.BX") & year == "2020" & species == "SNPL") %>% 
  plover_date_convert(input = "Rdate")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_12")

eggdf %>%
  dplyr::filter(ID == "2020_D_12" & length %in% c(31.5, 31.4)) %>% 
  dplyr::filter(ID == "2020_D_12")

########## 
# two females with the same nest ID (CN0215 and CN0424)
nest_caps_F %>%
  dplyr::filter(ID == "2020_D_201")

# CN0215 female in birdref and CN0424 assigned as a male
dbReadTable(CeutaCLOSED,"Nests") %>%
  dplyr::filter(ID == "2020_D_201") %>% 
  plover_date_convert(input = "Rdate")

dbReadTable(CeutaCLOSED,"Captures") %>%
  dplyr::filter(code %in% c("OX.RM|GX.GX", "WX.RM|GX.YX") & year == "2020" & species == "SNPL") %>% 
  plover_date_convert(input = "Rdate")

eggdf_2006_2020_cleaned %>% 
  dplyr::filter(ring %in% c("CN0155", "CN0379", "CN0478"))

dbReadTable(CeutaCLOSED,"Captures") %>% 
  dplyr::filter(ring %in% c("CN0155", "CN0379", "CN0478") & year == "2019")

eggdf_2006_2020 %>% 
  dplyr::select(ring, ID, est_age) %>% 
  distinct()

boxplot(y = eggs_2006_2020$volume, x = eggs_2006_2020$year, boxwex = 0.1)
boxplot.stats(eggs_2006_2020$volume)$out

eggs_2006_2020 %>% 
  group_by(year) %>%
  dplyr::filter(!volume %in% boxplot.stats(volume)$out) %>%
  ggplot(., aes(year, volume)) +
  geom_boxplot()

eggs_2006_2020 %>% 
  ggplot(., aes(year, volume)) +
  geom_jitter()

eggs_2006_2020 %>% 
  # group_by(year) %>%
  # dplyr::filter(!eggv %in% boxplot.stats(eggv)$out) %>%
  ggplot(., aes(year, volume)) +
  geom_boxplot()

## Sample check
# Here we double check the sample to make sure that each individual and nest meets our criteria
# tally number of nests with 1, 2, or 3 eggs (should have maximum of 3)
eggdf_2006_2020 %>% 
  mutate(ID = as.factor(ID)) %>% 
  group_by(ID) %>% 
  summarise(n_eggs = n()) %>% 
  mutate(n_eggs = as.factor(n_eggs)) %>% 
  group_by(n_eggs) %>% 
  tally() %>% 
  collect() %>%
  kable(col.names = c("Clutch size",
                      "Frequency of nests")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# tally number of individuals with 3, 4, etc. years of observations (should have minimum of 3)
eggdf_2006_2020 %>% 
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

# tally number of individuals with a total of x nests in the sample (should have minimum of 3)
eggdf_2006_2020 %>% 
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

# total sample sizes of dataset
eggdf_2006_2020 %>% 
  summarise(Years = n_distinct(year),  # N = 14 years
            Individuals = n_distinct(ring),    # N = 430 females
            Nests = n_distinct(ID),    # N = 850 nests
            Eggs = nrow(.)) %>%  # N = 2451 eggs
  t(.) %>% 
  as.data.frame() %>% 
  rename(n = V1) %>% 
  collect() %>%
  kable(col.names = c("Sample size")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# tally egg observations over age groups
eggdf_2006_2020 %>% 
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
eggdf_2006_2020 %>% 
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
eggdf_2006_2020 %>% 
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

save(ceuta_egg_chick_female_data,
     file = "data/ceuta_egg_chick_female_data.rds")