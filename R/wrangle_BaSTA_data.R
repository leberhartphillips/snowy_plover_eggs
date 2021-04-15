source("R/project_functions.R")
source("R/project_libraries.R")
source("R/project_plotting.R")

## ---- BaSTA_wrangle --------
# Wrangle the capture data
captures <-
  # extract the capture datatable
  dbReadTable(CeutaCLOSED,"Captures") %>% 
  
  # subset to only include snowy plovers
  dplyr::filter(species == "SNPL") %>% 
  
  # group by individual
  group_by(ring) %>%
  
  # determine the first year that an individual was seen
  mutate(originalsight = ifelse(age == "J", year,
                                year[which.min(as.numeric(year))])) %>% 
  
  # determine if the first encouter was as an adult or a juvenile
  mutate(chick = age[which.min(as.numeric(year))]) %>% 
  
  # specify first encounters as juveniles as recruits and adults as immigrants
  mutate(recruit = ifelse(chick == "J", "Recruit", "Immigrant")) %>% 
  
  # if a recruit then specify the year at which it was first encountered (i.e., it's birth year); 0 for immigrants
  mutate(birth = ifelse(recruit == "Recruit", originalsight, "0")) %>% 
  
  # remove 13 rows that do not have ring information (i.e., these are "observation only" captures of XX.XX|XX.XX birds)
  dplyr::filter(ring != "NA") %>%
  
  # subset to females
  dplyr::filter(sex == "F")

# Assess the sample size of the capture population
# assess how many individuals we have: Female = 860
captures %>%  
  group_by(sex) %>% 
  summarise(count = n_distinct(ring)) %>% 
  collect() %>%
  kable(col.names = c("Sex",
                      "Number of individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# assess how many immigrants and recruits we have:
# Immigrant Females = 406, Recruit Females = 454
captures %>%
  group_by(recruit, sex) %>%
  summarise(n_inds = n_distinct(ring)) %>% 
  collect() %>%
  kable(col.names = c("Status",
                      "Sex",
                      "Number of individuals")) %>%
  kable_styling() %>%
  scroll_box(width = "50%")

# Wrangle the resight data
resightings <- 
  # extract the resight datatable
  dbReadTable(CeutaCLOSED,"Resights") %>% 
  
  # subset to only include snowy plovers
  dplyr::filter(species == "SNPL") %>% 
  
  # merge the ring identity to the resight codes
  left_join(., dplyr::select(captures, ring, code), by = "code") %>% 
  
  # remove duplicates
  distinct()

# extract combinations that are unique
unique_combos <- 
  resightings %>% 
  
  # remove all combos without a distinct set of rings (i.e., needs at most 4 X's)
  dplyr::filter(str_count(code, "X") < 5) %>% 
  
  # remove all combos with uncertainty (i.e., no ?'s)
  dplyr::filter(str_detect(code, "\\?", negate = TRUE)) %>%
  
  # remove all combos without the appropriate number of rings
  dplyr::filter(nchar(code) == 11) %>% 
  
  # extract the combos that have only one metal ring associate with them
  group_by(code) %>% 
  summarise(n_obs = n_distinct(ring)) %>% 
  arrange(desc(n_obs)) %>% 
  dplyr::filter(n_obs == 1)

# Tidy up capture and resight data
resightings <- 
  resightings %>% 
  
  # subset resightings to only include observations of unique combinations
  dplyr::filter(code %in% unique_combos$code) %>% 
  
  # remove all combos that don't have a ring associated with them
  dplyr::filter(!is.na(ring)) %>% 
  
  # sort by ring and year to assess the output
  arrange(ring, year)

capture_final <- 
  captures %>%
  
  # paste ring and year together to make a unique identifier for this observation
  unite(ring_year, ring, year, remove = FALSE) %>%
  
  # specify as a capture
  mutate(observation = "capture") %>%
  
  # clean up output
  dplyr::select(ring_year, sex, ring, year, recruit, birth, observation) %>% 
  arrange(ring, year)

resightings_final <- 
  resightings %>%
  
  # paste ring and year together to make a unique identifier for this observation
  unite(ring_year, ring, year, remove = FALSE) %>%
  
  # specify as a resight
  mutate(observation = "resight") %>%
  
  # clean up output
  dplyr::select(ring_year, ring, year, observation) %>% 
  
  # join with the capture data to merge recruit status, sex, and birth year
  left_join(., dplyr::select(capture_final, ring, recruit, birth, sex), by = "ring") %>% 
  
  # remove duplicates
  distinct()

# Bind capture and resight data into a complete encounter history
encounter_histories <- 
  bind_rows(capture_final, resightings_final) %>% 
  arrange(ring_year)

# Remove resight encounters that occurred prior to the first capture
# determine the year of first capture for each individual
first_cap <- 
  capture_final %>%
  group_by(ring) %>%
  dplyr::filter(as.numeric(year) == min(as.numeric(year))) %>%
  dplyr::select(ring, year) %>%
  distinct(ring,.keep_all = TRUE) %>%
  rename(first_cap = year) %>% 
  arrange(first_cap)

# determine which resights occurred before the first capture
resights_before_first_capture <- 
  encounter_histories %>% 
  left_join(., first_cap, by = "ring") %>% 
  dplyr::filter(observation == "resight" & (as.numeric(year) < as.numeric(first_cap)))

# exclude early resightings from encounter history
encounter_histories <-
  encounter_histories %>% 
  dplyr::filter(ring_year %!in% resights_before_first_capture$ring_year) %>% 
  arrange(ring, as.numeric(year))

# Make the encounter history table needed for the survival analysis
encounter_history_table <- 
  distinct(encounter_histories, ring_year, .keep_all = TRUE) %>% 
  dplyr::select(ring, year) %>%
  arrange(ring) %>% 
  mutate(year = as.integer(year)) %>%
  CensusToCaptHist(ID = .$ring,
                   d = .$year, 
                   timeInt = "Y") %>% 
  mutate(ring = rownames(.),
         ID = as.character(ID))

# Extract the known birth and death information for each individual
birth_death_mat <- 
  encounter_histories %>%
  dplyr::select(ring, birth) %>%
  mutate(death = 0) %>%
  arrange(ring) %>%
  distinct(ring, .keep_all = TRUE)

# Unite the encounter history table with the birth and death matrices
raw_life_table_females_2006_2020 <-
  left_join(birth_death_mat, encounter_history_table, by = "ring") %>%
  as.data.frame() %>% 
  distinct() %>% 
  dplyr::select(ID, birth, death,
                "2006", "2007", "2008", "2009",
                "2010", "2011", "2012", "2013", 
                "2014", "2015", "2016", "2017",
                "2018", "2019", "2020", ring) %>%
  mutate_at(vars(-ring), as.numeric) %>% 
  mutate(sum_years = rowSums(.[4:18])) %>%
  dplyr::filter(sum_years > 1 | birth == 0) %>%
  mutate(ID = as.numeric(row.names(.))) %>%
  dplyr::select(-sum_years)

# Run "DataCheck" BaSTA function to make final cleans to encounter history. The 
# only change needed is that the birth year of recruits should be '0' instead 
# of '1'; this function will solve this issue and provide a cleaned version 
# ready for analysis. In total, we have 918 encounters of 452 individually 
# marked females that were seen as adults, of which 46 are of known birth year.
BaSTA_checked_life_table_females_2006_2020 <- 
  DataCheck(object = raw_life_table_females_2006_2020[, -c(length(raw_life_table_females_2006_2020))], 
            studyStart = 2006, studyEnd = 2020,
            autofix = rep(1, 7), silent = FALSE)

## ---- BaSTA_lifetable_save --------
# save cleaned encounter history file, now ready for BaSTA
save(BaSTA_checked_life_table_females_2006_2020,
     file = "data/BaSTA_checked_life_table_females_2006-2020.rds")

save(raw_life_table_females_2006_2020,
     file = "data/raw_life_table_females_2006_2020.rds")