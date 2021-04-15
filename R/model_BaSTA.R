## BaSTA model selection

# Run the multibasta function to fit all possible survival trends to the data, 
# while specifying the study start year at 2006, study end year at 2020, 
# 800000 iterations, burn-in of 10000, thinning every 2000th iteration, 
# 4 parallel chains, and a minimum age of 1 (i.e., our mark-recapture data 
# only includes individuals that are either adults of unknown age (age >= 1), 
# or individuals that were born locally but were encountered in subsequent years 
# (ie., first-year survival = 1.0)).

library(BaSTA)
library(tidyverse)
library(snowfall)

load(file = "data/BaSTA_checked_life_table_females_2006-2020.rds")

# should be 46 if corrected version of data
length(BaSTA_checked_life_table_females_2006_2020$type6)

multiout_females <-
  multibasta(object = BaSTA_checked_life_table_females_2006_2020$newData,
             studyStart = 2006, studyEnd = 2020, covarsStruct = "fused", 
             minAge = 1, niter = 800000, burnin = 10000, thinning = 2000,
             nsim = 4, parallel = TRUE, ncpus = 4, updateJumps = TRUE)

save(multiout_females,
     file = "R_objects/multibasta_females_min_age_1_2006-2020.rds")