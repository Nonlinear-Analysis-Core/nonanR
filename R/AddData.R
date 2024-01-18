

young_person = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S007_G01_D01_B01_T01.csv")
middle_aged = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S074_G02_D01_B01_T01.csv") 
old_person = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S119_G03_D01_B01_T01.csv")



# Attach column names
header = data.table::fread("C:/Users/jsommerfeld/Desktop/Gaitprint/Gaitprint_Noraxon_Names_Clean.csv", header = F)
colnames(young_person) = as.character(header)
colnames(old_person) = as.character(header)
colnames(middle_aged) = as.character(header)


# Data generally needs to be less than 1mb for it to be accepted by CRAN.... we need to cut some columns out
var = as.character(header)

# These are the things that I want to remove
# get_rid_of_these = c("mG", "deg", "deg/s")

library(dplyr)
# Young adults
young = young_person %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

# Have to take the first 10,000 data points of time so that it starts at 0 and not 49.995 seconds
t = young[1:10000, 1]
young = young[10000:19999, 2:18]
young = cbind(t, young)


# Middle aged adults
middle = middle_aged %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

t = middle[1:10000, 1]
middle = middle[10000:19999, 2:18]
middle = cbind(t, middle)

# Old adults
old = old_person %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

t = old[1:10000, 1]
old = old[10000:19999, 2:18]
old = cbind(t, old)

# Add data to package -- generally needs to be under 1mb
usethis::use_data(young, overwrite = T, compress = "xz")
usethis::use_data(middle, overwrite = T, compress = "xz")
usethis::use_data(old, overwrite = T, compress = "xz")
