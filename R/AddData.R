

young_person = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S007_G01_D01_B01_T01.csv")
middle_aged = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S074_G02_D01_B01_T01.csv") 
old_person = data.table::fread("S:/Team Members/Wiles Tyler/Gaitprint/CLEAN DATA/S119_G03_D01_B01_T01.csv")



# Attach column names
header = data.table::fread("C:/Users/jsommerfeld/Desktop/Gaitprint/Gaitprint_Noraxon_Names_Clean.csv", header = F)
colnames(young_person) = as.character(header)
colnames(old_person) = as.character(header)
colnames(middle_aged) = as.character(header)


# Data generally needs to be less than 1mb for it to be accepted by CRAN.... we need to cut some columns out
library(dplyr)

# Young adults
healthy_young = young_person %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

# Have to take the first 10,000 data points of time so that it starts at 0 and not 49.995 seconds
t = healthy_young[1:10000, 1]
healthy_young = healthy_young[10000:19999, 2:18]
healthy_young = cbind(t, healthy_young)
colnames(healthy_young)[1] = "time"


# Middle aged adults
healthy_middle = middle_aged %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

t = healthy_middle[1:10000, 1]
healthy_middle = healthy_middle[10000:19999, 2:18]
healthy_middle = cbind(t, healthy_middle)
colnames(healthy_middle)[1] = "time"

# Old adults
healthy_old = old_person %>% 
  select(-contains(c("mG", "Gyroscope", "Noraxon", "roll", "course", "Pelvic", "Lateral", 
                     "Inversion", "Supination", "abduction", "Rotation", "Cervical", "Wrist", 
                     "Axial", "Upper", "Forearm", "Hand", "Elbow", "Shoulder", "Spine", "Down", "Thoracic")))

t = healthy_old[1:10000, 1]
healthy_old = healthy_old[10000:19999, 2:18]
healthy_old = cbind(t, healthy_old)
colnames(healthy_old)[1] = "time"


# Add data to package -- generally needs to be under 1mb
usethis::use_data(healthy_young, overwrite = T, compress = "xz")
usethis::use_data(healthy_middle, overwrite = T, compress = "xz")
usethis::use_data(healthy_old, overwrite = T, compress = "xz")
