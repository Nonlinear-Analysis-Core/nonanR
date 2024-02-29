

healthy_young = data.table::fread("./data-raw/healthy_young.csv")
healthy_middle = data.table::fread("./data-raw/healthy_middle.csv") 
healthy_old = data.table::fread("./data-raw/healthy_old.csv")

# Add data to package -- generally needs to be under 1mb
usethis::use_data(healthy_young, overwrite = T, compress = "xz")
usethis::use_data(healthy_middle, overwrite = T, compress = "xz")
usethis::use_data(healthy_old, overwrite = T, compress = "xz")

rm(healthy_young)
rm(healthy_middle)
rm(healthy_old)

