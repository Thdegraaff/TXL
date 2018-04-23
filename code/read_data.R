library("tidyverse")

# Read in the STATA dataset titled TIGRIS_LISA_gem2014_sector_tot.dta (LISA dataset aggregates)

lisa <- read_dta(file = "./data/src/TIGRIS_LISA_gem2014_sector_tot.dta")

# Note that data lisa is now already in a tidy format (thank you Hans!)
# But lisa gives only the values, and we want labels for plotting, so:

lisa_factor <- as_factor(lisa) # not sure whether I need this one later

# Now write lisa dataset to csv file

write_csv(lisa, "./data/derived/lisa.csv", col_names = TRUE)
write_csv(lisa_factor, "./data/derived/lisa.csv", col_names = TRUE)

# And clean up memory

rm(list = ls())

