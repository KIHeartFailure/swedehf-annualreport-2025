# load packages, globals and project specific functions
source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/rsdata.RData"))

rsdatacenter <-
  rsdata %>%
  filter(location != "Primary care" & indexyear == 2023) %>%
  distinct(centre)

rsdataregion <-
  rsdata %>%
  filter(location != "Primary care" & indexyear == 2023) %>%
  distinct(county)
