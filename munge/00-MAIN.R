# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# SwedeHF data ------------------------------------------------------------

## Load data

datapath <- "data/raw-data/20250115/export/"

### data
newrs <- read_sas(paste0(datapath, "datauttag_arsrapport.sas7bdat"))

### centre
center <- read_sas(paste0(datapath, "rikssvikt_ou.sas7bdat"))

### sex, age
sexage <- read_sas(paste0(datapath, "rikssvikt_pd_dodsdatum.sas7bdat"))

## Munge data

source(here::here("munge/01-clean_missing.R"))
source(here::here("munge/02-fixcenter.R"))
source(here::here("munge/03-fixvars.R"))
source(here::here("munge/04-qi.R"))

## Save data

save(file = here("data/clean-data/rsdata.RData"), list = c(
  "rsdata", "qiinfo"
))

# Coverage ----------------------------------------------------------------

## Load data
tgpath <- "data/raw-data/tg/"

prevtime <- read.xlsx(here(tgpath, "64898_2024 RiksSvikt - TG Prev mot Prev 2010-2024 - Inkl Bidiagnos 2025-05-21_LB.xlsx"),
  sheet = "Län per år"
)
inctime <- read.xlsx(here(tgpath, "64898_2024 RiksSvikt - TG Inc mot Inc 2006-2023 - Registrering inom 1 år 2025-05-21_LB.xlsx"),
  sheet = "Län per år"
)
prev <- read.xlsx(here(tgpath, "64898_2024 RiksSvikt - TG Prev mot Prev 2010-2024 - Inkl Bidiagnos 2025-05-21_LB.xlsx"),
  sheet = "2024"
)
inc <- read.xlsx(here(tgpath, "64898_2024 RiksSvikt - TG Inc mot Inc 2006-2023 - Registrering inom 1 år 2025-05-21_LB.xlsx"),
  sheet = "2023"
)

prev <- prev %>% as_tibble(.name_repair = "unique")
inc <- inc %>% as_tibble(.name_repair = "unique")

## Munge

source(here::here("munge/05-tg.R"))

## Save data

save(file = here("data/clean-data/tg.RData"), list = c(
  "tg", "tg_overtime"
))

# Get map data ------------------------------------------------------------

# swedenmap <- geodata::gadm(country = "SWE", level = 1, path = here("data/raw-data"))
mapdata <- geodata::gadm("SWE", level = 1, path = "./data/clean-data", version = "latest", resolution = 1)
