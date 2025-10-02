# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

global_cols <- c(
  "#213067",
  "#bb7cb3",
  "#e6007e",
  "#009fe3",
  "#33ac5a",
  "#95368c",
  "#878787",
  "#f29ec4",
  "#f7a600",
  "#a1daf8",
  "#89c58b",
  "#e6007e",
  "#854f46",
  "#9d9c9c",
  "#fdcb78",
  "#10bbef",
  "#ac8579"
)

global_colsgreymiss <- "#9d9c9c"

global_gridcolor <- rgb(235, 235, 235, 100, maxColorValue = 355)

global_colslimit <- c("#33ac5a", "#f7a600")

global_figfontsize <- 18
global_figfontsizesmall <- 10

# year of report

global_year <- 2024
global_startdtm <- ymd(paste0(global_year, "-01-01"))
global_stopdtm <- ymd(paste0(global_year, "-12-31"))

global_labnams <- c("Year", "Upper target level", "Lower target level", "Unknown")
global_labnamssv <- c("År", "Övre målnivå", "Lägre målnivå", "Okänd")
global_shortttype <- c("Index", "3-month", "1-year", "2+-year")
