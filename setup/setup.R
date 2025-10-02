source(here::here("setup/libs.R"))

# source(here("setup/globals.R"))

# project specific functions
funcs <- list.files(path = here::here("setup"))
funcs <- funcs[!funcs %in% c("setup.R", "libs.R")]
sourcefunc <- function(x) {
  source(here::here("setup/", x))
}
sapply(funcs, sourcefunc)
