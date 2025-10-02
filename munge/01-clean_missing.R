rep_func_new <- function(var) {
  var <- na_if(var, ".A")
  var <- na_if(var, ".N")
  var <- na_if(var, ".E")
  var <- na_if(var, "")
}

rsdata <- newrs %>%
  mutate(across(where(is.character), rep_func_new))
