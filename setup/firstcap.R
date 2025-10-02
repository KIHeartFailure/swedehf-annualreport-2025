firstcap <- function(txt) {
  stringr::str_replace(txt, "([[:alpha:]])", toupper)
}
