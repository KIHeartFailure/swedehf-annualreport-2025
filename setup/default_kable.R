default_kable <- function(tab, font_size = NULL, scale_down = TRUE,
                          row.names = FALSE,
                          align = c(rep("l", ncol(tab))),
                          ...) {
  parms <- list(...)

  if (any(names(parms) == "longtable")) {
    latexoptions <- c("striped", "repeat_header")
  } else {
    if (scale_down) {
      latexoptions <- c("striped", "scale_down")
    } else {
      latexoptions <- c("striped")
    }
  }

  knitr::kable(tab,
    booktabs = TRUE,
    linesep = "",
    row.names = row.names,
    align = align,
    ...
  ) %>%
    kableExtra::kable_styling(
      latex_options = latexoptions,
      font_size = font_size, full_width = F
    )
}
