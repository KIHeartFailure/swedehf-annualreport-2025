timefunc <- function(qi = qitmp, starttime = global_year - 7, stoptime = global_year, ll = lltmp, ul = ultmp,
                     data = rsdata, onlyindex = FALSE) {
  tmp <- data %>%
    filter(indexyear %in% paste(seq(starttime, stoptime, 1)) &
      !is.na(!!sym(qi)))

  if (onlyindex) {
    tmp <- tmp %>%
      filter(ttype == "Index")

    byvar <- "vtype"
  }
  if (!onlyindex) {
    byvar <- "ttype"
  }

  datafig <- tmp %>%
    filter(!is.na(!!sym(byvar))) %>%
    group_by(!!sym(byvar), indexyear, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    rename(unit = !!sym(byvar))

  p <- ggplot(datafig, aes(x = indexyear, y = percent, group = unit)) +
    geom_line(aes(col = unit), linewidth = 1.5) +
    geom_point(aes(col = unit), size = 3.5) +
    geom_hline(aes(yintercept = ll * 100, linetype = global_labnams[3]), col = global_colslimit[2]) +
    geom_hline(aes(yintercept = ul * 100, linetype = global_labnams[2]), col = global_colslimit[1]) +
    scale_colour_manual(
      values = global_cols,
      guide = guide_legend(order = 1)
    ) +
    scale_linetype_manual(
      name = "limit", values = c("longdash", "longdash"),
      guide = guide_legend(override.aes = list(color = global_colslimit[c(2, 1)]))
    ) +
    theme_classic() +
    theme(
      text = element_text(size = global_figfontsize),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      panel.grid.major.y = element_line(
        color = global_gridcolor,
        linewidth = 0.5,
        linetype = 1
      )
    ) +
    scale_x_discrete(expand = expansion(add = .1)) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.05, 0.05)) +
    labs(y = "Proportion (%)", x = "Year")
  p
}
