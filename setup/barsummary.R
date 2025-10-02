barsummary <- function(qi = qitmp,
                       starttime = global_year - 2, stoptime = global_year,
                       ll = lltmp, ul = ultmp,
                       unit,
                       data = rsdata) {
  tmp <- data %>%
    filter(!is.na(!!sym(qi)) &
      vtype %in% unit)

  all <- tmp %>%
    group_by(ttype, indexyear, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1 &
      indexyear %in% paste(seq(starttime, stoptime, 1)))

  all <- all %>%
    mutate(
      cols = case_when(
        indexyear == starttime ~ global_cols[1],
        indexyear == starttime + 1 ~ global_cols[2],
        TRUE ~ global_cols[3]
      ),
      ntot = paste0(comma(n), " of ", comma(tot)),
      per = paste0(percent, "%"),
      per = if_else(tot < 10, "", per),
      ntot = if_else(tot < 10, "<10", ntot),
      percent = if_else(tot < 10, 0, percent),
      row = 1:n(),
      indexyear = as.character(indexyear),
      ttype = fct_inorder(factor(str_replace_all(ttype, " follow-up", "\nfollow-up")))
    )

  p <- ggplot(data = all, aes(x = ttype, y = percent, fill = indexyear)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtext::geom_richtext(aes(x = ttype, y = 1, label = ntot, group = indexyear), position = position_dodge(0.9), fill = "white", hjust = 0, size = (global_figfontsizesmall + 3) / .pt, angle = 90) +
    scale_fill_manual(
      values = global_cols,
      guide = guide_legend(order = 1)
    ) +
    geom_hline(aes(yintercept = ll * 100, linetype = global_labnams[3]), col = global_colslimit[2]) +
    geom_hline(aes(yintercept = ul * 100, linetype = global_labnams[2]), col = global_colslimit[1]) +
    scale_linetype_manual(
      name = "limit", values = c("longdash", "longdash"),
      guide = guide_legend(override.aes = list(color = global_colslimit[c(2, 1)]), order = 2)
    ) +
    theme_classic() +
    theme(
      text = element_text(size = global_figfontsize),
      legend.position = "bottom",
      legend.box.margin = margin(c(0, 0, 0, 0)),
      legend.box = "vertical",
      legend.title = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      # axis.text.x = element_text(colour = "black"),
      axis.line.x = element_line(colour = "white"),
      panel.grid.major.y = element_line(
        color = global_gridcolor,
        linewidth = 0.5,
        linetype = 1
      )
    ) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(y = "Proportion (%)")
  p
}
