qifunc <- function(qi = qitmp, startime = global_startdtm, stoptime = global_stopdtm, type,
                   ll = lltmp, ul = ultmp, data = rsdata, unit = "centre") {
  tmp <- data %>%
    filter(ttype %in% type &
      indexdtm >= startime &
      indexdtm <= stoptime &
      !is.na(!!sym(qi)))

  # riket
  riket <- tmp %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    filter(!!sym(qi) == 1) %>%
    mutate(
      unit = "Sweden",
      byvar = 1
    )

  # per hf duration
  hfdur <- tmp %>%
    filter(!is.na(hfdurimp)) %>%
    group_by(hfdurimp, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 2) %>%
    rename(unit = hfdurimp)

  # per vtyp
  vtype <- tmp %>%
    filter(!is.na(vtype)) %>%
    group_by(vtype, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 3) %>%
    rename(unit = vtype)

  # per centre/county
  unitdata <- tmp %>%
    filter(!is.na(!!sym(unit))) %>%
    group_by(!!sym(unit), .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 4) %>%
    rename(unit = !!sym(unit))

  empty <- riket %>%
    mutate(
      n = 0,
      tot = 0,
      percent = 0,
      unit = "",
      byvar = NA
    )

  unitdata <- unitdata %>%
    arrange(desc(percent), unit)

  all <- bind_rows(riket, hfdur, vtype, empty, unitdata)

  all <- all %>%
    mutate(
      cols = case_when(
        byvar == 4 ~ global_cols[2],
        byvar %in% c(1, 2, 3) ~ global_cols[1],
        is.na(byvar) ~ "white"
      ),
      ntot = if_else(!is.na(byvar), paste0(comma(n), " of ", comma(tot)), ""),
      per = if_else(!is.na(byvar), paste0(fn(percent, 0), "%"), ""),
      row = n():1
    ) %>%
    arrange(desc(row))

  all <- all %>%
    mutate(
      unit = forcats::fct_reorder(unit, row),
      unitpad = paste0(unit, "  ", ntot)
    )

  maxrow <- max(all$row)

  if (maxrow > 60) {
    sizeuse <- 9
    dodgenr <- 2
    breaksx <- c(0, 25, 50, 75, 100)
  }
  if (maxrow <= 60 & maxrow > 30) {
    sizeuse <- 9
    dodgenr <- 1
    breaksx <- seq(0, 100, 20)
  }
  if (maxrow <= 30) {
    sizeuse <- 14
    dodgenr <- 1
    breaksx <- seq(0, 100, 20)
  }

  p <- ggplot(data = all, aes(x = row, y = percent, fill = cols)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c(global_cols[c(1, 2)], "white")) +
    geom_hline(aes(yintercept = ll * 100, linetype = global_labnams[3]), col = global_colslimit[2]) +
    geom_hline(aes(yintercept = ul * 100, linetype = global_labnams[2]), col = global_colslimit[1]) +
    scale_linetype_manual(
      name = "limit", values = c("longdash", "longdash"),
      guide = guide_legend(override.aes = list(color = global_colslimit[c(2, 1)]))
    ) +
    theme_classic() +
    theme(
      text = element_text(size = global_figfontsize),
      legend.position = "bottom",
      legend.margin = margin(0, 100, 0, 0), # move legend to right otherwise outside fig
      legend.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(hjust = 1, colour = "black", size = sizeuse),
      axis.line.y = element_line(colour = "white")
    ) +
    scale_y_continuous(breaks = breaksx, limits = c(0, 100.01), expand = c(0, 0)) +
    scale_x_continuous(breaks = c(all$row), labels = all$unitpad, expand = c(0, 0), sec.axis = dup_axis(labels = all$per), guide = guide_axis(n.dodge = dodgenr)) +
    labs(y = "Proportion (%)")
  p
}
