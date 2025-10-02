barfunc <- function(var, startime = global_startdtm, stoptime = global_stopdtm, type = "Index",
                    data = rsdata) {
  tmp <- data %>%
    filter(ttype %in% type &
      indexdtm >= startime &
      indexdtm <= stoptime &
      !is.na(!!sym(var))) %>%
    mutate(region = fct_rev(region))

  # per region
  unitdata <- tmp %>%
    filter(region != "") %>%
    group_by(region) %>%
    count(!!sym(var), .drop = FALSE) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100
    ) %>%
    ungroup() %>%
    filter(tot >= 10) %>%
    group_by(!!sym(var)) %>%
    mutate(regionnum = row_number()) %>%
    ungroup()

  # maxwidth <- max(str_length(unitdata$region)) + 1

  percent_ntot <- unitdata %>%
    group_by(region, regionnum, tot) %>%
    summarize(
      percent = paste0(fn(percent, 0), collapse = "/"),
      ntot = paste0(comma(n), collapse = "/"),
      .groups = "drop"
    ) %>%
    mutate(
      percent = paste0(percent, "%"),
      ntot = paste0(ntot, " of ", comma(tot)),
      # countrypad = paste0(str_pad(region, width = maxwidth, side = "left", use_width = FALSE, pad = " "), "  ", ntot)
      countrypad = paste0(region, "  ", ntot)
    )

  if (nlevels(unitdata %>% pull(!!sym(var))) > 2) {
    legrows <- 2
  } else {
    legrows <- 1
  }

  p <- ggplot(data = unitdata, aes(x = regionnum, y = percent, fill = !!sym(var))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    # annotate("text", x = 1, y = 0, label = "test", size = 8) +
    scale_fill_manual(values = global_cols) +
    theme_classic() +
    theme(
      text = element_text(size = global_figfontsize),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(),
      legend.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(hjust = 1, colour = "black"),
      axis.line.y = element_line(colour = "white")
    ) +
    guides(fill = guide_legend(nrow = legrows, reverse = T, byrow = T)) +
    scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100.01), expand = c(0, 0)) +
    scale_x_continuous(breaks = c(unique(unitdata$regionnum)), labels = percent_ntot$countrypad, expand = c(0, 0), sec.axis = dup_axis(labels = percent_ntot$percent)) +
    labs(y = "Proportion (%)")
  p
}
