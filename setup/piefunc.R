piefunc <- function(var,
                    type = "Index",
                    startime = global_startdtm,
                    stoptime = global_stopdtm,
                    data = rsdata) {
  tmp <- data %>%
    filter(
      indexdtm >= startime &
        indexdtm <= stoptime &
        ttype %in% type
    ) %>%
    mutate(!!sym(var) := fct_na_value_to_level(!!sym(var), level = "Unknown"))

  levs <- levels(tmp %>% pull(!!sym(var)))
  mycols <- global_cols[1:length(levs)]
  mycols[levs == "Unknown"] <- global_colsgreymiss

  riket <- tmp %>%
    count(!!sym(var)) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100,
      !!sym(var) := paste0(!!sym(var), "\n", fn(percent, 0), "%"),
      varfac = fct_inorder(!!sym(var))
    )

  p <- ggplot(data = riket, aes(x = "", y = percent, fill = varfac)) +
    geom_bar(stat = "identity") +
    theme_void() +
    theme(
      legend.position = "none",
      text = element_text(size = global_figfontsize)
    ) +
    geom_text(aes(x = 1.7, label = !!sym(var)), position = position_stack(vjust = 0.5), size = global_figfontsize / 3) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = mycols)

  p
}
