tgfixfunc <- function(data, tgtype, ncol, totcol, pcol) {
  outdata <- data %>%
    rename(
      n = !!sym(ncol),
      tot = !!sym(totcol),
      p = !!sym(pcol)
    ) %>%
    mutate(
      typetg = tgtype,
      type = case_when(
        X1 == "RIKET" ~ "riket",
        str_detect(X1, "^\\d\\d") ~ "county",
        TRUE ~ "centre"
      ),
      name = if_else(type == "county", str_sub(X1, 4), X1),
      name = case_when(
        name == "RIKET" ~ "Sweden",
        name == "Södermanland" ~ "Sörmland",
        name == "Jämtland" ~ "Jämtland Härjedalen",
        TRUE ~ name
      ),
      tmplan = if_else(type == "county", name, NA_character_),
      tmplan = zoo::na.locf(tmplan, na.rm = F),
      name = if_else(name == "Övriga vårdenheter", paste0("Övriga vårdenheter ", tmplan), name)
    ) %>%
    select(X1, name, type, typetg, n, tot, p)
}
incdata <- tgfixfunc(
  data = inc, tgtype = "Incident",
  ncol = "Antal...2",
  totcol = "Antal...4",
  pcol = "%"
)
prevdata <- tgfixfunc(prev, "Prevalent",
  ncol = "Antal...2",
  totcol = "Antal...4",
  pcol = "%"
)
tg <- bind_rows(incdata, prevdata)

tmp_inc <- inctime %>%
  filter(X1 == "RIKET") %>%
  pivot_longer(cols = 2:ncol(inctime), names_to = "year", values_to = "tg") %>%
  select(-X1) %>%
  mutate(tgtype = "Incident")

tmp_prev <- prevtime %>%
  filter(X1 == "RIKET") %>%
  pivot_longer(cols = 2:ncol(prevtime), names_to = "year", values_to = "tg") %>%
  select(-X1) %>%
  mutate(tgtype = "Prevalent")

tg_overtime <- bind_rows(tmp_inc, tmp_prev)

# fix for 2022
# tg_overtime <- bind_rows(tg_overtime, c(ar = "2022", inc = NA, prev = 35)) %>%
#  mutate(inc = if_else(ar == "2021", as.character(15), inc))
