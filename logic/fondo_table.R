
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(reactable)
library(purrr)

# Preparing data ----------------------------------------------------------

miembros <- read_excel("./data/miembros.xlsx", sheet = "miembros") |>
  mutate(fecha_inicio = as.Date(fecha_inicio))

aportes <- read_excel("./data/aportes.xlsx", sheet = "Ingreso") |>
  janitor::clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    label_fecha = factor(format(fecha, "%d %b %Y", locale = "spanish")),
    label_fecha = forcats::fct_reorder(label_fecha, fecha),
    month = factor(format(fecha, "%b %Y", locale = "spanish")),
    month = forcats::fct_reorder(label_fecha, fecha),
    miembro = str_to_title(miembro)
    )

gastos <- read_excel("./data/aportes.xlsx", sheet = "Gastos") |>
  janitor::clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    label_fecha = factor(format(fecha, "%b %Y", locale = "spanish")),
    label_fecha = forcats::fct_reorder(label_fecha, fecha)
  )

multas <- read_excel("./data/aportes.xlsx", sheet = "Cuentas por cobrar") |>
  mutate(Fecha = as.Date(Fecha)) |>
  select(Fecha, Deudor, Concepto, Monto)

dates_order <- aportes |>
  distinct(label_fecha) |>
  pull(label_fecha) %>%
  as.character() %>%
  c("miembro", "total", .)

# Summary tables ----------------------------------------------------------

aportes_table <- aportes %>%
  group_by(miembro) %>%
  nest() %>%
  mutate(
    cantidad_aportes = map_dbl(data, nrow),
    total = map_dbl(data, ~sum(.x[["aporte"]], na.rm = TRUE)),
    data = map(data, ~.x[,c("fecha", "aporte", "concepto")] %>% arrange(desc(fecha)))
    ) %>%
  select(miembro, cantidad_aportes, total, data) %>% 
  arrange(desc(total))

current_month <- aportes |>
  filter(fecha >= lubridate::floor_date(max(fecha), "month")) |>
  group_by(miembro) |>
  nest() |>
  mutate(
    cantidad_aportes = map_dbl(data, nrow),
    total = map_dbl(data, ~sum(.x[["aporte"]], na.rm = TRUE)),
    data = map(data, ~.x[,c("fecha", "aporte", "concepto")] %>% arrange(desc(fecha)))
  ) %>%
  select(miembro, cantidad_aportes, total, data) %>% 
  arrange(desc(total))

last_month <- aportes |>
  filter(
    fecha >= lubridate::floor_date(max(fecha), "month") - months(1),
    fecha < lubridate::floor_date(max(fecha), "month")
    ) |>
  group_by(miembro) |>
  nest() |>
  mutate(
    cantidad_aportes = map_dbl(data, nrow),
    total = map_dbl(data, ~sum(.x[["aporte"]], na.rm = TRUE)),
    data = map(data, ~.x[,c("fecha", "aporte")] %>% arrange(desc(fecha)))
  ) %>%
  select(miembro, cantidad_aportes, total, data) %>% 
  arrange(desc(total))

current_year <- lubridate::year(lubridate::today())

# balance_2023
this_year <- 
  aportes |>
  filter(lubridate::year(fecha) == current_year) |>
  mutate(nmonth = max(lubridate::month(fecha))) |>
  group_by(miembro, concepto) |>
  summarise(
    aporte = sum(aporte),
    nmonth = max(nmonth),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = concepto, values_from = aporte) |>
  left_join(
    x = select(miembros, miembro = nombre_pago, condicion, fecha_inicio),
    y = _,
    by = c("miembro")
  ) |> 
  mutate(
    nmonth = max(nmonth, na.rm = TRUE),
    active_month = nmonth - (lubridate::month(fecha_inicio) - 1),
    Aporte = tidyr::replace_na(Aporte, 0),
    objetivo = case_when(
      is.na(condicion) | condicion == "Inactivo" ~ 0,
      TRUE ~ active_month * 300
    ),
    balance = case_when(
      condicion == "Inactivo" ~ -300,
      TRUE ~ Aporte - objetivo
    )
  ) |> 
  select(-c(condicion, fecha_inicio, nmonth, active_month))

# html table --------------------------------------------------------------

tabla_aportes <- aportes_table %>%
  select(-data) %>%
  arrange(desc(total)) %>% 
  reactable(
    fullWidth = TRUE,
    searchable = TRUE,
    pagination = FALSE,
    defaultColDef = colDef(headerClass = "header"),
    columns = list(
      miembro = colDef(name = "Miembro"),
      cantidad_aportes = colDef(name = "Aportes"),
      total = colDef("Total", format = colFormat(separators = TRUE))
    ),
    details = function(index) {
      htmltools::div(
        style = "padding: 1rem",
        reactable(aportes_table[["data"]][[index]], outlined = TRUE)
      )
    },
    class = "aportes-table"
  )

tabla_current_month <- current_month %>%
  select(-data) %>%
  arrange(desc(total)) %>% 
  reactable(
    fullWidth = TRUE,
    searchable = TRUE,
    pagination = FALSE,
    defaultColDef = colDef(headerClass = "header"),
    columns = list(
      miembro = colDef(name = "Miembro"),
      cantidad_aportes = colDef(name = "Aportes"),
      total = colDef("Total", format = colFormat(separators = TRUE))
    ),
    details = function(index) {
      htmltools::div(
        style = "padding: 1rem",
        reactable(current_month[["data"]][[index]], outlined = TRUE)
      )
    },
    class = "aportes-table"
  )
  
tabla_last_month <- last_month %>%
  select(-data) %>%
  arrange(desc(total)) %>% 
  reactable(
    fullWidth = TRUE,
    searchable = TRUE,
    pagination = FALSE,
    defaultColDef = colDef(headerClass = "header"),
    columns = list(
      miembro = colDef(name = "Miembro"),
      cantidad_aportes = colDef(name = "Aportes"),
      total = colDef("Total", format = colFormat(separators = TRUE))
    ),
    details = function(index) {
      htmltools::div(
        style = "padding: 1rem",
        reactable(last_month[["data"]][[index]], outlined = TRUE)
      )
    },
    class = "aportes-table"
  )

tabla_gastos <- gastos |>
  arrange(desc(fecha)) |> 
  select(fecha, item, monto) |> 
  setNames(c("Fecha", "Concepto", "Monto")) |>
  reactable(
    fullWidth = TRUE,
    class = "aportes-table",
    defaultColDef = colDef(headerClass = "header"),
    columns = list(
      Concepto = colDef(minWidth = 150),
      Monto = colDef(format = colFormat(separators = TRUE))
    )
  )

tabla_multas <- multas |>
  reactable(
    fullWidth = TRUE,
    class = "aportes-table",
    defaultColDef = colDef(headerClass = "header", minWidth = 90),
    columns = list(Fecha = colDef(minWidth = 100))
  )

balances <- this_year |>
  arrange(desc(balance)) |>
  select(miembro, matches("Técnica"), Aporte, objetivo, balance) |>
  pull(balance)

tabla_this_yer <- this_year |>
  arrange(desc(balance)) |>
  select(miembro, matches("Técnica"), Aporte, objetivo, balance) |> 
  reactable(
    class = "aportes-table",
    defaultColDef = colDef(headerClass = "header", minWidth = 70),
    searchable = TRUE,
    pagination = FALSE,
    fullWidth = TRUE,
    columns = list(
      miembro = colDef(name = "Miembro", minWidth = 80),
      aporte = colDef(name = "Aporte"),
      objetivo = colDef(name = "Objetivo"),
      balance = colDef(name = "Balance")
    ),
    rowStyle = function(index) {
      if (balances[index] < -900) {
        list(backgroundColor = "#ff000080")
      } else if (balances[index] < -300) {
        list(backgroundColor = "#eff76080")
      }
    }
  )
