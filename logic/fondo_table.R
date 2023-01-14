
Sys.setlocale("LC_TIME", "Spanish")

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(reactable)
library(purrr)


# Preparing data ----------------------------------------------------------

aportes <- read_excel("./data/aportes.xlsx", sheet = "Ingreso") |>
  janitor::clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    label_fecha = factor(format(fecha, "%d %b %Y", locale = "spanish")),
    label_fecha = forcats::fct_reorder(label_fecha, fecha),
    month = factor(format(fecha, "%b %Y", locale = "spanish")),
    month = forcats::fct_reorder(label_fecha, fecha)
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

aportes_table <- aportes |>
  group_by(miembro, label_fecha) |>
  summarise(aporte = sum(aporte)) |> 
  pivot_wider(names_from = label_fecha, values_from = aporte, values_fill = 0) |>
  rowwise() |>
  mutate(total = sum(c_across(where(is.numeric)))) |>
  select(all_of(dates_order))


aportes_table_2 <- aportes %>%
  group_by(miembro) %>%
  nest() %>%
  mutate(
    cantidad_aportes = map_dbl(data, nrow),
    total = map_dbl(data, ~sum(.x[["aporte"]], na.rm = TRUE)),
    data = map(data, ~.x[,c("fecha", "aporte")] %>% arrange(desc(fecha)))
    ) %>%
  select(miembro, cantidad_aportes, total, data) %>% 
  arrange(desc(total)) 

# html table --------------------------------------------------------------

tabla_aportes <- aportes_table_2 %>%
  select(-data) %>%
  arrange(desc(total)) %>% 
  reactable(
    fullWidth = FALSE,
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
        reactable(aportes_table_2[["data"]][[index]], outlined = TRUE)
      )
    },
    class = "aportes-table"
  )

tabla_gastos <- gastos |>
  arrange(fecha) |> 
  select(fecha, item, monto) |> 
  setNames(c("Fecha", "Concepto", "Monto")) |>
  reactable(
    class = "aportes-table",
    defaultColDef = colDef(headerClass = "header"),
    columns = list(
      Concepto = colDef(minWidth = 150),
      Monto = colDef(format = colFormat(separators = TRUE))
    )
  )

tabla_multas <- multas |>
  reactable(
    class = "aportes-table",
    defaultColDef = colDef(headerClass = "header", width = 90),
    columns = list(Fecha = colDef(width = 100))
  )
