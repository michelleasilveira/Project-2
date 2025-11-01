# Utility helpers used across the app


# safe_n: safe count for factors/characters
safe_n <- function(x) {
  sum(!is.na(x))
}


# two_way_table: pretty two-way contingency table using janitor if available
.two_way_table <- function(df, var1, var2) {
  if (requireNamespace("janitor", quietly = TRUE)) {
    df |>
      janitor::tabyl({{ var1 }}, {{ var2 }}) |>
      janitor::adorn_totals(where = c("row","col"))
  } else {
    as.data.frame.matrix(table(dplyr::pull(df, {{ var1 }}), dplyr::pull(df, {{ var2 }}), useNA = "no"))
  }
}