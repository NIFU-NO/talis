#' Frekvenstabell for spørsmålsbatterier
#'
#' Lager en frekvenstabell for alle variabler som starter med et gitt prefix.
#'
#' @param data Datasettet som skal brukes
#' @param svy Survey-type: "TALISEC_LEADER" eller "TALISEC_STAFF"
#' @param battery_prefix Prefiks for variablene i batteriet (f.eks. "q_29")
#' @param fast Bruk rask metode med færre vekter? (default: FALSE)
#' @param as_gt Returner som gt-tabell? (default: FALSE)
#' @param plot Returner som ggplot? (default: FALSE)
#' @param return_data Hva skal returneres? "none" (default), "tabell" eller "rrepest"
#'
#' @return Tabell, plot, tibble eller liste med resultater per variabel
#' @export
freq_battery <- function(data,
                         svy,
                         battery_prefix,
                         fast = FALSE,
                         as_gt = FALSE,
                         plot = FALSE,
                         return_data = c("none", "tabell", "rrepest")) {

  return_data <- match.arg(return_data)

  if (!svy %in% c("TALISEC_LEADER", "TALISEC_STAFF")) stop("Ugyldig survey-type.")

  battery_vars <- names(data)[startsWith(names(data), battery_prefix)]
  if (length(battery_vars) == 0) stop("Ingen variabler funnet med angitt prefix.")

  full_label <- attributes(data[[battery_vars[1]]])$label %||% battery_prefix
  split_label <- stringr::str_match(full_label, "^(.*?)/")
  battery_label <- if (!is.na(split_label[1, 2])) split_label[1, 2] else full_label

  # Lagre Rrepest-resultater dersom ønsket
  rrepest_list <- list()

  process_column <- function(var) {
    result <- Rrepest::Rrepest(
      data = data,
      svy = svy,
      est = Rrepest::est(c("freq"), var),
      fast = fast
    )

    if (return_data == "rrepest") {
      rrepest_list[[var]] <<- result
    }

    label <- attributes(data[[var]])$label %||% var
    short_label <- stringr::str_remove(label, ".*/\\ ") %||% label
    n <- sum(!is.na(data[[var]]))

    result <- result %>%
      select(starts_with("b.")) %>%
      rename_with(~ str_replace(., paste0("b\\.", var, "\\."), "")) %>%
      pivot_longer(everything(), names_to = "kategori", values_to = "Andel") %>%
      mutate(variabel = short_label, n = n)
  }

  tabell <- purrr::map_dfr(battery_vars, process_column) %>%
    select(variabel, kategori, Andel, n) %>%
    mutate(Andel = round(Andel, 1))

  tabell_wide <- tabell %>%
    select(variabel, kategori, Andel) %>%
    pivot_wider(names_from = kategori, values_from = Andel) %>%
    left_join(tabell %>% select(variabel, n) %>% distinct(), by = "variabel")

  if (as_gt) {
    return(
      gt::gt(tabell_wide, rowname_col = "variabel") %>%
        gt::tab_spanner(
          label = battery_label,
          columns = colnames(tabell_wide)[-1]
        ) %>%
        gt::fmt_number(
          decimals = 1,
          drop_trailing_zeros = TRUE
        ) %>%
        gt::tab_style(
          style = gt::cell_borders(sides = "left", color = "#D3D3D3", weight = gt::px(1)),
          locations = gt::cells_body(columns = "n")
        ) %>%
        gt::cols_label(
          n = gt::html("<i>n</i>")
        )
    )
  }

  if (plot) {
    tabell$variabel <- fct_rev(factor(tabell$variabel, levels = unique(tabell_wide$variabel)))
    p <- ggplot(tabell, aes(x = Andel, y = variabel, fill = kategori)) +
      geom_col(position = "stack") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Prosent", y = NULL, fill = NULL, title = battery_label) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_minimal() +
      theme(legend.position = "bottom")
    return(p)
  }

  if (return_data == "tabell") return(tabell_wide)
  if (return_data == "rrepest") return(rrepest_list)

  print(tabell_wide)
  invisible(tabell_wide)
}
