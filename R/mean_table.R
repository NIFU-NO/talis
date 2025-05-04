# -*- coding: UTF-8 -*-
#' Gjennomsnittstabell med replikasjonsvekter
#'
#' Beregner gjennomsnitt og usikkerhet for en variabel, med støtte for grupper, plot, og konfidensintervall.
#'
#' @param data Datasettet som skal brukes
#' @param svy Survey-type: "TALISEC_LEADER" eller "TALISEC_STAFF"
#' @param variabel Variabelnavn (tekst, f.eks. "ss2g02")
#' @param by Grupperingsvariabel (valgfritt, tekst)
#' @param fast Bruk rask metode med færre vekter? (default: FALSE)
#' @param as_gt Returner som gt-tabell? (default: FALSE)
#' @param plot Returner som ggplot? (default: FALSE)
#' @param return_data Hva skal returneres? "none" (default), "tabell" eller "rrepest"
#' @param sort Sorter etter gjennomsnitt? (default: FALSE)
#' @param var_label Egendefinert etikett for variabelnavnet
#' @param by_label Egendefinert etikett for grupperingsvariabelen
#' @param ci Inkluder 95 % konfidensintervall? (default: FALSE)
#'
#' @return Tabell, plott, eller tibble
#' @export
mean_table <- function(data,
                       svy,
                       variabel,
                       by = NULL,
                       fast = FALSE,
                       as_gt = FALSE,
                       plot = FALSE,
                       return_data = c("none", "tabell", "rrepest"),
                       sort = FALSE,
                       var_label = NULL,
                       by_label = NULL,
                       ci = FALSE) {

  if (!requireNamespace("Rrepest", quietly = TRUE)) stop("Du må installere pakken 'Rrepest'.")
  if (as_gt && !requireNamespace("gt", quietly = TRUE)) stop("Du må installere pakken 'gt' for å bruke as_gt = TRUE.")
  if (plot && !requireNamespace("ggplot2", quietly = TRUE)) stop("Du må installere pakken 'ggplot2' for å bruke plot = TRUE.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Du må installere pakken 'stringr'.")

  return_data <- match.arg(return_data)

  n_outputs <- sum(plot, as_gt, return_data != "none")
  if (n_outputs > 1) {
    warning("Flere output-alternativer er spesifisert. Bare én vil bli brukt: plot > as_gt > return_data.")
  }

  if (!variabel %in% names(data)) stop("Variabel finnes ikke i datasettet.")
  if (!is.null(by) && !by %in% names(data)) stop("Gruppevariabel finnes ikke i datasettet.")
  if (!svy %in% c("TALISEC_STAFF", "TALISEC_LEADER")) stop("Ugyldig survey-type.")

  if (is.null(var_label)) {
    var_label <- attr(data[[variabel]], "label")
    if (is.null(var_label)) {
      message("Merk: variabelen mangler 'label'-attributt. Bruker variabelnavnet som etikett.")
      var_label <- variabel
    }
  }

  if (!is.null(by) && is.null(by_label)) {
    by_label <- attr(data[[by]], "label")
    if (is.null(by_label)) {
      message("Merk: gruppevariabelen mangler 'label'-attributt. Bruker variabelnavnet som etikett.")
      by_label <- by
    }
  }

  result <- Rrepest::Rrepest(
    data = data,
    svy = svy,
    est = Rrepest::est(c("mean"), variabel),
    by = by,
    fast = fast
  )

  if (nrow(result) == 0) stop("Ingen resultater fra Rrepest. Sjekk at variabelen har gyldige verdier.")

  if (is.null(by)) {
    tabell <- result %>%
      select(starts_with("b."), starts_with("se.")) %>%
      rename(Gjennomsnitt = starts_with("b."),
             Standardfeil = starts_with("se.")) %>%
      mutate(across(everything(), round, 2))

    if (ci) {
      tabell <- tabell %>%
        mutate(
          `Nedre grense (95 % CI)` = pmax(Gjennomsnitt - 1.96 * Standardfeil, 0),
          `Øvre grense (95 % CI)` = Gjennomsnitt + 1.96 * Standardfeil
        ) %>%
        mutate(across(c(`Nedre grense (95 % CI)`, `Øvre grense (95 % CI)`), ~ round(., 2)))
    }

    tabell <- tabell %>%
      mutate(Variabel = var_label) %>%
      relocate(Variabel, .before = everything())

  } else {
    tabell <- result %>%
      select(all_of(by), starts_with("b."), starts_with("se.")) %>%
      pivot_longer(cols = -all_of(by), names_to = "parameter", values_to = "verdi") %>%
      separate(parameter, into = c("type", "stat", "variabelnavn"), sep = "\\.", extra = "merge", fill = "right") %>%
      mutate(type = ifelse(type == "b", "Gjennomsnitt", "Standardfeil")) %>%
      pivot_wider(names_from = type, values_from = verdi) %>%
      select(-stat, -variabelnavn) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename(!!by_label := all_of(by))
  }

  if (ci) {
    tabell <- tabell %>%
      mutate(
        `Nedre grense (95 % CI)` = pmax(Gjennomsnitt - 1.96 * Standardfeil, 0),
        `Øvre grense (95 % CI)` = Gjennomsnitt + 1.96 * Standardfeil
      ) %>%
      mutate(across(c(`Nedre grense (95 % CI)`, `Øvre grense (95 % CI)`), ~ round(., 2)))
  }

  if (!is.null(by) && sort) tabell <- tabell %>% arrange(desc(Gjennomsnitt))

  if (plot) {
    if (is.null(by)) {
      message("Plot ikke laget fordi det ikke er spesifisert noen grupperingsvariabel (by).")
      return(invisible(NULL))
    }

    x_col <- by_label
    plot_data <- tabell %>%
      arrange(Gjennomsnitt) %>%
      mutate(!!x_col := factor(!!sym(x_col), levels = !!sym(x_col)))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!sym(x_col), y = Gjennomsnitt)) +
      ggplot2::geom_col(fill = "steelblue", width = 0.7, show.legend = FALSE) +
      ggplot2::labs(
        x = x_col,
        y = stringr::str_wrap(paste("Gjennomsnitt av", var_label), width = 80)
      ) +
      ggplot2::theme_minimal()

    if (ci && "Nedre grense (95 % CI)" %in% names(plot_data)) {
      p <- p + ggplot2::geom_errorbar(
        aes(ymin = `Nedre grense (95 % CI)`, ymax = `Øvre grense (95 % CI)`),
        width = 0.15, linewidth = 0.6, color = "black")
    }

    p <- p + ggplot2::coord_flip()
    return(p)
  }

  if (as_gt) {
    if (!is.null(by)) {
      return(
        gt::gt(tabell) %>%
          gt::tab_spanner(
            label = var_label,
            columns = c("Gjennomsnitt", "Standardfeil",
                        if (ci) c("Nedre grense (95 % CI)", "Øvre grense (95 % CI)") else NULL)
          )
      )
    } else {
      return(gt::gt(tabell))
    }
  }

  if (return_data == "tabell") return(tabell)
  if (return_data == "rrepest") return(result)

  print(tabell)
  invisible(tabell)
}
