# -*- coding: UTF-8 -*-
#' Frekvenstabell med replikasjonsvekter
#'
#' Lager en frekvenstabell for én variabel, eventuelt gruppert etter en annen.
#' Resultatene beregnes med replikasjonsvekter via `Rrepest`, og kan vises som tabell,
#' `gt`-tabell, figur (`ggplot2`) eller rådata.
#'
#' @importFrom Rrepest Rrepest est
#' @importFrom dplyr across arrange mutate rename select
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_col geom_errorbar ggplot labs scale_x_discrete theme_minimal vars
#' @importFrom gt cols_label gt tab_options
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer pivot_wider separate
#'
#' @param data Datasett på individnivå med replikasjonsvekter.
#' @param svy Navn på survey-designet som skal brukes, f.eks. "TALISEC_STAFF" eller "TALISEC_LEADER".
#' @param variabel Navn på variabelen som det skal lages frekvenstabell for (streng).
#' @param by Valgfri grupperingsvariabel (streng). Lager separate tabeller per gruppe.
#' @param fast Logisk. Bruk raskere metode med færre vekter? Default er `FALSE`.
#' @param as_gt Logisk. Returner resultat som `gt`-tabell? Default er `FALSE`.
#' @param plot Logisk. Returner resultat som `ggplot2`-figur? Default er `FALSE`.
#' @param return_data Hva skal returneres? "none" (default), "tabell" for tabell,
#'   eller "rrepest" for selve Rrepest-resultatet.
#' @param sort Logisk. Hvis `TRUE`, sorteres resultatet etter andel.
#' @param ci Logisk. Hvis `TRUE`, legges det til konfidensintervall (95 %).
#' @param var_label Egendefinert visningsnavn for `variabel` (valgfritt).
#' @param by_label Egendefinert visningsnavn for `by` (valgfritt).
#'
#' @return En `gt`-tabell, `ggplot2`-figur, `tibble` eller `Rrepest`-objekt, avhengig av valgene.
#' @export
#'
#' @examples
#' \dontrun{
#' # Frekvenstabell uten gruppering
#' freq_table(data = data_02_ansatt, svy = "TALISEC_STAFF", variabel = "gender")
#'
#' # Tabell gruppert etter eierskap, med andeler
#' freq_table(data = data_02_ansatt, svy = "TALISEC_STAFF", variabel = "gender", by = "eierform")
#'
#' # Returner som figur
#' freq_table(data = data_02_ansatt, svy = "TALISEC_STAFF", variabel = "gender", plot = TRUE)
#' }

freq_table <- function(data,
                       svy,
                       variabel,
                       by = NULL,
                       fast = FALSE,
                       as_gt = FALSE,
                       plot = FALSE,
                       return_data = c("none", "tabell", "rrepest"),
                       sort = FALSE,
                       ci = FALSE,
                       var_label = NULL,
                       by_label = NULL) {

  if (!requireNamespace("Rrepest", quietly = TRUE)) stop("Du må installere pakken 'Rrepest'.")
  if (as_gt && !requireNamespace("gt", quietly = TRUE)) stop("Du må installere pakken 'gt' for å bruke as_gt = TRUE.")
  if (plot && !requireNamespace("ggplot2", quietly = TRUE)) stop("Du må installere pakken 'ggplot2' for å bruke plot = TRUE.")

  return_data <- match.arg(return_data)

  if (!variabel %in% names(data)) stop(glue::glue("Variabelen '{variabel}' finnes ikke i datasettet."))
  if (!is.null(by) && !by %in% names(data)) stop(glue::glue("Gruppevariabelen '{by}' finnes ikke i datasettet."))
  if (!svy %in% c("TALISEC_STAFF", "TALISEC_LEADER")) stop("Ugyldig survey-type.")

  var_label <- var_label %||% attr(data[[variabel]], "label") %||% variabel
  if (!is.null(by)) {
    by_label <- by_label %||% attr(data[[by]], "label") %||% by
  }

  # Advarsel hvis flere outputtyper er valgt samtidig
  n_outputs <- sum(plot, as_gt, return_data != "none")
  if (n_outputs > 1) {
    warning("Flere output-alternativer er spesifisert (plot, as_gt, return_data). Bare én vil bli brukt, i prioritert rekkefølge: plot > as_gt > return_data.")
  }

  result <- Rrepest::Rrepest(
    data = data,
    svy = svy,
    est = Rrepest::est(c("freq"), variabel),
    by = by,
    fast = fast
  )

  if (nrow(result) == 0) stop("Ingen resultater fra Rrepest. Sjekk at variabelen har gyldige verdier.")

  if (is.null(by)) {
    tabell <- result %>%
      select(starts_with("b."), starts_with("se.")) %>%
      pivot_longer(cols = everything(), names_to = "parameter", values_to = "verdi") %>%
      separate(parameter, into = c("type", "variabel", "verdi_navn"), sep = "\\.", extra = "merge", fill = "right") %>%
      mutate(type = ifelse(type == "b", "Andel", "Standardfeil")) %>%
      pivot_wider(names_from = type, values_from = verdi) %>%
      select(-variabel) %>%
      rename(`Andel (%)` = Andel, `Standardfeil` = Standardfeil) %>%
      rename(!!var_label := verdi_navn) %>%
      mutate(across(c(`Andel (%)`, `Standardfeil`), ~ round(., 1)))

    if (ci) {
      tabell <- tabell %>%
        mutate(
          `Nedre grense (95 % CI)` = `Andel (%)` - 1.96 * `Standardfeil`,
          `Øvre grense (95 % CI)` = `Andel (%)` + 1.96 * `Standardfeil`
        ) %>%
        mutate(across(c(`Nedre grense (95 % CI)`, `Øvre grense (95 % CI)`), ~ round(., 1)))
    }

  } else {
    tabell <- result %>%
      select(all_of(by), starts_with("b."), starts_with("se.")) %>%
      pivot_longer(cols = -all_of(by), names_to = "parameter", values_to = "verdi") %>%
      separate(parameter, into = c("type", "variabel", "verdi_navn"), sep = "\\.", extra = "merge", fill = "right") %>%
      mutate(type = ifelse(type == "b", "Andel", "Standardfeil")) %>%
      pivot_wider(names_from = type, values_from = verdi) %>%
      select(-variabel) %>%
      rename(`Andel (%)` = Andel, `Standardfeil` = Standardfeil) %>%
      rename(!!var_label := verdi_navn) %>%
      rename(!!by_label := !!sym(by)) %>%
      mutate(across(c(`Andel (%)`, `Standardfeil`), ~ round(., 1)))

    if (ci) {
      tabell <- tabell %>%
        mutate(
          `Nedre grense (95 % CI)` = `Andel (%)` - 1.96 * `Standardfeil`,
          `Øvre grense (95 % CI)` = `Andel (%)` + 1.96 * `Standardfeil`
        ) %>%
        mutate(across(c(`Nedre grense (95 % CI)`, `Øvre grense (95 % CI)`), ~ round(., 1)))
    }
  }

  if (!is.null(by) && sort) tabell <- tabell %>% arrange(!!sym(by_label), desc(`Andel (%)`))
  if (is.null(by) && sort) tabell <- tabell %>% arrange(desc(`Andel (%)`))

  if (plot) {
    x_col <- var_label
    group_col <- by_label
    plot_data <- tabell
    ant_kat_x <- length(unique(plot_data[[x_col]]))
    ant_kat_by <- if (!is.null(by)) length(unique(plot_data[[group_col]])) else 0
    skal_wrappe <- is.null(by) || (ant_kat_x < 4 && ant_kat_by <= 2)

    p <- ggplot(plot_data, aes(x = !!sym(x_col), y = `Andel (%)`, fill = !!sym(x_col))) +
      geom_col(show.legend = FALSE) +
      labs(x = x_col, y = "Andel (%)") +
      theme_minimal() + {
        if (skal_wrappe) scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
        else scale_x_discrete()
      }

    if (ci) {
      p <- p + geom_errorbar(
        aes(ymin = `Nedre grense (95 % CI)`, ymax = `Øvre grense (95 % CI)`),
        width = 0.2, linewidth = 0.6, color = "black")
    }

    if (!is.null(by)) {
      if (ant_kat_x >= 4 || ant_kat_by > 2) {
        p <- p + coord_flip() + facet_wrap(vars(!!sym(group_col)), ncol = 1)
      } else {
        p <- p + facet_wrap(vars(!!sym(group_col)))
      }
    } else {
      if (ant_kat_x > 5) p <- p + coord_flip()
    }

    return(p)
  }

  if (as_gt) {
    if (!is.null(by)) {
      return(gt::gt(tabell, groupname_col = by_label) %>%
               gt::tab_options(row_group.as_column = TRUE))
    } else {
      return(gt::gt(tabell))
    }
  }

  if (return_data == "tabell") return(tabell)
  if (return_data == "rrepest") return(result)

  print(tabell)
  invisible(tabell)
}
