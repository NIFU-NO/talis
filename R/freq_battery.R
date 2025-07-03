# -*- coding: UTF-8 -*-
#' Frekvenstabell for spørsmålsbatterier
#'
#' Lager en samlet frekvenstabell for alle variabler som starter med et gitt prefiks – typisk brukt for batterier av likeformede spørsmål.
#' Tabellene lages ved hjelp av `Rrepest` og kan returneres som `gt`-tabell, `ggplot2`-figur eller bred `tibble`.
#'
#' Prefiks hentes vanligvis fra variabelnavn som `q_29`, og funksjonen forsøker å hente batterinavn fra variablenes `label`.
#'
#' @importFrom Rrepest Rrepest est
#' @importFrom dplyr distinct left_join mutate rename_with select
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 aes geom_col ggplot guides labs scale_fill_brewer theme_minimal
#' @importFrom gt cells_body cols_label fmt_number gt tab_spanner tab_style
#' @importFrom purrr map_dfr
#' @importFrom stringr str_match str_remove
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @param data Datasett med relevante variabler og vektinformasjon.
#' @param svy Survey-designet: `"TALISEC_LEADER"` eller `"TALISEC_STAFF"`.
#' @param battery_prefix Felles prefiks for variablene i spørsmålsbatteriet (f.eks. `"q_29"`).
#' @param battery_label Overskrift for tabellen eller figuren. Hvis `NULL` (default), forsøker funksjonen å hente navn fra første variabels `label`.
#' @param fast Bruk raskere metode med færre vekter? (default: `FALSE`)
#' @param as_gt Returner som `gt`-tabell? (default: `FALSE`)
#' @param plot Returner som `ggplot2`-figur? (default: `FALSE`)
#' @param bar_labels Legg til prosentetiketter i stolpene i figuren? (default: `FALSE`)
#' @param decimals Antall desimaler i figurer og tabeller. Brukes i både `ggplot` og `gt`. Default: `1`.
#' @param local Språklig format for tall. `"no"` (default) gir norsk tegnsetting (komma som desimal og mellomrom som tusenskillere), `"en"` gir engelsk format (punktum som desimal). Brukes i både `gt` og `ggplot`.
#' @param return_data Hva skal returneres? `"none"` (default), `"tabell"` (en bred `tibble`), eller `"rrepest"` (liste med `Rrepest`-resultater per variabel)
#'
#' @return Én av følgende, avhengig av valg:
#' - `gt`-tabell (hvis `as_gt = TRUE`)
#' - `ggplot`-objekt (hvis `plot = TRUE`)
#' - bred `tibble` med andeler (hvis `return_data = "tabell"`)
#' - liste med `Rrepest`-resultater (hvis `return_data = "rrepest"`)
#' - usynlig printet tabell (default)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Enkel frekvenstabell for spørsmål med prefix ss2g15
#' freq_battery(data = data_02_ansatt, svy = "TALISEC_STAFF", battery_prefix = "ss2g15")
#'
#' # Returner som figur
#' freq_battery(data = data_02_ansatt, svy = "TALISEC_STAFF", battery_prefix = "ss2g15", plot = TRUE)
#'
#' # Returner som bred tabell
#' tabell <- freq_battery(data = data_02_ansatt, svy = "TALISEC_STAFF",
#'                        battery_prefix = "ss2g15", return_data = "tabell")
#' }
freq_battery <- function(data,
                         svy,
                         battery_prefix,
                         battery_label = NULL,
                         fast = FALSE,
                         as_gt = FALSE,
                         plot = FALSE,
                         bar_labels = FALSE,
                         decimals = 1,
                         local = "no",
                         return_data = c("none", "tabell", "rrepest")) {

  if (!requireNamespace("Rrepest", quietly = TRUE)) stop("Du må installere pakken 'Rrepest'.")
  if (as_gt && !requireNamespace("gt", quietly = TRUE)) stop("Du må installere pakken 'gt' for å bruke as_gt = TRUE.")
  if (plot && !requireNamespace("ggplot2", quietly = TRUE)) stop("Du må installere pakken 'ggplot2' for å bruke plot = TRUE.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Du må installere pakken 'stringr'.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Du må installere pakken 'purrr'.")
  if (!requireNamespace("forcats", quietly = TRUE)) stop("Du må installere pakken 'forcats'.")

  return_data <- match.arg(return_data)

  if (!svy %in% c("TALISEC_LEADER", "TALISEC_STAFF")) stop("Ugyldig survey-type.")

  battery_vars <- names(data)[startsWith(names(data), battery_prefix)]
  if (length(battery_vars) == 0) stop("Ingen variabler funnet med angitt prefix.")

  if (is.null(battery_label)) {
    battery_label_raw <- attr(data[[battery_vars[1]]], "label")

    if (is.null(battery_label_raw)) {
      message("Merk: Første variabel i batteriet mangler 'label'-attributt. Prefix-navn brukes som batterinavn.")
      battery_label <- battery_prefix
    } else {
      # Split kun på '/ ' – altså en skråstrek etterfulgt av mellomrom
      split_label <- stringr::str_split(battery_label_raw, "/ ", simplify = TRUE)
      n_parts <- length(split_label)
      battery_label <- dplyr::case_when(
        n_parts >= 3 ~ split_label[2],
        n_parts == 2 ~ split_label[1],
        .default = battery_label_raw
      )
    }
  }



  # Advarsel hvis flere outputtyper er valgt samtidig
  n_outputs <- sum(plot, as_gt, return_data != "none")
  if (n_outputs > 1) {
    warning("Flere output-alternativer er spesifisert (plot, as_gt, return_data). Bare én vil bli brukt, i prioritert rekkefølge: plot > as_gt > return_data.")
  }

  rrepest_list <- list()

  process_column <- function(var) {
    if (all(is.na(data[[var]]))) return(NULL)

    result <- Rrepest::Rrepest(
      data = data,
      svy = svy,
      est = Rrepest::est(c("freq"), var),
      fast = fast
    )

    if (return_data == "rrepest") {
      rrepest_list[[var]] <<- result
    }

    label <- attr(data[[var]], "label") %||% var

    split_label <- stringr::str_split(label, "/ ", simplify = TRUE)
    n_parts <- length(split_label)

    short_label <- dplyr::case_when(
      n_parts >= 2 ~ split_label[n_parts],  # Bruk siste del etter siste "/ "
      .default = label
    )
    n <- sum(!is.na(data[[var]]))

    # Hent originale nivåer (faktor-rekkefølge)
    original_levels <- levels(data[[var]])

    # Lag tabellen
    result %>%
      select(starts_with("b.")) %>%
      rename_with(~ stringr::str_replace(., paste0("b\\.", var, "\\."), "")) %>%
      pivot_longer(everything(), names_to = "kategori", values_to = "Andel") %>%
      mutate(
        kategori = factor(kategori, levels = original_levels),
        variabel = short_label,
        n = n
      )
  }

  tabell <- purrr::map_dfr(battery_vars, process_column) %>%
    select(variabel, kategori, Andel, n)

  # Etter pivot_wider()
  kategori_rekkefolge <- levels(data[[battery_vars[1]]])

  tabell_wide <- tabell %>%
    select(variabel, kategori, Andel) %>%
    pivot_wider(names_from = kategori, values_from = Andel, names_sort = FALSE) %>%
    left_join(tabell %>% select(variabel, n) %>% distinct(), by = "variabel") %>%
    select(
      variabel,
      all_of(kategori_rekkefolge[kategori_rekkefolge %in% colnames(.)]),
      n
    )


  if (as_gt) {
    return(
      gt::gt(tabell_wide, rowname_col = "variabel", locale = local) %>%
        gt::tab_spanner(
          label = battery_label,
          columns = colnames(tabell_wide)[-1]
        ) %>%
        gt::fmt_number(
          decimals = decimals,
          drop_trailing_zeros = TRUE
        ) %>%
        gt::tab_style(
          style = gt::cell_borders(sides = "left", color = "#D3D3D3", weight = gt::px(1)),
          locations = gt::cells_body(columns = "n")
        ) %>%
        gt::cols_label(
          n = gt::md("*n*")
        )
    )
  }

  if (plot) {
    tabell$variabel <- forcats::fct_rev(factor(tabell$variabel, levels = unique(tabell_wide$variabel)))

    # Beregn og informer om anbefalt høyde
    n_spm <- length(unique(tabell$variabel))
    anbefalt_hoyde <- beregn_figheight(n_spm)
    message(glue::glue("Tips: Bruk fig.height = {round(anbefalt_hoyde, 1)} i Quarto for bedre lesbarhet"))

    p <- ggplot2::ggplot(tabell, ggplot2::aes(x = Andel, y = variabel, fill = fct_rev(kategori))) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 50)) +
      ggplot2::labs(
        x = ifelse(local == "no", "Prosent", "Percent"),
        y = NULL,
        fill = NULL,
        title = battery_label
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    if (bar_labels) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(Andel >= 2, paste0(
            format(
              round(Andel, decimals),
              nsmall = decimals,
              drop0trailing = T,
              big.mark = ifelse(local == "no", " ", ","),
              decimal.mark = ifelse(local == "no", ",", ".")
            )
          ), "")),
          position = ggplot2::position_stack(vjust = 0.5),
          color = "black",
          size = 3
        )
    }

    return(p)
  }





  if (return_data == "tabell") return(tabell_wide)
  if (return_data == "rrepest") return(rrepest_list)

  print(tabell_wide)
  invisible(tabell_wide)
}
