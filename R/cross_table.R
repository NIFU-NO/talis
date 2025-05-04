#' Krysstabell med replikasjonsvekter
#'
#' Lager en krysstabell (prosent eller antall) med opsjon for test, tabell og plott.
#'
#' @param data Datasettet som skal brukes
#' @param svy Survey-type: "TALISEC_LEADER" eller "TALISEC_STAFF"
#' @param row_var Raden i tabellen (tekst)
#' @param col_var Kolonnen i tabellen (tekst)
#' @param prosent Type prosent: "nei", "rad", "kolonne", "total" (default: "nei")
#' @param test Inkluder Rao-Scott-justert chi²-test? (default: TRUE)
#' @param as_gt Returner som gt-tabell? (default: FALSE)
#' @param plot Returner som ggplot? (default: FALSE)
#' @param return_data Returner data og ev. testresultat? (TRUE eller FALSE, default: FALSE)
#' @param row_label Egendefinert etikett for rader (valgfritt)
#' @param col_label Egendefinert etikett for kolonner (valgfritt)
#'
#' @return Tabell, plot eller tibble
#' @export
cross_table <- function(data,
                        svy,
                        row_var,
                        col_var,
                        prosent = c("nei", "rad", "kolonne", "total"),
                        test = TRUE,
                        as_gt = FALSE,
                        plot = FALSE,
                        return_data = FALSE,
                        row_label = NULL,
                        col_label = NULL) {

  # Pakkesjekker
  if (!requireNamespace("survey", quietly = TRUE)) stop("Du må installere pakken 'survey'.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Du må installere pakken 'dplyr'.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Du må installere pakken 'tibble'.")
  if (!requireNamespace("gt", quietly = TRUE) && as_gt) stop("Du må installere pakken 'gt'.")
  if (!requireNamespace("ggplot2", quietly = TRUE) && plot) stop("Du må installere pakken 'ggplot2'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Du må installere pakken 'stringr'.")

  prosent <- match.arg(prosent)

  if (plot && as_gt) {
    warning("Både plot = TRUE og as_gt = TRUE er spesifisert. Returnerer kun plottet.")
  }

  if (!row_var %in% names(data) || !col_var %in% names(data)) {
    stop("Variablene finnes ikke i datasettet.")
  }

  if (!svy %in% c("TALISEC_STAFF", "TALISEC_LEADER")) {
    stop("Ugyldig survey-type.")
  }

  # Etiketter
  row_label <- row_label %||% attr(data[[row_var]], "label")
  col_label <- col_label %||% attr(data[[col_var]], "label")
  if (is.null(row_label)) {
    message("Merk: row_var mangler 'label'-attributt. Bruker variabelnavnet.")
    row_label <- row_var
  }
  if (is.null(col_label)) {
    message("Merk: col_var mangler 'label'-attributt. Bruker variabelnavnet.")
    col_label <- col_var
  }

  # Vekter og repvekter
  if (svy == "TALISEC_STAFF") {
    rep_col <- grep("^srwgt", names(data), value = TRUE)
    weight_var <- ~staffwgt
  } else {
    rep_col <- grep("^crwgt", names(data), value = TRUE)
    weight_var <- ~cntrwgt
  }

  valid_rep_col <- rep_col[colSums(!is.na(data[rep_col])) > 0]

  data_design <- data %>%
    dplyr::select(all_of(c(row_var, col_var, all.vars(weight_var), valid_rep_col))) %>%
    dplyr::mutate(across(where(is.factor), droplevels))

  design <- survey::svrepdesign(
    weights = weight_var,
    repweights = data_design %>% dplyr::select(all_of(valid_rep_col)),
    type = "Fay",
    rho = 0.5,
    mse = TRUE,
    combined.weights = TRUE,
    data = data_design
  )

  t <- survey::svytable(as.formula(paste("~", row_var, "+", col_var)), design)

  if (prosent != "nei") {
    margin <- switch(prosent,
                     rad = 1,
                     kolonne = 2,
                     total = NULL)
    t <- prop.table(t, margin = margin) * 100
  }

  tabell <- as.data.frame.matrix(t) %>%
    tibble::rownames_to_column(var = "_row")

  test_result <- NULL
  if (test) {
    test_result <- tryCatch({
      survey::svychisq(as.formula(paste("~", row_var, "+", col_var)), design)
    }, error = function(e) {
      message("Chi2-test kunne ikke beregnes: ", e$message)
      NULL
    })
  }

  if (plot) {
    plot_data <- as.data.frame(t) %>%
      dplyr::rename(value = Freq) %>%
      dplyr::mutate(across(c(1, 2), as.factor))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = row_var, y = "value", fill = col_var)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::labs(
        y = ifelse(prosent == "nei", "Antall", "Prosent"),
        x = NULL,
        title = stringr::str_wrap(paste0(row_label, " × ", col_label), width = 60)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, reverse = TRUE)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    if (length(levels(plot_data[[row_var]])) > 5) {
      p <- p + ggplot2::coord_flip()
    }

    if (!is.null(test_result)) {
      message("Rao-Scott p-verdi: ", round(test_result$p.value, 3))
    }

    return(p)
  }

  if (as_gt) {
    gt_tab <- gt::gt(tabell, rowname_col = "_row") %>%
      gt::fmt_number(decimals = 1) %>%
      gt::tab_header(title = paste0("Krysstabell: ", row_label, " × ", col_label))

    if (!is.null(test_result)) {
      test_text <- paste0("Rao-Scott test: F = ", round(test_result$statistic, 2),
                          ", df = ", round(test_result$parameter[1], 2),
                          ", p = ", round(test_result$p.value, 3))
      gt_tab <- gt_tab %>% gt::tab_source_note(test_text)
    }

    return(gt_tab)
  }

  if (return_data) {
    return(list(tabell = tabell, rao_scott_test = test_result))
  }

  print(tabell)
  invisible(tabell)
}
