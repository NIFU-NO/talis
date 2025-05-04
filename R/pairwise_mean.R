#' Parvise sammenlikninger av gjennomsnitt (survey design)
#'
#' Estimerer gjennomsnitt for en kontinuerlig variabel innenfor grupper, og tester parvise forskjeller
#'
#' @param data Datasettet som skal brukes
#' @param svy Survey-type: "TALISEC_LEADER" eller "TALISEC_STAFF"
#' @param outcome Den kontinuerlige variabelen vi sammenlikner (tekst)
#' @param group Gruppene vi sammenlikner på (faktorvariabel, tekst)
#' @param p_adjust Metode for p-justering (default: "holm")
#' @param as_gt Returner som gt-tabell? (default: FALSE)
#'
#' Gyldige p_adjust-metoder for funksjoner som sammenlikner gjennomsnitt:
#' \itemize{
#'   \item "bonferroni" (strengest)
#'   \item "holm" (anbefalt for parvise tester)
#'   \item "BH" (Benjamini-Hochberg, FDR)
#'   \item "fdr" (synonym for "BH")
#'   \item "none" (ingen justering)
#' }
#'
#' @return Tabell med estimerte forskjeller
#' @export
pairwise_mean <- function(data, svy, outcome, group, p_adjust = "holm", as_gt = FALSE) {

  # Pakkesjekker
  if (!requireNamespace("survey", quietly = TRUE)) stop("Du må installere pakken 'survey'.")
  if (as_gt && !requireNamespace("gt", quietly = TRUE)) stop("Du må installere pakken 'gt' for å bruke as_gt = TRUE.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Du må installere pakken 'dplyr'.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Du må installere pakken 'tibble'.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Du må installere pakken 'purrr'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Du må installere pakken 'stringr'.")

  if (!group %in% names(data) || !outcome %in% names(data)) {
    stop("Variablene finnes ikke i datasettet.")
  }

  if (!svy %in% c("TALISEC_STAFF", "TALISEC_LEADER")) {
    stop("Ugyldig survey-type.")
  }

  # Design
  rep_col <- if (svy == "TALISEC_STAFF") grep("^srwgt", names(data), value = TRUE) else grep("^crwgt", names(data), value = TRUE)
  weight_var <- if (svy == "TALISEC_STAFF") ~staffwgt else ~cntrwgt
  valid_rep_col <- rep_col[colSums(!is.na(data[rep_col])) > 0]

  data_design <- data %>%
    dplyr::select(all_of(c(outcome, group, all.vars(weight_var), valid_rep_col))) %>%
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

  # Modell
  formula <- as.formula(paste(outcome, "~", group))
  mod <- survey::svyglm(formula, design = design)

  coefs <- coef(mod)
  coef_names <- names(coefs)
  mf <- model.frame(mod)
  factor_levels <- levels(mf[[group]])

  if (length(factor_levels) < 2) stop("Variabelen group må ha minst to nivåer.")

  ref_level <- factor_levels[1]
  combs <- combn(factor_levels, 2, simplify = FALSE)

  make_contrast <- function(lv1, lv2) {
    vec <- setNames(rep(0, length(coefs)), coef_names)
    if (lv1 != ref_level) vec[paste0(group, lv1)] <- 1
    if (lv2 != ref_level) vec[paste0(group, lv2)] <- -1
    return(vec)
  }

  results <- purrr::map_dfr(combs, function(pair) {
    lv1 <- pair[1]
    lv2 <- pair[2]
    c_vec <- make_contrast(lv1, lv2)
    res <- survey::svycontrast(mod, c_vec)
    est <- as.numeric(res)
    se <- sqrt(as.numeric(attr(res, "var")))
    df <- df.residual(mod)
    t_val <- est / se
    ci <- est + qt(c(0.025, 0.975), df = df) * se
    tibble::tibble(
      sammenlikning = paste(lv1, "vs", lv2),
      est = est,
      se = se,
      t = t_val,
      p = 2 * pt(-abs(t_val), df = df),
      ci_low = ci[1],
      ci_high = ci[2]
    )
  })

  if (p_adjust != "none") {
    if (!p_adjust %in% p.adjust.methods) stop("Ugyldig metode for p-justering.")
    results <- results %>% dplyr::mutate(p_adj = p.adjust(p, method = p_adjust))
  }

  if (as_gt) {
    y_label <- attr(data[[outcome]], "label")
    if (is.null(y_label)) {
      message("Merk: outcome-variabelen mangler 'label'-attributt. Bruker variabelnavnet som etikett.")
      y_label <- outcome
    }

    results <- results %>% dplyr::select(-se, -t)  # Fjern SE og t-verdi fra visning
    gt_tab <- gt::gt(results) %>%
      gt::tab_spanner(label = paste0("Gjennomsnitt av ", y_label), columns = c(est, ci_low, ci_high, p, p_adj)) %>%
      gt::fmt_number(columns = c(est, ci_low, ci_high), decimals = 2) %>%
      gt::fmt(columns = c(p, p_adj), fns = function(x) {
        ifelse(x < 0.001, "< .001", formatC(x, digits = 3, format = "f"))
      }) %>%
      gt::cols_label(
        sammenlikning = "Sammenlikning",
        est = "Estimat",
        p = "p-verdi",
        ci_low = "Nedre CI",
        ci_high = "Øvre CI",
        p_adj = "Justert p"
      ) %>%
      gt::cols_align(align = "left", columns = sammenlikning) %>%
      gt::cols_align(align = "right", columns = where(is.numeric))

    if (p_adjust != "none") {
      gt_tab <- gt_tab %>% gt::tab_source_note(gt::md(glue::glue("*P-verdier justert med {p_adjust} metode*")))
    }
    return(gt_tab)
  }

  return(results)
}
