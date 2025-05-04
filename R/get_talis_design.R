# -*- coding: UTF-8 -*-
#' Returnerer survey design for TALIS Starting Strong
#'
#' Lager et `svrep.design`-objekt med replikasjonsvekter, tilpasset enten leder- eller ansattdata fra TALIS Starting Strong.
#' Brukes internt i andre funksjoner for å lage gyldig surveydesign med riktige vekter.
#'
#' @importFrom dplyr across all_of mutate select where
#' @importFrom survey svrepdesign
#'
#' @param data Datasett som inneholder vektvariablene `staffwgt` eller `cntrwgt` samt replikasjonsvekter (`srwgt*` eller `crwgt*`).
#' @param svy Survey-type: "TALISEC_STAFF" eller "TALISEC_LEADER". Brukes for å velge riktige vektvariabler.
#'
#' @return Et objekt av klassen `svyrep.design` fra `survey`-pakken. Kan brukes direkte i `svyglm()`, `svymean()` osv.
#' @export
#'
#' @examples
#' \dontrun{
#' # Lag survey-design for ansattdata
#' design <- get_talis_design(data = data_02_ansatt, svy = "TALISEC_STAFF")
#'
#' # Deretter kan du bruke survey-funksjoner:
#' # svymean(~ss2g02, design)
#' }
get_talis_design <- function(data, svy) {
  if (!requireNamespace("survey", quietly = TRUE)) stop("Du må installere pakken 'survey'.")

  if (!svy %in% c("TALISEC_STAFF", "TALISEC_LEADER")) {
    stop("Ugyldig survey-type. Må være 'TALISEC_STAFF' eller 'TALISEC_LEADER'.")
  }

  if (svy == "TALISEC_STAFF") {
    rep_col <- grep("^srwgt", names(data), value = TRUE)
    weight_var <- ~staffwgt
  } else {
    rep_col <- grep("^crwgt", names(data), value = TRUE)
    weight_var <- ~cntrwgt
  }

  valid_rep_col <- rep_col[colSums(!is.na(data[rep_col])) > 0]

  if (length(valid_rep_col) == 0) stop("Ingen gyldige replikasjonsvekter funnet i datasettet.")

  survey::svrepdesign(
    weights = weight_var,
    repweights = data[valid_rep_col],
    type = "Fay",
    rho = 0.5,
    mse = TRUE,
    combined.weights = TRUE,
    data = data
  )
}
