#' Returnerer survey design for TALIS Starting Strong
#'
#' Lager et survey design-objekt med replikasjonsvekter for bruk med survey-pakken.
#'
#' @param data Datasett som inneholder riktige vektvariabler og replikasjonsvekter
#' @param svy Survey-type: "TALISEC_STAFF" eller "TALISEC_LEADER"
#'
#' @return Et objekt av klassen svyrep.design
#' @export
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
