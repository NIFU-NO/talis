% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_table.R
\name{cross_table}
\alias{cross_table}
\title{Krysstabell med replikasjonsvekter}
\usage{
cross_table(
  data,
  svy,
  row_var,
  col_var,
  prosent = c("nei", "rad", "kolonne", "total"),
  test = TRUE,
  as_gt = FALSE,
  plot = FALSE,
  return_data = FALSE,
  row_label = NULL,
  col_label = NULL
)
}
\arguments{
\item{data}{Datasett med replikasjonsvekter.}

\item{svy}{Navn på survey-designet: "TALISEC_LEADER" eller "TALISEC_STAFF".}

\item{row_var}{Raden i tabellen (streng).}

\item{col_var}{Kolonnen i tabellen (streng).}

\item{prosent}{Type prosent: "nei", "rad", "kolonne" eller "total". Default er "nei".}

\item{test}{Logisk. Inkluder Rao-Scott-justert chi²-test? Default er \code{TRUE}.}

\item{as_gt}{Logisk. Returner som \code{gt}-tabell? Default er \code{FALSE}.}

\item{plot}{Logisk. Returner som \code{ggplot2}-figur? Default er \code{FALSE}.}

\item{return_data}{Logisk. Returner også test og tabell som liste? Default er \code{FALSE}.}

\item{row_label}{Egendefinert visningsnavn for \code{row_var} (valgfritt).}

\item{col_label}{Egendefinert visningsnavn for \code{col_var} (valgfritt).}
}
\value{
En \code{gt}-tabell, \code{ggplot2}-figur, \code{tibble} eller liste med test og tabell.
}
\description{
Lager en krysstabell for to kategoriske variabler, med mulighet for prosentvis fordeling,
statistisk test og ulike outputformater (tabell, \code{gt} eller \code{ggplot2}).
}
\examples{
# Krysstabell med antall
\dontrun{
cross_table(data = data_02_ansatt, svy = "TALISEC_STAFF",
             row_var = "gender", col_var = "eierform")

# Krysstabell med prosentvis fordeling og gt-tabell
cross_table(data = data_02_ansatt, svy = "TALISEC_STAFF",
            row_var = "gender", col_var = "eierform",
            prosent = "rad", as_gt = TRUE)

# Plottet krysstabell
cross_table(data = data_02_ansatt, svy = "TALISEC_STAFF",
            row_var = "gender", col_var = "eierform",
            prosent = "kolonne", plot = TRUE)
}
}
