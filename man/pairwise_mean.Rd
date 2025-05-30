% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_mean.R
\name{pairwise_mean}
\alias{pairwise_mean}
\title{Parvise sammenlikninger av gjennomsnitt}
\usage{
pairwise_mean(data, svy, outcome, group, p_adjust = "holm", as_gt = FALSE)
}
\arguments{
\item{data}{Datasett på individnivå med replikasjonsvekter.}

\item{svy}{Navn på survey-designet som skal brukes, f.eks. "TALISEC_STAFF" eller "TALISEC_LEADER".}

\item{outcome}{Kontinuerlig variabel det skal beregnes gjennomsnitt av (streng).}

\item{group}{Kategorisk variabel med gruppene som skal sammenliknes (streng).}

\item{p_adjust}{Metode for p-justering. Default er "holm". Se ?p.adjust.methods for alternativer.}

\item{as_gt}{Logisk. Returner som \code{gt}-tabell? Default er \code{FALSE}.}
}
\value{
Tabell med estimert forskjell mellom grupper, med standardfeil, konfidensintervall og p-verdi.
}
\description{
Sammenlikner gjennomsnitt for en kontinuerlig variabel mellom alle par av grupper,
med støtte for p-justering og output som \code{gt}-tabell.
}
