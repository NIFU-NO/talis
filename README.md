
<!-- README.md is generated from README.Rmd. Please edit that file -->

# talis

<!-- badges: start -->
<!-- badges: end -->

Intern R-pakke for analyse av de norske TALIS Starting Strong-dataene.

Pakken inneholder funksjoner for å: - Beregne prosentfordelinger og
gjennomsnitt med replikasjonsvekter (via `Rrepest`) - Lage tabeller med
`gt` og figurer med `ggplot2` - Forenkle arbeid med komplekse
survey-design - Standardisere databehandling på tvers av brukere

## Installation

Dette er et **privat GitHub-repo**. Du må ha tilgang for å installere
det.

``` r
# Installer devtools hvis du ikke har det fra før
install.packages("devtools")

# Installer pakken fra GitHub (hvis du har tilgang)
devtools::install_github("NIFU-NO/talis")
```

## Example

library(talis)

# Eksempel på frekvenstabell

freq_table(data, variabel = “kjonn”, group = “eierform”)
