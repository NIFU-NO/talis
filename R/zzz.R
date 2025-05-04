# -*- coding: UTF-8 -*-
#' @importFrom dplyr %>% desc everything starts_with sym
#' @importFrom data.table :=
#' @importFrom rlang .data
NULL

utils::globalVariables(c(
  # Brukes i flere funksjoner, spesielt etter pivotering
  "Andel", "Standardfeil", "Andel (%)", "Nedre grense (95 % CI)", "Øvre grense (95 % CI)",
  "parameter", "type", "verdi", "verdi_navn", "variabel", "Variabel", "variabelnavn",
  "Gjennomsnitt", "kategori", "n", "Freq", "stat", "se", "p", "p_adj", "ci_low", "ci_high",
  "sammenlikning"
))
