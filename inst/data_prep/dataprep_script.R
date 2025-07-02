# dataprep_script.R

# ---- 0. Oppsett ----

# Sikre at vi jobber relativt til scriptets plassering
script_dir <- dirname(normalizePath(sys.frame(1)$ofile %||% rstudioapi::getActiveDocumentContext()$path))
setwd(script_dir)

# ---- 1. Last inn pakker ----

required_packages <- c("haven", "tidyverse", "janitor", "labelled", "stringr", "rstudioapi")
invisible(lapply(required_packages, library, character.only = TRUE))

# ---- 2. Angi sti til data ----

data_path <- file.path(script_dir, "1_Data")
bakgrunn_path <- file.path(script_dir, "8_Bakgrunnsdata")

# ---- 3. Hjelpefunksjoner for import ----

read_and_clean_ansatt <- function(file_path) {
  message("Leser inn og klargjør ansattfil: ", basename(file_path))
  read_sav(file_path) %>%
    clean_names() %>%
    mutate(across(where(is.labelled), unlabelled)) %>%
    as_tibble()
}

read_and_clean_leder <- function(file_path) {
  message("Leser inn og klargjør lederfil: ", basename(file_path))
  read_sav(file_path) %>%
    clean_names() %>%
    mutate(across(where(is.labelled), unlabelled)) %>%
    rename(cntrwgt = leaderwgt) %>%
    rename_with(~ str_replace(., "lrwgt", "crwgt"), matches("lrwgt")) %>%
    as_tibble()
}

# ---- 4. Lese inn surveydata ----

data_u3_ansatt <- read_and_clean_ansatt(file.path(data_path, "asgNORS2.sav"))
data_u3_leder  <- read_and_clean_leder(file.path(data_path, "algNORS2.sav"))
data_02_ansatt <- read_and_clean_ansatt(file.path(data_path, "bsgNORS2.sav"))
data_02_leder  <- read_and_clean_leder(file.path(data_path, "blgNORS2.sav"))

# ---- 5. Lese inn bakgrunnsdata (placeholder) ----

# Eksempel:
# bakgrunn_kommune <- read_csv(file.path(bakgrunn_path, "kommune_data.csv")) %>%
#   clean_names()

# ---- 6. Ferdig ----
message("✅ Alle datasett er importert og klare til bruk.")
