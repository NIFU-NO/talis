# data_prep/dataprep_script.R

# Pakker
library(haven)
library(tidyverse)

# ---- 1. Angi sti til data ----

data_path <- "C:/Users/py156/NIFU/21268 TALIS 2024 - General/Delt UiS og NIFU/Data/1_Data"


# ---- 2. Lese inn data ----

# Hjelpefunksjon for standard import
read_and_clean_ansatt <- function(file_path) {
  read_sav(file_path) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.labelled), labelled::unlabelled)) %>%
    as_tibble()
}

read_and_clean_leder <- function(file_path) {
  read_sav(file_path) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.labelled), labelled::unlabelled)) %>%
    rename(cntrwgt = leaderwgt) %>%
    rename_with(~ str_replace(., "lrwgt", "crwgt"), matches("lrwgt")) %>%
    as_tibble()
}



data_u3_ansatt <- read_and_clean_ansatt(file.path(data_path, "asgNORS2.sav"))
data_u3_leder  <- read_and_clean_leder(file.path(data_path, "algNORS2.sav"))
data_02_ansatt <- read_and_clean_ansatt(file.path(data_path, "bsgNORS2.sav"))
data_02_leder  <- read_and_clean_leder(file.path(data_path, "blgNORS2.sav"))

# ---- 3. Bakgrunnsvariabler  ----



# ---- 4. Skriv ut melding ----
message("Alle datasett er importert og klar til bruk.")
