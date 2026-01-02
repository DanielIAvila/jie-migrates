# Name: OpenAlex metadata download
# Author: Daniel Itzamna Avila-Ortega
# Article: 
# Version: Final
# Description: This code downloads downloads all the works available in
#               OpenAlex pertaining to the Journal of Industrial Ecology. 



#########################################################
####     Section 0. Load libraries and set paths     ####
#########################################################

# Load necessary libraries
library(here)
library(readxl)
library(tidyverse)
library(jsonlite)
library(openalexR)  #Source: https://docs.ropensci.org/openalexR/


# Path were files are (will be) stored (change it accordingly)
base <- here()
data_path <- file.path(base, "Data")


#########################################################
####        Section 1. Download OpenAlex data        ####
#########################################################

# Polite request data
options(openalexR.mailto = "daniel.avila@su.se")

# Set parameters to download data
options(openalexR.max_retries = 5)
options(openalexR.retry_backoff_factor = 0.1)
options(openalexR.retry_http_codes = c[429, 500, 503])
#options(openalexR.n_max_entities = None)

# Raw OpenAlex URL
url <- "https://openalex.org/"

# Retrieve works from Journal of Industrial Ecology
# Publishers: https://api.openalex.org/publishers
# Journals: 
works_jie <- openalexR::oa_fetch(
  primary_location.source.id = "S203731762",   # Journal ID
  verbose = TRUE
  )

# create directory if it does not exist
if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

# Save to RDS
saveRDS(works_jie, file.path(data_path, 'works_jie_raw.rds'))

# Standardize dataframe for analysis
works_jie <- works_jie %>%
  filter(type %in% c("article", "letter", "review")) %>%  # Keep only articles, letters, reviews
  filter(!is.na(author)) %>%                              # Keep only works with authors
  filter(
    !str_starts(title, "Spanish Abstracts"),              # Remove titles starting with
    !str_starts(title, "Chinese Abstracts")               # Remove titles starting with
  )
  

# Save to RDS
saveRDS(works_jie, file.path(data_path, 'works_jie_clean.rds'))


""" End of 01_download_data.R """
