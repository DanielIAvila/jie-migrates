# Name: OpenAlex metadata download
# Author: Daniel Itzamna Avila-Ortega
# Article: 
# Version: Final
# Description:
#               It cleans the data, assembles author affiliations with 
#               university press information, and creates figures.



#########################################################
####     Section 0. Load libraries and set paths     ####
#########################################################

# Load necessary libraries
library(here)
library(readxl)
library(tidyverse)
library(stringdist)
library(stringi)
library(rcrossref)

# Path were files are (will be) stored (change it accordingly)
base <- here()
data_path <- file.path(base, "Data")
fig_path <- file.path(base, "Figures")
data_supporting <- file.path(dirname(dirname(data_path)), "Data")


#########################################################
####        Section 1. Load OpenAlex data        ####
#########################################################

# Load RDS file
works_jie <- readRDS(file.path(data_path, 'works_jie_clean.rds'))


# Get Authors affiliations
affiliations <- works_jie %>% 
  select(
    work_id = id,
    publication_year,
    author
  ) %>%
  tidyr::unnest(author) %>%
  transmute(
    work_id,
    publication_year,
    au_display_name,
    au_orcid,
    institution_name = str_squish(institution_display_name)
  )

# Get dominant affiliation per author
author_dominant_affil <- affiliations %>%
  # Filter out authors with no affiliation
  filter(!is.na(institution_name)) %>%       
  # Count affiliations per author
  count(au_display_name, institution_name, name = "n") %>%
  group_by(au_display_name) %>%
  slice_max(n, with_ties = FALSE) %>%
  mutate(
    share = n / sum(n)
  ) %>%
  ungroup()


# Keep stable authors
stable_authors <- author_dominant_affil %>%
  filter(share >= 0.9)

# Impute affiliations 
affiliations_imputed <- affiliations %>%
  left_join(
    stable_authors %>% select(au_display_name, institution_name),
    by = "au_display_name",
    suffix = c("", "_imputed")
  ) %>%
  mutate(
    institution_final = coalesce(institution_name, institution_name_imputed),
    affiliation_source = if_else(
      is.na(institution_name) & !is.na(institution_name_imputed),
      "imputed_stable_author",
      "observed"
    )
  )

# Check authors without affiliation after imputation
affiliations_imputed_na <- affiliations_imputed %>%
  filter(is.na(institution_final))

# Save to CSV for manual check-up: No action taken!
write_csv(affiliations_imputed_na,
          file.path(data_supporting, "affiliations_imputed_na.csv"))


# Get University Press
uni_press <- read_csv(file.path(data_path, "affiliations_press.csv")) %>%
  rename(
    institution_name = name,
    university_press = `university press`
  ) %>%
  mutate(
    institution_name = str_squish(institution_name),
    university_press = university_press == "Yes"
  )


# Match affiliations with university press info
affiliations_final <- affiliations_imputed %>%
  left_join(
    uni_press,
    by = "institution_name"
  ) %>%
  select(work_id, publication_year, au_display_name, au_orcid,
         institution_final, university_press, url) %>%
  rename(institution_name = institution_final,
         id = work_id) %>%
  mutate(university_press = if_else(is.na(university_press), 
                FALSE, university_press))


# Remove dataframes
rm(authors, author_dominant_affil,
    name_pairs, similar_names, 
    affiliations_imputed, affiliations_imputed_na, 
    uni_press, stable_authors)


############ Start figure section ############

# Calculate articles per year 
articles_year <- works_jie %>% 
  group_by(publication_year, is_oa) %>% 
  summarise(n_articles = n(), .groups = "drop") %>%
  mutate(
    is_oa = if_else(is_oa, "Yes", "No")
  )


# Calculate authors per year 
authors_year <- affiliations_final %>%
  group_by(publication_year) %>%
  summarise(
    total_authors = n_distinct(au_display_name),
    authors_unipress = n_distinct(au_display_name[university_press == TRUE]),
    .groups = "drop"
  )

# Reshape authors data to long format
authors_year_long <- authors_year %>%
  select(publication_year, total_authors, authors_unipress) %>%
  pivot_longer(
    cols = c(total_authors, authors_unipress),
    names_to = "author_series",
    values_to = "n_authors"
  ) %>%
  mutate(
    author_series = recode(
      author_series,
      total_authors = "All authors",
      authors_unipress = "Authors with university press"
    )
  )

# Scale factor for dual y-axis
scale_factor <- max(articles_year$n_articles) /
                max(authors_year$total_authors)



# Combined figure: Articles and Authors per year
fig1 <- ggplot() +

  # --- bars: articles ---
  geom_col(
    data = articles_year,
    aes(
      x = publication_year,
      y = n_articles,
      fill = is_oa
    )
  ) +

  # --- lines: authors ---
  geom_line(
    data = authors_year_long,
    aes(
      x = publication_year,
      y = n_authors * scale_factor,
      linetype = author_series
    ),
    linewidth = 1,
    color = "black"
  ) +

  # --- axes ---
  scale_y_continuous(
    name = "Number of articles",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Number of authors"
    )
  ) +

  # --- legends ---
  scale_fill_manual(
    values = c("No" = "#B12A90FF", "Yes" = "#FCA636FF")
  ) +
  scale_linetype_manual(
    values = c(
      "All authors" = "solid",
      "Authors with university press" = "dashed"
    )
  ) +

  labs(
    x = "Publication year",
    fill = "Open access",
    linetype = "Authors"
  ) +

  theme_minimal() +
  theme(
    legend.position = "top"
  )

fig1

# Create directory if it does not exist
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}

# Save figure 1
ggsave(
  filename = file.path(base, "Figures", "fig1_articles_authors_per_year.png"),
  plot = fig1,
  width = 8,
  height = 5,
  dpi = 300
)

