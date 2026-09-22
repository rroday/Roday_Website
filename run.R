# Metadata ---------------------------------------------------------------------
# Personal Website - Rachel Roday
# Adapted from Emily Markowitz's template

# Knowns -----------------------------------------------------------------------

yourname <- "Rachel E. Roday" 
yournames <- c(yourname, 
               "Roday, R. E.", 
               "Roday, R.") # for paper authorships, as listed in your CV spreadsheet
yourwebsitelink <- "https://rroday.github.io/"

# Fine tuned editing

title_ital <- c( # things that should always be italized in titles. 
  "Cum Laude",
  "Alosa sapidissima")
desc_ital <- c("et al.") # things that should always be italized in descriptions. 
desc_bullet_ital <- c("STUDY SPP")

# Libraries --------------------------------------------------------------------

PKG <- c(
  # page management
  "rmarkdown",
  "pagedown",
  
  # tidyverse
  "dplyr",
  "glue",
  "magrittr",
  "readr",
  "readxl",
  "tidyr",
  "stringr",
  "lubridate",
  "purrr",
  "fs",
  
  # working with URLS
  "xml2",
  
  # mapping
  "leaflet",
  "leafpop",
  "maps", 
  
  # google drive (kept installed in case you ever refresh data locally from
  # Drive again; not used by this script anymore)
  "googledrive",
  "googlesheets4",
  "readtext",
  
  # icons
  "fontawesome"
)

for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)}
}

# Load Data ----------------------------------------------------------------
# NOTE: Data now lives in the repo itself under /data (bio.docx, entries.csv,
# language_skills.csv, text_blocks.csv, contact_info.csv). Google Drive is no
# longer touched by this script, so it can run unattended in CI with no
# credentials. To refresh /data from the Google Doc/Sheet, do that manually
# and commit the updated files.

# Bio document: data/bio.docx is already in the repo, nothing to download.

# CV data: reading from the local /data folder instead of the Google Sheet
# triggers the local-CSV branch inside create_CV_object()/load_data() in
# cv/functions_cv.R (it only hits Google Sheets when data_location contains
# "docs.google.com"). Trailing slash matters - load_data() does
# paste0(data_location, "entries.csv") with no separator.
source("cv/functions_cv.R")
cv_data <- create_CV_object(
  data_location = "data/",
  cache_data = FALSE)
dat0 <- cv_data$entries_data

# Edit Data --------------------------------------------------------------------

dat0 <- dat0 %>%
  dplyr::mutate(public_cv = as.logical(public_cv)) %>%
  dplyr::mutate(website = as.logical(website)) %>%
  dplyr::mutate(custom_cv = as.logical(custom_cv)) %>%
  # Images
  dplyr::mutate(images = ifelse(is.na(img), "",
                              paste0("![*",img_txt,"*](",img,"){width='400px'}"))) %>%
  # URL links
  dplyr::mutate(Links = ifelse(is.na(url), "",
                               paste0("[", url_txt,"](",url,")"))) %>%
  dplyr::mutate(Links = ifelse(is.na(url1), Links,
                               paste0(Links, " \n\n [", url_txt1,"](",url1,")")))

dat0$description_1 <- copyedit(
  format = "bold",
  pattern = c(yournames),
  x = dat0$description_1)

dat0$description_1 <- copyedit(
  format = "italics",
  pattern = desc_ital,
  x = dat0$description_1)

dat0$title <- copyedit(
  format = "italics",
  pattern = title_ital,
  x = dat0$title)

# Clean up entries dataframe to format we need it for printing
dat0 <- dat0 %>%
  tidyr::unite(
    tidyr::starts_with('description'),
    col = "description_bullets",
    sep = "\n- ",
    remove = FALSE,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(description_bullets =
                  ifelse(description_bullets != "",
                         paste0("\n- ", description_bullets),
                         "")) %>%
  dplyr::mutate(timeline =
                  dplyr::case_when(
                    grepl(pattern = "[a-zA-Z]+", x = start) ~ start,
                    is.na(start) ~ "",
                    is.na(end) ~ as.character(start),
                    start == end ~ as.character(start),
                    !is.na(start) & !is.na(end) ~
                      paste0(start, " - ", end)) ) %>%
  dplyr::arrange(desc((end))) %>%
  dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

dat0$description_bullets <- copyedit(
  format = "italics",
  pattern = desc_bullet_ital,
  x = dat0$description_bullets)

dat0 <- dat0 %>%
  dplyr::mutate(Links_inline =
                  ifelse(Links == "",
                         "",
                         paste0('Links: ', gsub(pattern = ' \n\n ',
                                                replacement = ', ', x = Links), ''))) %>%
  dplyr::arrange(desc(start))

cv_data$entries_data <- dat0

# Render CV --------------------------------------------------------------------

source("cv/functions_cv.r")
readr::write_rds(cv_data, 'cv/cached_positions.rds')
cache_data <- TRUE

# Knit the HTML version
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = FALSE,
                                cache_data = cache_data),
                  output_file = "index.html")

# Knit the PDF version to temporary html location
tmp_html_cv_loc <- fs::file_temp(ext = ".html")
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = TRUE, cache_data = cache_data),
                  output_file = tmp_html_cv_loc)

# Convert to PDF using Pagedown
pagedown::chrome_print(input = tmp_html_cv_loc,
                       output = "docs/cv.pdf")

# Render Site ------------------------------------------------------------------

CV <- readr::write_rds(cv_data, 'cv/cached_positions.rds')
cv <-
  CV$entries_data <-
  CV$entries_data %>%
  dplyr::filter(website == TRUE)

sections <- c("about", "index", unique(cv$page))
sections <- sections[sections != "other"]

for (i in 1:length(sections)) {
  rmarkdown::render(paste0("./", sections[i], ".Rmd"),
                    output_dir = "./docs/",
                    output_file = paste0(sections[i], ".html"))
}
