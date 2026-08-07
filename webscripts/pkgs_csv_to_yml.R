library(tidyverse)

if (FALSE) {
  pkgs <- read_csv('pkgs.csv')

  descs <- lapply(pkgs$name, \(x) desc::desc(package = x))
  descs <- sapply(descs, \(x) x$get_field('Description'))

  pkgs$description <- descs |>
    sapply(
      function(x) {
        x |>
          markdown::markdownToHTML(fragment.only = TRUE) |>
          stringr::str_remove_all(pattern = '<p>|</p>|\n')
      }) |>
    unname() |>
    stringr::str_replace_all(pattern = '‘', replacement = "'") |>
    stringr::str_replace_all(pattern = '’', replacement = "'") |>
    stringr::str_remove_all(' <a[^>]*?doi[^>]*?>.*?</a>') |>
    stringr::str_remove_all(' <a[^>]*?arXiv[^>]*?>.*?</a>')

  pkgs |>
    write_csv('pkgs.csv')
}

descs <- tribble(
  ~topic, ~topic_title,
  'redistricting', 'Packages for redistricting',
  'census', 'Packages for working with Census Bureau data',
  'plotting', 'Packages for plotting data',
  'api', 'Packages interfacing with API services',
  'other', 'Other R packages',
)

pkgs <- read_csv('pkgs.csv') |>
  mutate(
    logo = paste0('hexes/hex_', name, '.png'),
    .after = name
  ) |>
  left_join(descs, by = "topic") |>
  mutate(topic = factor(topic, levels = unique(descs$topic)))

pkgs <- pkgs |>
  mutate(
    topic = as.character(topic),
    section = paste(tolower(status), topic, sep = '-'),
    categories = purrr::map(topic, list)
  ) |>
  relocate(name, section, categories, .before = 1)

pkgs |>
  purrr::transpose() |>
  purrr::map(purrr::discard, is.na) |>
  yaml::write_yaml('webscripts/pkgs.yml')
