library(dplyr)
library(readr)

pkgs <- read_csv('pkgs.csv') |> 
  mutate(
    logo = paste0('hexes/hex_', name, '.png'),
    .after = name
  )

pkgs |> 
  group_by(status) |> 
  group_split() |> 
  lapply(function(x) {
    list(
      packages = lapply(seq_len(nrow(x)), function(i) {
        x |> 
          select(-status) |> 
          slice(i) |> 
          as.list() |> 
          purrr::discard(is.na)
      })
    )
  }) |> 
  setNames(c('CRAN', 'GitHub')) |> 
  list(category = _) |> 
  yaml::write_yaml('webscripts/pkgs.yml')
