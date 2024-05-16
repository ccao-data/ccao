left <- readr::read_csv(
  file = paste0(
    "https://raw.githubusercontent.com/ccao-data/data-architecture/master/",
    "dbt/seeds/ccao/ccao.adjective.csv"
  )
) %>%
  dplyr::pull(adjective)

# Honorary list of Data Department employees, interns, and fellows
right <- readr::read_csv(
  file = paste0(
    "https://raw.githubusercontent.com/ccao-data/data-architecture/master/",
    "dbt/seeds/ccao/ccao.person.csv"
  )
) %>%
  dplyr::pull(person)

names_gen <- list("left" = left, "right" = right)

usethis::use_data(names_gen, internal = TRUE, overwrite = TRUE)
