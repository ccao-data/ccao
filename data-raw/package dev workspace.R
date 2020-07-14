library(ccao)
library(devtools)
library(dplyr)
library(tidyr)
library(stringr)

class_dict <- ccao::class_dict
cdu <- ccao::cdu_dict
options(digits = 16)

# class_dict <- load(file = "data/class_dict.rda")
class_dict$duplicate_desc <- class_dict$class_desc
# split class description on comma-space character sequence to separate characteristics into individual columns
test_split <- class_dict %>% separate(duplicate_desc, c("desc", "age", "size"), ", ", extra = "merge", fill = "right")

# residential properties are in the 200-class, so the major class code is 2 for all properties
residential <- test_split

# create range of values based on descriptions of characteristics
# max age is 9999, max size is 9999999999, and max stories is 9 for residential units
residential <- residential %>%
  mutate(
    age = replace_na(age, "0-Inf"),
    age = case_when(
      age == "any age" ~ "0-999",
      age == "over 62 years" ~ "63-999",
      age == "up to 62 years" ~ "0-62",
      TRUE ~ age
    ),
    size = replace_na(size, "0-Inf"),
    size = case_when(
      size == "up to 999 sq. ft" ~ "0-999",
      size == "1,000 to 1,800 sq. ft." ~ "1000-1800",
      size == "1,801 sq. ft. and over" ~ "1801-Inf",
      size == "up to 2,200 sq. ft" ~ "0-2200",
      size == "2,201 to 4,999 sq. ft." ~ "2201-4999",
      size == "up to 2,000 sq. ft." ~ "0-2000",
      size == "3,801 to 4,999 sq. ft." ~ "3801-4999",
      size == "5,000 sq. ft. and over" ~ "5000-Inf",
      size == "up to 20,000 sq. ft." ~ "0-20000",
      size == "2,001 to 3,800 sq. ft." ~ "2001-3800",
      TRUE ~ "0-Inf"
    ),
    desc = case_when(
      desc == "One story residence" ~ "1-1",
      desc == "Two or more story residence" ~ "2-Inf",
      TRUE ~ desc
    )
  )

# separate minimum and maximum of ranges into individual columns and filter out irrelevant class codes
residential <- residential %>%
  separate(age, c("min_age", "max_age"), "-", extra = "merge", fill = "right") %>%
  separate(size, c("min_size", "max_size"), "-", extra = "merge", fill = "right") %>%
  separate(desc, c("min_stories", "max_stories"), "-", extra = "merge", fill = "right") %>%
  mutate_at(
    vars(min_age, max_age, min_size, max_size, min_stories, max_stories),
    list(as.character)
  ) %>%
  mutate_at(
    vars(min_age, max_age, min_size, max_size, min_stories, max_stories, class_code),
    list(as.double)
  ) %>%
  mutate(
    min_stories = replace_na(min_stories, 1),
    max_stories = case_when(
      class_code == 210 ~ 9,
      class_code == 225 ~ 1,
      class_code == 295 ~ 9,
      TRUE ~ max_stories
    )
  )

saveRDS(residential, file = "data/class_dict.rda")

#
data <- loadRDS(file = "data/class_dict.rda")
View(data)
stories <- 7
size <- 1000
age <- 60
data <- residential

expected_class <- function(stories, age, sqft) {
  # filter data to be within range of input values
  data <- data %>%
    mutate_at(
      vars(min_age, max_age, min_size, max_size, min_stories, max_stories),
      list(as.numeric)
    ) %>%
    filter(
      min_stories <= stories,
      max_stories >= stories,
      min_age <= age,
      max_age >= age,
      min_size <= size,
      max_size >= size
    )

  # notify if no match
  if (nrow(data) == 0) {
    out <- "Class Not Found"
  } else {
    out <- c(data$class_code)
  }
  # return the class code of the result
  return(out)
}

sample_Data <- chars_sample_universe %>%
  slice(1:100) %>%
  select(PIN, TAX_YEAR, CLASS, )

expected_class(stories, age, size)
expected + 2

data <- residential
residential %>%
  mutate_at(
    vars(min_age, max_age, min_size, max_size, min_stories, max_stories),
    list(as.numeric)
  ) %>%
  filter(
    min_stories <= stories,
    max_stories >= stories,
    min_age <= age,
    max_age >= age,
    min_size <= size,
    max_size >= size
  )

class_dict <- load(file = "data/class_dict.rda")
