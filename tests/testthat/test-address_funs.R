context("get api key")

# Load from env variable that is passed via GitLab or .Renviron file
usps_api_key <- Sys.getenv("USPS_API_KEY")
if (nchar(usps_api_key) == 0) stop("Missing USPS_API_KEY env variable")

context("test .preprocess_address_data()")

##### TEST .preprocess_address_data() #####

# Create input data frame
input <- dplyr::tibble(
  Address = c(
    "  3726 N WILTON ", "7541 W BROWN #J", "13200 BALTIC CR",
    "11008 KEATING AVE  3E", "15940 S 78TH AV ", "GARFIELD"
  ),
  City = c(
    "CHICAGO      ", "FOREST PARK  ", "	LEMONT    ",
    "OAK LAWN     ", "TINLEY PARK  ", "CAT"
  ),
  State = c("IL", "IL", "IL", "IL", "IL", "IL"),
  Zip = c("60613", "60130", "60439", "60453", "60477", "3")
)

# Apply function to input
processed <- .preprocess_address_data(input)

# Create expected output data frame
new_address <- c(
  "3726 N WILTON", "7541 W BROWN  J", "13200 BALTIC CR",
  "11008 KEATING AVE  3E", "15940 S 78TH AV", "GARFIELD"
)
new_city <- c(
  "CHICAGO", "FOREST PARK", "LEMONT",
  "OAK LAWN", "TINLEY PARK", "CAT"
)
new_zip <- c("60613", "60130", "60439", "60453", "60477", "3")
new_state <- c("IL", "IL", "IL", "IL", "IL", "IL")

# Compare results
test_that("output is as expected", {
  expect_equal(processed$Address, new_address)
  expect_equal(processed$City, new_city)
  expect_equal(processed$Zip, new_zip)
  expect_equal(processed$State, new_state)
})

# Create invalid input data frame
invalid_df <- dplyr::tibble(
  addr = c("123 Ave", "456 St"),
  cty = c("Chicago", "Philadelphia"),
  zip = c(12345, 67890)
)

# Test that invalid input throws error
test_that("invalid schemas stop process", {
  expect_condition(.preprocess_address_data(invalid_df))
})

context("test .batch_query_address()")

##### TEST .batch_query_address() #####

# Use function from R cookbook to compare NA values (NA equals NA yields True)
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/ #nolint
compare_na <- function(v1, v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# Add record so that input data frame has 6 records
new_record <- c("4201F N PAULINA ", "  CHICAGO  ", "IL", "60613")
new_input <- rbind(input, new_record)

# Apply preprocessing function to input
new_input_processed <- .preprocess_address_data(new_input)

batch_query_output <- .batch_query_address(
  new_input_processed$Address[1:5],
  new_input_processed$City[1:5],
  new_input_processed$State[1:5],
  new_input_processed$Zip[1:5],
  batch_size = 5,
  api_key = usps_api_key
)

# Testing that a batch smaller than 5 will return fewer than
# 5 records, even when batch_size is set to 5 - specifically
# the same number of records that were in the batch initially
small_batch_output <- .batch_query_address(
  new_input_processed$Address[1:3],
  new_input_processed$City[1:3],
  new_input_processed$State[1:3],
  new_input_processed$Zip[1:3],
  api_key = usps_api_key
)

# Expected output
bq_address <- c(
  "3726 N WILTON AVE", "7541 BROWN AVE UNIT J", NA,
  "11008 S KEATING AVE APT 3E", "15940 78TH AVE"
)
bq_city <- c("CHICAGO", "FOREST PARK", NA, "OAK LAWN", "TINLEY PARK")
bq_state <- c("IL", "IL", NA, "IL", "IL")
bq_zip <- c("60613", "60130", NA, "60453", "60477")

# Expected small output
small_address <- c("3726 N WILTON AVE", "7541 BROWN AVE UNIT J", NA)
small_city <- c("CHICAGO", "FOREST PARK", NA)
small_state <- c("IL", "IL", NA)
small_zip <- c("60613", "60130", NA)

# Compare results of function to expected output
test_that("output is as expected", {
  expect_equal(compare_na(batch_query_output$Address, bq_address),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(compare_na(batch_query_output$City, bq_city),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(compare_na(batch_query_output$State, bq_state),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(compare_na(batch_query_output$Zip, bq_zip),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(compare_na(small_batch_output$Address, small_address),
               c(TRUE, TRUE, TRUE))
  expect_equal(compare_na(small_batch_output$City, small_city),
               c(TRUE, TRUE, TRUE))
  expect_equal(compare_na(small_batch_output$State, small_state),
               c(TRUE, TRUE, TRUE))
  expect_equal(compare_na(small_batch_output$Zip, small_zip),
               c(TRUE, TRUE, TRUE))
})

# Ensure invalid batch sizes stop process
test_that("invalid inputs throw errors", {
  expect_condition(.batch_query_address(
    new_input_processed$Address,
    new_input_processed$City,
    new_input_processed$State,
    new_input_processed$Zip,
    api_key = usps_api_key
  ))
  expect_condition(.batch_query_address(new_input_processed$Address[1:5],
    new_input_processed$City[1:5],
    new_input_processed$State[1:5],
    new_input_processed$Zip[1:5],
    batch_size = 6,
    api_key = usps_api_key
  ))
})

context("test .group_validation()")

##### TEST .group_validation() #####

output_df <- .group_validation(new_input_processed, api_key = usps_api_key)

expected_output <- c(
  "3726 N WILTON AVE, CHICAGO, IL, 60613",
  "7541 BROWN AVE UNIT J, FOREST PARK, IL, 60130",
  NA,
  "11008 S KEATING AVE APT 3E, OAK LAWN, IL, 60453",
  "15940 78TH AVE, TINLEY PARK, IL, 60477",
  NA,
  "4201F N PAULINA ST, CHICAGO, IL, 60613"
)

test_that("output is as expected", {
  expect_equal(
    compare_na(output_df$Address, expected_output),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("invalid input throws error", {
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, City, Zip), api_key = usps_api_key))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, State, Zip), api_key = usps_api_key))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, City, State), api_key = usps_api_key))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(City, State, Zip), api_key = usps_api_key))
})

context("test validate_address()")

##### TEST validate_address() #####

output <- validate_address(
  new_input$Address,
  new_input$City,
  new_input$State,
  new_input$Zip,
  api_key = usps_api_key
)

expected_output <- c(
  "3726 N WILTON AVE, CHICAGO, IL, 60613",
  "7541 BROWN AVE UNIT J, FOREST PARK, IL, 60130",
  NA,
  "11008 S KEATING AVE APT 3E, OAK LAWN, IL, 60453",
  "15940 78TH AVE, TINLEY PARK, IL, 60477",
  NA,
  "4201F N PAULINA ST, CHICAGO, IL, 60613"
)

test_that("output is as expected", {
  expect_equal(
    compare_na(output, expected_output),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("unequal input vector lengths throw error", {
  expect_condition(validate_address(
    new_input$Address[1:5],
    new_input$City,
    new_input$State,
    new_input$Zip,
    api_key = usps_api_key
  ))
  expect_condition(validate_address(
    new_input$Address,
    new_input$City[1:5],
    new_input$State,
    new_input$Zip,
    api_key = usps_api_key
  ))
  expect_condition(validate_address(
    new_input$Address,
    new_input$City,
    new_input$State[1:5],
    new_input$Zip,
    api_key = usps_api_key
  ))
  expect_condition(validate_address(
    new_input$Address,
    new_input$City,
    new_input$State,
    new_input$Zip[1:5],
    api_key = usps_api_key
  ))
})
