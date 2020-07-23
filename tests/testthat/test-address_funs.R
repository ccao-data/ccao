context("test .preprocess_address_data()")

##### TEST .preprocess_address_data() #####

# Create input data frame
input <- dplyr::tibble(
  Address = c(
    "  3726 N WILTON ",
    "7541 W BROWN #J",
    "13200 BALTIC CR",
    "11008 KEATING AVE  3E",
    "15940 S 78TH AV ",
    "GARFIELD"
  ),
  City = c(
    "CHICAGO      IL",
    "FOREST PARK  IL",
    "	LEMONT       IL",
    "OAK LAWN     IL",
    "TINLEY PARK  IL",
    "CAT"
  ),
  Zip = c("60613", "60130", "60439", "60453", "60477", "3")
)

# Apply function to input
processed <- .preprocess_address_data(input)

# Create expected output data frame
new_address <- c(
  "3726 N WILTON",
  "7541 W BROWN  J",
  "13200 BALTIC CR",
  "11008 KEATING AVE  3E",
  "15940 S 78TH AV"
)
new_city <- c("CHICAGO", "FOREST PARK", "LEMONT", "OAK LAWN", "TINLEY PARK")
new_zip <- c("60613", "60130", "60439", "60453", "60477")
new_state <- c("IL", "IL", "IL", "IL", "IL")

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

# Add record so that input data frame has 6 records
new_record <- c("4201F N PAULINA ", "CHICAGO      IL", "60613")
new_input <- rbind(input, new_record)

# Apply preprocessing function to input
new_input_processed <- .preprocess_address_data(new_input)

batch_query_output <- .batch_query_address(new_input_processed$Address[1:5],
  processed$City[1:5],
  processed$State[1:5],
  processed$Zip[1:5],
  batch_size = 5
)
# Expected output
bq_address <- c(
  "3726 N WILTON AVE",
  "7541 BROWN AVE UNIT J",
  "11008 S KEATING AVE APT 3E",
  "15940 78TH AVE"
)
bq_city <- c("CHICAGO", "FOREST PARK", "OAK LAWN", "TINLEY PARK")
bq_state <- c("IL", "IL", "IL", "IL")
bq_zip <- c("60613", "60130", "60453", "60477")

# Compare results of function to expected output
test_that("output is as expected", {
  expect_equal(batch_query_output$Address, bq_address)
  expect_equal(batch_query_output$City, bq_city)
  expect_equal(batch_query_output$State, bq_state)
  expect_equal(batch_query_output$Zip, bq_zip)
})

# Ensure invalid batch sizes stop process
test_that("invalid inputs throw errors", {
  expect_condition(.batch_query_address(
    new_input_processed$Address,
    new_input_processed$City,
    new_input_processed$State,
    new_input_processed$Zip
  ))
  expect_condition(.batch_query_address(new_input_processed$Address[1:5],
    new_input_processed$City[1:5],
    new_input_processed$State[1:5],
    new_input_processed$Zip[1:5],
    batch_size = 6
  ))
})

context("test .group_validation()")

##### TEST .group_validation() #####

output_df <- .group_validation(new_input_processed)

expected_output <- c(
  "3726 N WILTON AVE, CHICAGO, IL, 60613",
  "7541 BROWN AVE UNIT J, FOREST PARK, IL, 60130",
  "11008 S KEATING AVE APT 3E, OAK LAWN, IL, 60453",
  "15940 78TH AVE, TINLEY PARK, IL, 60477",
  "4201F N PAULINA ST, CHICAGO, IL, 60613"
)

test_that("output is as expected", {
  expect_equal(output_df$Address, expected_output)
})

test_that("invalid input throws error", {
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, City, Zip)))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, State, Zip)))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(Address, City, State)))
  expect_condition(.group_validation(new_input_processed %>%
    dplyr::select(City, State, Zip)))
})

context("test validate_addresses()")

##### TEST validate_addresses() #####

output <- validate_addresses(new_input$Address, new_input$City, new_input$Zip)

expected_output <- c(
  "3726 N WILTON AVE, CHICAGO, IL, 60613",
  "7541 BROWN AVE UNIT J, FOREST PARK, IL, 60130",
  "11008 S KEATING AVE APT 3E, OAK LAWN, IL, 60453",
  "15940 78TH AVE, TINLEY PARK, IL, 60477",
  "4201F N PAULINA ST, CHICAGO, IL, 60613"
)

test_that("output is as expected", {
  expect_equal(output$Address, expected_output)
})

test_that("unequal input vector lengths throw error", {
  expect_condition(validate_addresses(
    new_input$Address[1:5],
    new_input$City,
    new_input$Zip
  ))
  expect_condition(validate_addresses(
    new_input$Address,
    new_input$City[1:5],
    new_input$Zip
  ))
  expect_condition(validate_addresses(
    new_input$Address,
    new_input$City,
    new_input$Zip[1:5]
  ))
})
