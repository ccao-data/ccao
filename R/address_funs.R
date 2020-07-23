#' Verify and reformat address information with the USPS API
#'
#' @description Bulk verify address information using street address,
#'   state, city, and zip code data and return a single vector of
#'   complete, verified, comma-separated addresses.
#'
#'   Note that you must create an API key with USPS and set it as
#'   environmental variable called "USPS_API_KEY" on your local
#'   machine for this function to work. This can be done by typing
#'   \code{Sys.setenv("USPS_API_KEY" = "XXXXXXXXXX")} in the RStudio
#'   console.
#'
#' @param address Character string or vector of addresses ({number}
#'   {streetname} {unit, if applicable}). Either 1 long or the same
#'   length as \code{city} and \code{zip}.
#' @param city Character string or vector of city and state
#'   associated with an address ({city} {state}). Either 1 long
#'   or the same length as \code{address} and \code{zip}.
#' @param zip Character string or vector of 5-digit zip codes
#'   associated with an address. Either 1 long or the same length
#'   as \code{address} and \code{city}.
#' @param batch_size Default 5. Specifies the number of queries to be
#' sent to API at a time (maximum 5).
#'
#' @return A string or vector of verified addresses formatted the following way:
#'     "{address}, {city}, {state}, {zip}".
#'   Throws error if input data types are incorrect or if length
#'   conditions of input vectors are not met.
#'
#' @examples
#' address <- "3726 N WILTON"
#' city <- "CHICAGO  IL"
#' zip <- "60613"
#'
#' validate_addresses(address, city, zip)
#'
#' @importFrom magrittr %>%
#' @family address_funs
#' @export
validate_addresses <- function(address, city, zip, batch_size = 5) {
  stopifnot({
    length(address) == length(city) & length(address) == length(zip)
    length(address) != 0
    length(city)    != 0
    length(zip)     != 0
  })

  # Create tibble of address information
  address_df <- dplyr::tibble(Address = address, City = city, Zip = zip)

  # Reformat columns
  address_df <- .preprocess_address_data(address_df)

  # Generate tibble of validated addresses
  validated_addresses <- .group_validation(address_df, batch_size = batch_size)

  return(validated_addresses)
}


#' Reformat address data in preparation for API queries
#'
#' @description Preprocess address data before sending to API. This
#'   includes substituting "#" symbols with empty space, trimming all
#'   leading and trailing whitespace, and separating state abbreviations
#'   from the "City" column into their own "State" column.
#'
#' @param df A tibble that includes columns named "Address", "City", and "Zip".
#'
#' @return The pre-processed tibble with address data.
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
.preprocess_address_data <- function(df) {
  stopifnot({
    "Address" %in% names(df)
    "City" %in% names(df)
    "Zip" %in% names(df)
    typeof(df$Address) == "character"
    typeof(df$City) == "character"
    typeof(df$Zip) == "character"
  })

  df <- df %>%
    dplyr::mutate(
      Address = gsub("#", " ", stringr::str_trim(.data$Address)),
      City = stringr::str_trim(.data$City),
      State = stringr::str_sub(.data$City, start = -2),
      City = stringr::str_trim(
        stringr::str_sub(.data$City, 1, -3),
        side = "both"
      )
    )

  return(df)
}


#' Perform a single batched address query to the USPS API
#'
#' @description Generate 1-5 queries from the USPS API
#'   (max transaction size = 5)
#'
#' @param address Character string or vector of addresses ({number}
#'   {streetname} {unit if applicable}). Either 1 long or the same length
#'   as \code{city}, \code{zip}, and \code{state}.
#' @param city Character string or vector of city and state associated with
#'   an address ({city} {state}). Either 1 long or the same length as
#'   \code{address}, \code{zip}, and \code{state}.
#' @param zip Character string or vector of 5-digit zip codes associated with
#'   an address. Either 1 long or the same length as \code{address},
#'   \code{city}, and \code{state}.
#' @param state Character string or vector of 2-character state abbreviations
#'   associated with an address. Either 1 long or the same length as
#'   \code{address}, \code{city}, and \code{zip}.
#' @param batch_size Default 5. Specifies the number of queries to be sent
#'   to API at a time (maximum 5).
#'
#' @return A tibble with verified address information from the API for one batch
#'   of at most 5 addresses. If an input address is found to be nonexistent,
#'   it will not show up in the returned tibble.
#'
#' @importFrom magrittr %>%
#' @importFrom utils URLencode
.batch_query_address <- function(address, city, state, zip, batch_size = 5) {
  stopifnot({
    length(address) <= 5 &
      length(city) <= 5 &
      length(zip) <= 5 &
      length(address) == length(city) &
      length(address) == length(zip) &
      batch_size <= 5
  })
  # GENERATE QUERY #
  # Paste together and encode link with input values to prepare for API query

  # Initalize query tree
  query_so_far <- paste0(
    '<AddressValidateRequest USERID="',
    Sys.getenv("USPS_API_KEY"),
    '">'
  )

  # Prevent duplicate records from inputs smaller than batch_size
  if (length(address) < batch_size) {
    batch_size <- length(address)
  }

  # Format query string by iterating through records in batch
  for (i in 1:batch_size) {
    next_address <- paste0(
      '<Address ID="', i - 1, '"><Address1></Address1><Address2>', address[i],
      "</Address2><City>", city[i], "</City><State>IL</State><Zip5>",
      zip[i], "</Zip5><Zip4></Zip4></Address>"
    )
    query_so_far <- paste0(query_so_far, next_address)
  }

  # After loop completion, close bracket to finalize request
  query_so_far <- paste0(query_so_far, "</AddressValidateRequest>")

  # Concatenate beginning of URL to XML-formatted query tree
  stem <- "http://production.shippingapis.com/ShippingAPI.dll?API=Verify&XML="
  final_query <- paste0(stem, URLencode(query_so_far))

  # END GENERATE QUERY #

  # SCRAPE DATA #
  # Read html result using xml2 package
  html_data <- xml2::read_html(final_query)

  # Grab text of query result for each field using rvest package
  new_address <- html_data %>%
    rvest::html_nodes("address2") %>%
    rvest::html_text()

  new_city <- html_data %>%
    rvest::html_nodes("city") %>%
    rvest::html_text()

  new_state <- html_data %>%
    rvest::html_nodes("state") %>%
    rvest::html_text()

  new_zip <- html_data %>%
    rvest::html_nodes("zip5") %>%
    rvest::html_text()

  # END SCRAPE DATA #

  # Create {batch_size}-row tibble of results (unless 1 or more addresses
  # in batch are invalid)
  append <- dplyr::tibble(new_address, new_city, new_state, new_zip)
  names(append) <- c("Address", "City", "State", "Zip")

  return(append)
}


#' Validate all addresses within a tibble of addresses
#'
#' @description This function takes a tibble that has been pre-processed with
#'   the .preprocess_address_data function. It runs through the address data in
#'   batches to work with the restrictions of the USPS API using
#'   .batch_query_address until all addresses in the tibble are verified.
#'
#' @param df A tibble that must contain the following schema:
#'   Address (character)
#'   City (character)
#'   State (character)
#'   Zip (character)
#' @param batch_size Default 5. Specifies the size of a transaction
#'   (maximum 5 for USPS API)
#'
#' @importFrom rlang .data
#' @return A tibble with the same fields as the input tibble.

.group_validation <- function(df, batch_size = 5) {
  stopifnot({
    "Address" %in% names(df)
    "City" %in% names(df)
    "Zip" %in% names(df)
    "State" %in% names(df)
    typeof(df$Address) == "character"
    typeof(df$City) == "character"
    typeof(df$Zip) == "character"
    typeof(df$State) == "character"
  })

  # Initialize empty tibble
  results <- dplyr::tibble(
    Address = character(),
    City = character(),
    Zip = character(),
    State = character(),
    stringsAsFactors = FALSE
  )

  # Iterate over batches of records from input tibble until all
  # records are complete
  for (row in seq_len(nrow(df))) {
    if (row %% batch_size == 1) {
      # Handling tibbles with size not multiple of batch_size
      if (row <= (nrow(df) - batch_size)) {
        endrow <- row + batch_size - 1
      }
      else {
        endrow <- nrow(df)
      }

      # Define batched data
      address <- df$Address[row:endrow]

      city <- df$City[row:endrow]

      state <- df$State[row:endrow]

      zip <- df$Zip[row:endrow]

      # Call batch_query_address to verify batched data
      validation <- .batch_query_address(address,
        city,
        state,
        zip,
        batch_size = batch_size
      )

      # Append verified addresses to results tibble
      results <- rbind(results, validation)
    }
  }

  # Format results into single column of comma-separated values
  results <- results %>%
    dplyr::mutate(Address = paste0(
      .data$Address, ", ",
      .data$City, ", ",
      .data$State, ", ",
      .data$Zip
    )) %>%
    dplyr::select(.data$Address)

  return(results)
}
