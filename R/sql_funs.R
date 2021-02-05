#' Upload tables to SQL in chunks
#'
#' @description Writing large tables to a database often fails on an
#' intermittent connection or when the database has large active queries. This
#' function is a hacky workaround to upload large tables in chunks, rather than
#' all at once. It divides the input data (to be uploaded) into N equal-sized
#' chunks, then uploads them one by one.
#'
#' @param data A data frame to be uploaded.
#' @param conn A DBI connection object, created by \code{DBI::dbConnect()}.
#' @param sql_table_name The name of the table in SQL to write to.
#' @param n_chunks The number of chunks to divide the input data into. If
#'   \code{NULL}, no chunking will be used.
#' @param overwrite Whether or not to overwrite the existing data in SQL. Must
#'   be the opposite of \code{append}.
#' @param append Whether or not to append data to the existing data in SQL. Must
#'   be the opposite of \code{overwrite}.
#'
#' @examples
#' \dontrun{
#' # Create a SQLite database to test upload
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Upload the iris dataset in chunks
#' sql_upload(
#'   data = iris,
#'   conn = conn,
#'   sql_table_name = "test",
#'   n_chunks = 5,
#'   overwrite = TRUE,
#'   append = FALSE
#' )
#' }
#'
#' @family sql_funs
#' @export
sql_upload <- function(data, conn, sql_table_name, n_chunks = NULL,
                       overwrite = TRUE, append = FALSE) {

  # Checks for input arg conflicts
  stopifnot(
    is.logical(overwrite),
    is.logical(append),
    xor(overwrite, append),
    is.numeric(n_chunks) | is.null(n_chunks),
    is.finite(n_chunks),
    n_chunks > 0,
    is.data.frame(data),
    DBI::dbIsValid(conn),
    is.character(sql_table_name),
    length(sql_table_name) == 1
  )

  # If chunks aren't specified, this function is just a wrapper for dbWriteTable
  if (is.null(n_chunks)) {
    DBI::dbWriteTable(
      conn = conn,
      name = sql_table_name,
      value = data,
      overwrite = overwrite,
      append = append
    )
  } else {

    # Split table into N roughly equal chunks
    df_lst <- split(data, (seq(nrow(data)) - 1) %/% (nrow(data) / n_chunks))

    # Append table in chunks, overwriting on the first chunk if necessary
    for (i in seq_along(df_lst)) {
      message("Now uploading chunk: ", i, "/", length(df_lst))
      if (overwrite & i == 1) {
        DBI::dbWriteTable(
          conn = conn,
          name = sql_table_name,
          value = df_lst[[i]],
          overwrite = overwrite
        )
      } else {
        DBI::dbWriteTable(
          conn = conn,
          name = sql_table_name,
          value = df_lst[[i]],
          append = TRUE
        )
      }
    }
  }
}
