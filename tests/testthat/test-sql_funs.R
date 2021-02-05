context("test sql_upload()")

##### TEST sql_upload() #####

# Create a SQLite DB for testing
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Test that upload doesn't fail
test_that("upload throws no errors", {
  expect_silent(
    sql_upload(iris, conn, "test", overwrite = TRUE, append = FALSE)
  )
  expect_silent(
    sql_upload(iris, conn, "test", overwrite = FALSE, append = TRUE)
  )
  expect_message(
    sql_upload(iris, conn, "test", n_chunks = 5),
    regexp = "[0-9]/[0-9]"
  )
  expect_message(
    sql_upload(iris, conn, "test", n_chunks = 1),
    regexp = "[0-9]/[0-9]"
  )
  expect_message(
    sql_upload(
      iris, conn, "test",
      n_chunks = 3,
      overwrite = FALSE, append = TRUE
    ),
    regexp = "[0-9]/[0-9]"
  )
})

# Test that uploaded values are as expected
sql_upload(iris, conn, "test1", overwrite = TRUE, append = FALSE)
sql_upload(iris, conn, "test2", n_chunks = 10, overwrite = TRUE, append = FALSE)
sql_upload(iris, conn, "test3", overwrite = TRUE, append = FALSE)
sql_upload(iris, conn, "test3", n_chunks = 5, overwrite = FALSE, append = TRUE)
iris_no_factor <- iris
iris_no_factor$Species <- as.character(iris_no_factor$Species) # nolint

test_that("uploaded values are correct", {
  expect_equal(
    iris_no_factor, DBI::dbGetQuery(conn, "SELECT * FROM test1")
  )
  expect_equal(
    iris_no_factor, DBI::dbGetQuery(conn, "SELECT * FROM test2")
  )
  expect_equal(
    rbind(iris_no_factor, iris_no_factor),
    DBI::dbGetQuery(conn, "SELECT * FROM test3")
  )
})

# Test that errors are thrown as expect (bad inputs, no data, etc)
test_that("invalid arguments stops process", {
  expect_error(sql_upload(
    iris, conn, "test",
    overwrite = FALSE, append = FALSE
  ))
  expect_error(sql_upload(iris, conn, "test", overwrite = TRUE, append = TRUE))
  expect_error(sql_upload(iris$Sepal.Length, conn, "test"))
  expect_error(sql_upload(iris, "connection", "test"))
  expect_error(sql_upload(iris, conn, overwrite = "yes"))
  expect_error(sql_upload(iris, conn, "test", n_chunks = Inf))
  expect_error(sql_upload(iris, conn, "test", n_chunks = -5))
})

# Release in-memory SQLite DB
DBI::dbDisconnect(conn)
