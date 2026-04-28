test_that("java_major_version parses modern and legacy version strings", {
  expect_equal(
    opendataloader:::java_major_version('openjdk version "17.0.12" 2024-07-16'),
    17L
  )
  expect_equal(
    opendataloader:::java_major_version('java version "11.0.24" 2024-07-16 LTS'),
    11L
  )
  expect_equal(
    opendataloader:::java_major_version('java version "1.8.0_402"'),
    8L
  )
})

test_that("java_major_version returns NA for unparseable output", {
  expect_true(is.na(opendataloader:::java_major_version("not java")))
  expect_true(is.na(opendataloader:::java_major_version('java version "ea"')))
})

test_that("check_java rejects missing, broken, old, and unparseable Java", {
  local_mocked_bindings(
    find_java = function() c(java = ""),
    java_version = function(java) stop("should not run")
  )
  expect_error(check_java(), "Java was not found")

  local_mocked_bindings(
    find_java = function() c(java = "/usr/bin/java"),
    java_version = function(java) structure("bad", status = 1L)
  )
  expect_error(check_java(), "`java -version` failed", fixed = TRUE)

  local_mocked_bindings(
    find_java = function() c(java = "/usr/bin/java"),
    java_version = function(java) 'java version "1.8.0_402"'
  )
  expect_error(check_java(), "Java 11 or newer is required")

  local_mocked_bindings(
    find_java = function() c(java = "/usr/bin/java"),
    java_version = function(java) "not java"
  )
  expect_error(check_java(), "Could not determine the Java version")
})

test_that("check_java accepts Java 11 or newer", {
  local_mocked_bindings(
    find_java = function() c(java = "/usr/bin/java"),
    java_version = function(java) 'openjdk version "21.0.2" 2024-01-16'
  )
  expect_true(check_java())
})
