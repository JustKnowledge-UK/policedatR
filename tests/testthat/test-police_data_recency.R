
httptest::with_mock_api({
  test_that("newest_data() returns the correct latest date", {
    result <- newest_data()
    expect_equal(result, "2025-02")
  })
})

httptest::with_mock_api({
  test_that("oldest_data() returns the correct oldest date", {
    result <- oldest_data()
    expect_equal(result, "2022-03")
  })
})

