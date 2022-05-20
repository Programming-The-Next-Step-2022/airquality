test_that('aqi is mentioned', {

  string <- "aqi"
  expect_match(string, "aqi")

})

test_that('output is a data.frame', {

  test_df <- current_aq("Amsterdam", "NL")
  expect_true(is.data.frame(test_df))
})


