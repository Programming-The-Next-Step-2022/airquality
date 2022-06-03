test_that('check for correct city name', {

 city_from_api <- geocoding("Berlin", "GER")
 city_from_api$name
 expect_match(city_from_api$name, "Berlin")

})

test_that('geocoding throws correct error for typos', {

  expect_warning(geocoding("Ammmmsterdam", "NL"),
                 "There is probably a typo in the city or country entered!")

})
