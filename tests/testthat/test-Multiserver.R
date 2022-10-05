test_that("Multiserver Works", {
  expect_type(Multiserver(Arrivals=c(1,5,10), ServiceTimes=c(1,15,40),
                          NumServers=1), type = "list")
  expect_length(Multiserver(Arrivals=c(1,5,10,15), ServiceTimes=c(1,15,40,60),
                            NumServers=1), n=4)
})

