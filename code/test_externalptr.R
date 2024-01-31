
library(furrr)

vect <- c(1,2)

plan(multisession)

test.func <- function(x, y) {
  return(x * y)
}

options(future.globals.onReference = "error")

x <- vect |> furrr::future_map(~test.func(x = 10, y = .x))

