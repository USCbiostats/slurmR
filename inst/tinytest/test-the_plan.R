
expect_error(the_plan(c(1,1)), "character of length")
expect_error(the_plan(1), "character of length")

expect_silent(the_plan("collect"))
