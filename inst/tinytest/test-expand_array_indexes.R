ans1 <- expand_array_indexes(c("123_1", "55_[1-5]", "122_[1, 5-6]", "44_[1-3:2]"))
ans0 <- c("123_1", "55_1", "55_2", "55_3", "55_4", "55_5", "122_1", "122_5", "122_6", "44_1", "44_3")
expect_equal(ans1, ans0)

expect_equal(
  slurmR:::submitted_array_ids(list(array = "1,3-7:2%2"), 10L),
  c(1L, 3L, 5L, 7L)
)
expect_equal(slurmR:::submitted_array_ids(list(a = 2L), 4L), 2L)
expect_equal(
  slurmR:::submitted_array_ids(list(), 4L, list(array = "2-4")),
  2:4
)
expect_equal(slurmR:::submitted_array_ids(list(), 3L), 1:3)
