# Testing cohorts with one overlap ---------------------------

target <- c("AA B")

cohort <- c("AA B", "AA C", "AA D CC", "AA EE CC DD")
non_matches <- c("A", "A B", "B", "CC AA B")

lexicon <- Reduce(
  union,
  list(
    cohort,
    non_matches
  )
)

test_that("two phone cohort are correct", {
  expect_setequal(lexicon[get_cohorts(target, lexicon, overlap = 1)], cohort)
})

target <- c("AA")

cohort <- c("AA")
non_matches <- c("AAA B", "AAA B CC", "B", "B AA", "B AA CC", "A")

lexicon <- Reduce(
  union,
  list(
    cohort,
    non_matches
  )
)

test_that("two character and one phone cohort with one overlap are correct", {
  expect_setequal(lexicon[get_cohorts(target, lexicon, overlap = 1)], cohort)
})


target <- c("A")

cohort <- c("A")
non_matches <- c("AA", "AA B", "AA B CC", "B", "B AA", "B AA CC")

lexicon <- Reduce(
  union,
  list(
    cohort,
    non_matches
  )
)

test_that("one character and one phone cohort with one overlap are correct", {
  expect_setequal(lexicon[get_cohorts(target, lexicon, overlap = 1)], cohort)
})

# Testing rhymes third onset ---------------------------

target <- c("AA B C")

rhyme <- c("C", "B C", "AA B C", "AA BB C", "DD B C", "A AA B C", "D AA B C", "E D AA B C")
non_matches <- c("A B", "AA B", "CC", "AA B CC", "AA B C D", "EE AA B C D")

lexicon <- Reduce(
  union,
  list(
    rhyme,
    non_matches
  )
)

test_that("three phone rhyme third onset is correct", {
  expect_setequal(lexicon[get_rhymes(target, lexicon, mismatch = 2)], rhyme)
})

target <- c("AA")

rhyme <- c("C B AA", "B AA", "AA")
non_matches <- c("A", "BB", "AA BB", "C AA B", "B A")

lexicon <- Reduce(
  union,
  list(
    rhyme,
    non_matches
  )
)

test_that("one phone rhyme third onset is correct", {
  expect_setequal(lexicon[get_rhymes(target, lexicon, mismatch = 2)], rhyme)
})


target <- c("AA B")

rhyme <- c("B", "AA B", "DD B", "D AA B", "C D AA B")
non_matches <- c("A", "AA", "BB", "AA BB", "AA CC", "AA B CC", "C D AAA B", "C D AAA BB", "C D AA E")

lexicon <- Reduce(
  union,
  list(
    rhyme,
    non_matches
  )
)

test_that("two phone rhyme third onset is correct", {
  expect_setequal(lexicon[get_rhymes(target, lexicon, mismatch = 2)], rhyme)
})
