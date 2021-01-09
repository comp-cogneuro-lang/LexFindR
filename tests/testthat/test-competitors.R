# Testing in general ---------------------------

target <- c("AA B CC")

cohort <- c("AA B", "AA B CC", "AA B CC DD", "AA B DD", "AA B DD A", "AA B CCC")
rhyme <- c("AA B CC", "B B CC", "B CC", "DD AA B CC", "AAA B CC")
d_neighbor <- c("AA B", "AA CC", "B CC")
a_neighbor <- c("DD AA B CC", "AA B CC DD", "AA A B CC")
s_neighbor <- c("AA B CC", "AA AA CC", "AA B DD", "B B CC", "AA B CCC", "AAA B CC")
target_embed_in <- c("AA B CC", "AA B CC DD", "DD AA B CC", "DD AA B CC A")
embed_in_target <- c("AA B CC", "AA B", "B CC", "CC")
homophone <- c("AA B CC")
non_matches <- c("", " ", "BB C DD", "CC B AA", "CC AA", "A B C", "A BB C", "A B", "BB CC", "B C")

lexicon <- Reduce(
  union,
  list(
    cohort,
    rhyme,
    d_neighbor,
    a_neighbor,
    s_neighbor,
    target_embed_in,
    embed_in_target,
    homophone,
    non_matches
  )
)

test_that("cohort and rhyme are correct", {
  expect_setequal(get_cohorts(target, lexicon, form = TRUE), cohort)
  expect_setequal(get_rhymes(target, lexicon, form = TRUE), rhyme)

  expect_setequal(get_cohorts(target, lexicon, count = TRUE), length(cohort))
  expect_setequal(get_rhymes(target, lexicon, count = TRUE), length(rhyme))

  expect_setequal(lexicon[get_cohorts(target, lexicon)], cohort)
  expect_setequal(lexicon[get_rhymes(target, lexicon)], rhyme)
})

test_that("neighbor are correct", {
  expect_setequal(get_neighbors(target, lexicon, neighbor = "a", form = TRUE), a_neighbor)
  expect_setequal(get_neighbors(target, lexicon, neighbor = "d", form = TRUE), d_neighbor)
  expect_setequal(get_neighbors(target, lexicon, neighbor = "s", form = TRUE), s_neighbor)

  expect_setequal(get_neighbors(target, lexicon, neighbor = "a", count = TRUE), length(a_neighbor))
  expect_setequal(get_neighbors(target, lexicon, neighbor = "d", count = TRUE), length(d_neighbor))
  expect_setequal(get_neighbors(target, lexicon, neighbor = "s", count = TRUE), length(s_neighbor))

  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "a")], a_neighbor)
  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "d")], d_neighbor)
  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "s")], s_neighbor)

  expect_setequal(
    get_neighbors(target, lexicon, neighbor = "das", form = TRUE),
    Reduce(union, list(
      d_neighbor,
      a_neighbor,
      s_neighbor
    ), )
  )

  expect_setequal(
    get_neighbors(target, lexicon, neighbor = "das", count = TRUE),
    length(Reduce(union, list(
      d_neighbor,
      a_neighbor,
      s_neighbor
    ), ))
  )

  expect_setequal(
    lexicon[get_neighbors(target, lexicon, neighbor = "das")],
    Reduce(union, list(
      d_neighbor,
      a_neighbor,
      s_neighbor
    ), )
  )
})

test_that("target_embed_in and embed_in_target are correct", {
  expect_setequal(get_target_embeds_in(target, lexicon, form = TRUE), target_embed_in)
  expect_setequal(get_embeds_in_target(target, lexicon, form = TRUE), embed_in_target)

  expect_setequal(get_target_embeds_in(target, lexicon, count = TRUE), length(target_embed_in))
  expect_setequal(get_embeds_in_target(target, lexicon, count = TRUE), length(embed_in_target))

  expect_setequal(lexicon[get_target_embeds_in(target, lexicon)], target_embed_in)
  expect_setequal(lexicon[get_embeds_in_target(target, lexicon)], embed_in_target)
})

test_that("homophone are correct", {
  expect_setequal(get_homoforms(target, lexicon, form = TRUE), homophone)
  expect_setequal(get_homoforms(target, lexicon, count = TRUE), length(homophone))
  expect_setequal(lexicon[get_homoforms(target, lexicon)], homophone)
})


# Testing cohorts ---------------------------

target <- c("AA B")

cohort <- c("AA B", "AA B CC", "AA B CC DD")
non_matches <- c("A", "A B", "B", "AA BB", "AA CC", "CC AA B")

lexicon <- Reduce(
  union,
  list(
    cohort,
    non_matches
  )
)

test_that("two phone cohort are correct", {
  expect_setequal(lexicon[get_cohorts(target, lexicon)], cohort)
})

target <- c("AA")

lexicon <- c("AA", "AA B", "AA B CC", "B", "B AA", "B AA CC", "A")


test_that("one phone cohort are correct", {
  expect_setequal(lexicon[get_cohorts(target, lexicon)], character(0))
})

# Testing rhymes ---------------------------

target <- c("AA B")

rhyme <- c("B", "AA B", "DD B", "D AA B")
non_matches <- c("A", "AA", "BB", "AA BB", "AA CC", "AA B CC")

lexicon <- Reduce(
  union,
  list(
    rhyme,
    non_matches
  )
)

test_that("two phone rhyme is correct", {
  expect_setequal(lexicon[get_rhymes(target, lexicon)], rhyme)
})

target <- c("AA")

rhyme <- c("B AA", "AA")
non_matches <- c("A", "BB", "AA BB")

lexicon <- Reduce(
  union,
  list(
    rhyme,
    non_matches
  )
)

test_that("one phone rhyme is correct", {
  expect_setequal(lexicon[get_rhymes(target, lexicon)], rhyme)
})


# Testing phonological uniqueness point ---------------------------
target <- "A BB"
lexicon <- c("B", "BB", "AA", "AA BB", "B BBB", "C A BB")


test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 1)
})

target <- "A BB"
lexicon <- c("A", "BB", "AA", "AA BB", "A BBB", "C A BB")


test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 2)
})

target <- "A BB"
lexicon <- c("A", "BB", "AA BB", "A BBB", "C A BB")

lexicon <- union(target, lexicon)
test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 2)
})

target <- "A BB"
lexicon <- c("A", "BB", "A BB CC D EE F", "AA BB", "A BBB", "C A BB")


test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 3)
})

target <- "A BB C"
lexicon <- c("A", "BB", "AA", "AA BB", "A BBB", "C A BB", "A BB CC")


test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 3)
})

target <- "A BB C"
lexicon <- c("A", "BB", "AA", "AA BB", "A BBB", "C A BB", "A BB CC")

lexicon <- union(target, lexicon)
test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 3)
})

target <- "A BB C"
lexicon <- c("A", "BB", "AA", "AA BB", "A BBB", "C A BB", "A BB C D EE F")


test_that("phonological uniqueness point is correct", {
  expect_equal(get_uniqpt(target, lexicon), 4)
})
