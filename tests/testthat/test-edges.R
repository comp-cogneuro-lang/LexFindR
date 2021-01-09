# Testing empty string ---------------------------

target_list <- list("", 5, FALSE, list(1, "A", TRUE), c("A", "B", "C"))
lexicon_list <- list("", 5, FALSE, list(1, "A", TRUE))
sep_list <- list(5, FALSE, list(1, "A", TRUE), c("A", "B", "C"))

for (target in target_list) {
  for (lexicon in lexicon_list) {
    for (sep in sep_list) {
      test_that("empty strings are correct", {
        expect_equal(get_cohorts(target, lexicon, sep), NA)
        expect_equal(get_rhymes(target, lexicon, sep), NA)
        expect_equal(get_neighbors(target, lexicon, sep, neighbor = "d"), NA)
        expect_equal(get_neighbors(target, lexicon, sep, neighbor = "a"), NA)
        expect_equal(get_neighbors(target, lexicon, sep, neighbor = "s"), NA)
        expect_equal(get_neighbors(target, lexicon, sep, neighbor = "das"), NA)
        expect_equal(get_target_embeds_in(target, lexicon, sep), NA)
        expect_equal(get_embeds_in_target(target, lexicon, sep), NA)
        expect_equal(get_homoforms(target, lexicon, sep), NA)
        expect_equal(get_uniqpt(target, lexicon, sep), NA)
      })
    }
  }
}
