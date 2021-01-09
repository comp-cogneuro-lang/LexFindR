# Testing fwcp

target_freq <- 100
comp_freq <- c(200, 500)
pad <- 1

test_that("get_fwcp is correct", {
  expect_equal(
    get_fwcp(target_freq, comp_freq),
    (log(target_freq) / sum(log(comp_freq)))
  )
  expect_equal(
    get_fwcp(target_freq, comp_freq, pad = pad),
    (log(target_freq + pad) / (sum(log(comp_freq + pad))))
  )
  expect_equal(
    get_fwcp(target_freq, comp_freq, add_target = TRUE),
    (log(target_freq) / (sum(log(comp_freq)) + log(target_freq)))
  )
  expect_equal(get_fwcp(target_freq, NULL), 1.0)
})

test_that("get_fwcp warnings are correct", {
  expect_warning(get_fw(0.1))
  expect_warning(get_fwcp(target_freq, 10))
})

# Testing nohorts -------------------------------

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
nohorts <- c("AA B", "AA B CC", "AA B CC DD", "AA B DD", "AA B CCC")
cohorts_prime <- c("AA B DD A")
neighbors_prime <- c("AA CC", "AA A B CC", "AA AA CC")
embed_in_target_prime <- c("CC")
target_embed_in_prime <- c("DD AA B CC A")

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
    non_matches,
    nohorts,
    cohorts_prime,
    neighbors_prime,
    embed_in_target_prime,
    target_embed_in_prime
  )
)

test_that("nohorts are correct", {
  expect_setequal(get_nohorts(target, lexicon, form = TRUE), nohorts)
  expect_setequal(get_nohorts(target, lexicon, count = TRUE), length(nohorts))
  expect_setequal(lexicon[get_nohorts(target, lexicon)], nohorts)
})

# Testing Cohorts Prime -------------------------------

test_that("cohorts prime are correct", {
  expect_setequal(get_cohortsP(target, lexicon, form = TRUE), cohorts_prime)
  expect_setequal(get_cohortsP(target, lexicon, count = TRUE), length(cohorts_prime))
  expect_setequal(lexicon[get_cohortsP(target, lexicon)], cohorts_prime)
})

# Testing Neighbors Prime -------------------------------

test_that("neighbors prime are correct", {
  expect_setequal(get_neighborsP(target, lexicon, form = TRUE), neighbors_prime)
  expect_setequal(get_neighborsP(target, lexicon, count = TRUE), length(neighbors_prime))
  expect_setequal(lexicon[get_neighborsP(target, lexicon)], neighbors_prime)
})

# Testing Embeds-In-Target Prime -------------------------

test_that("embeds-in-target prime are correct", {
  expect_setequal(get_embeds_in_targetP(target, lexicon, form = TRUE), embed_in_target_prime)
  expect_setequal(get_embeds_in_targetP(target, lexicon, count = TRUE), length(embed_in_target_prime))
  expect_setequal(lexicon[get_embeds_in_targetP(target, lexicon)], embed_in_target_prime)
})

# Testing Target-Embeds-In Prime -------------------------

test_that("target-embeds-in prime are correct", {
  expect_setequal(get_target_embeds_inP(target, lexicon, form = TRUE), target_embed_in_prime)
  expect_setequal(get_target_embeds_inP(target, lexicon, count = TRUE), length(target_embed_in_prime))
  expect_setequal(lexicon[get_target_embeds_inP(target, lexicon)], target_embed_in_prime)
})
