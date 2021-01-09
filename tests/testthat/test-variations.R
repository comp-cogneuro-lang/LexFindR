# Testing separators ---------------------------

new_sep_list <- c("   ", "#", "###", "\t", "\t\t\t", "^", "^^^", "*+?", "!@#$%^&*()_+")
for (new_sep in new_sep_list) {
  target <- gsub(" ", new_sep, c("AA B CC"))

  cohort <- gsub(" ", new_sep, c("AA B", "AA B CC", "AA B CC DD", "AA B DD", "AA B DD A", "AA B CCC"))
  rhyme <- gsub(" ", new_sep, c("AA B CC", "B B CC", "B CC", "DD AA B CC", "AAA B CC"))
  d_neighbor <- gsub(" ", new_sep, c("AA B", "AA CC", "B CC"))
  a_neighbor <- gsub(" ", new_sep, c("DD AA B CC", "AA B CC DD", "AA A B CC"))
  s_neighbor <- gsub(" ", new_sep, c("AA B CC", "AA AA CC", "AA B DD", "B B CC", "AA B CCC", "AAA B CC"))
  target_embed_in <- gsub(" ", new_sep, c("AA B CC", "AA B CC DD", "DD AA B CC", "DD AA B CC A"))
  embed_in_target <- gsub(" ", new_sep, c("AA B CC", "AA B", "B CC", "CC"))
  homophone <- gsub(" ", new_sep, c("AA B CC"))

  non_matches <- gsub(" ", new_sep, c("", "   ", "\t", "\t\t\t", "BB C DD", "CC B AA", "CC AA", "A B C", "A BB C", "A B", "BB CC", "B C"))

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
    expect_setequal(lexicon[get_cohorts(target, lexicon, sep = new_sep)], cohort)
    expect_setequal(lexicon[get_rhymes(target, lexicon, sep = new_sep)], rhyme)
  })

  test_that("neighbor are correct", {
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "a", sep = new_sep)], a_neighbor)
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "d", sep = new_sep)], d_neighbor)
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "s", sep = new_sep)], s_neighbor)
    expect_setequal(
      lexicon[get_neighbors(target, lexicon, neighbor = "das", sep = new_sep)],
      Reduce(union, list(
        d_neighbor,
        a_neighbor,
        s_neighbor
      ), )
    )
  })

  test_that("target_embed_in and embed_in_target are correct", {
    expect_setequal(lexicon[get_target_embeds_in(target, lexicon, sep = new_sep)], target_embed_in)
    expect_setequal(lexicon[get_embeds_in_target(target, lexicon, sep = new_sep)], embed_in_target)
  })

  test_that("homophone are correct", {
    expect_setequal(lexicon[get_homoforms(target, lexicon, sep = new_sep)], homophone)
  })
}

# Testing empty separator ---------------------------

new_sep <- ""

target <- c("ABC")

cohort <- c("AB", "ABC", "ABCD", "ABD", "ABDA", "ABC")
rhyme <- c("ABC", "BBC", "BC", "DABC", "AABC")
d_neighbor <- c("AB", "AC", "BC")
a_neighbor <- c("DABC", "ABCD", "AABC")
s_neighbor <- c("ABC", "AAC", "ABD", "BBC", "ABC")
target_embed_in <- c("ABC", "ABCD", "DABC", "AABC", "DABCA")
embed_in_target <- c("ABC", "AB", "BC", "C")
homophone <- c("ABC")

non_matches <- c("", "   ", "\t", "\t\t\t", "BCD", "CBA", "CA", "ABC", "ABC", "AB", "BC", "BC")

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
  expect_setequal(lexicon[get_cohorts(target, lexicon, sep = new_sep)], cohort)
  expect_setequal(lexicon[get_rhymes(target, lexicon, sep = new_sep)], rhyme)
})

test_that("neighbor are correct", {
  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "a", sep = new_sep)], a_neighbor)
  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "d", sep = new_sep)], d_neighbor)
  expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "s", sep = new_sep)], s_neighbor)
  expect_setequal(
    lexicon[get_neighbors(target, lexicon, neighbor = "das", sep = new_sep)],
    Reduce(union, list(
      d_neighbor,
      a_neighbor,
      s_neighbor
    ), )
  )
})

test_that("target_embed_in and embed_in_target are correct", {
  expect_setequal(lexicon[get_target_embeds_in(target, lexicon, sep = new_sep)], target_embed_in)
  expect_setequal(lexicon[get_embeds_in_target(target, lexicon, sep = new_sep)], embed_in_target)
})

test_that("homophone are correct", {
  expect_setequal(lexicon[get_homoforms(target, lexicon, sep = new_sep)], homophone)
})

# Testing all characters ---------------------------

new_chr_list <- c(
  "a", "0", "1", "P", "%", "#", "^",
  "aa", "Aa", "A0", "00", "01", "A#", "#%", "#^", "^^",
  "Aaa", "A00", "000", "012", "A#@", "#%&", "#^$", "^^^"
)
for (new_chr in new_chr_list) {
  target <- gsub("AA", new_chr, c("AA B CC"))

  cohort <- gsub("AA", new_chr, c("AA B", "AA B CC", "AA B CC DD", "AA B DD", "AA B DD A", "AA B CCC"))
  rhyme <- gsub("AA", new_chr, c("AA B CC", "B B CC", "B CC", "DD AA B CC", "AAA B CC"))
  d_neighbor <- gsub("AA", new_chr, c("AA B", "AA CC", "B CC"))
  a_neighbor <- gsub("AA", new_chr, c("DD AA B CC", "AA B CC DD", "AA A B CC"))
  s_neighbor <- gsub("AA", new_chr, c("AA B CC", "AA AA CC", "AA B DD", "B B CC", "AA B CCC", "AAA B CC"))
  target_embed_in <- gsub("AA", new_chr, c("AA B CC", "AA B CC DD", "DD AA B CC", "DD AA B CC A"))
  embed_in_target <- gsub("AA", new_chr, c("AA B CC", "AA B", "B CC", "CC"))
  homophone <- gsub("AA", new_chr, c("AA B CC"))

  non_matches <- gsub("AA", new_chr, c("", "   ", "\t", "\t\t\t", "BB C DD", "CC B AA", "CC AA", "A B C", "A BB C", "A B", "BB CC", "B C"))

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
    expect_setequal(lexicon[get_cohorts(target, lexicon)], cohort)
    expect_setequal(lexicon[get_rhymes(target, lexicon)], rhyme)
  })

  test_that("neighbor are correct", {
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "a")], a_neighbor)
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "d")], d_neighbor)
    expect_setequal(lexicon[get_neighbors(target, lexicon, neighbor = "s")], s_neighbor)
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
    expect_setequal(lexicon[get_target_embeds_in(target, lexicon)], target_embed_in)
    expect_setequal(lexicon[get_embeds_in_target(target, lexicon)], embed_in_target)
  })

  test_that("homophone are correct", {
    expect_setequal(lexicon[get_homoforms(target, lexicon)], homophone)
  })
}
