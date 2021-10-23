
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LexFindR

<!-- badges: start -->

[![R-CMD-check](https://github.com/maglab-uconn/LexFindR/workflows/R-CMD-check/badge.svg)](https://github.com/maglab-uconn/LexFindR/actions)
<!-- badges: end -->

This package allows language researchers to generate lexical competitors
for a given set of words. Generating these competitors is useful for
both experimental control (i.e., balancing a word list based on known
lexical competitor types) and testing hypotheses about how lexical
competitors may influence aspects of language processing. The package
includes many competitor types frequently studied in word recognition
research, such as cohorts, neighbors, and rhymes (along with many
others), and also makes use of lexical dimensions such as lexical
frequency to calculate measures like frequency weighted competitor
probabilities. Importantly, the package can be modified so that
researchers can add novel competitor definitions suitable for their
research questions.

## How to use the package with a small lexicon

Let’s say you wanted to get the cohorts of ark in a small word set: ark,
art, and bab:

``` r
library(LexFindR)

# Get cohort index of ark in dictionary of ark, art and bab
target <- "AA R K"
lexicon <- c("AA R K", "AA R T", "B AA B")

cohort <- get_cohorts(target, lexicon)
cohort
#> [1] 1 2

# To get string rather than the index
lexicon[cohort]
#> [1] "AA R K" "AA R T"

# Get count
length(cohort)
#> [1] 2

# Get the log Frequency Weighted Competitor Probabilities
target_freq <- 50
lexicon_freq <- c(50, 274, 45)

get_fwcp(target_freq, lexicon_freq)
#> [1] 0.2934352
```

## Installation

You can install the released version of LexFindR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("LexFindR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maglab-uconn/LexFindR")
```

## Using larger lexicons

The package comes with two lexicons: the 212-word *slex* lexicon (with
only 14 phonemes) from the TRACE model of spoken word recognition
\[@TRACE\] as a small data set for the user to experiment with, and a
larger lexicon (*lemmalex*) that we compiled from various open-access,
non-copyrighted materials. Let’s say that we wanted to find the rhymes
of ARK within the *lemmalex* lexicon.

Running the most basic version of the command will give us the rhyme
indices:

``` r
get_rhymes("AA R K", lemmalex$Pronunciation)
#> [1]   767  1217  3826  7094  8785  9434 11073 14010
```

If we want to get the actual competitors, we can run the following,
where the first command will show the forms and the second command will
show the orthographic labels:

``` r
get_rhymes("AA R K", lemmalex$Pronunciation, form = TRUE)
#> [1] "AA R K"    "B AA R K"  "D AA R K"  "HH AA R K" "L AA R K"  "M AA R K" 
#> [7] "P AA R K"  "SH AA R K"

lemmalex[get_rhymes("AA R K", lemmalex$Pronunciation),]$Item
#> [1] "arc"   "bark"  "dark"  "hark"  "lark"  "mark"  "park"  "shark"
```

## Note on the form of the lexicon

Note that it is important to strip lexical stress.

``` r
# Not stripping lexical stress will result in errors
target <- "AA0 R K"
lexicon <- c("AA1 R K", "AA2 R T", "B AA3 B")

get_cohorts(target, lexicon)
#> integer(0)

# Strip lexical stress using regex
target <- gsub("\\d", "", target)
lexicon <- gsub("\\d", "", lexicon)

print(target)
#> [1] "AA R K"
print(lexicon)
#> [1] "AA R K" "AA R T" "B AA B"

get_cohorts(target, lexicon)
#> [1] 1 2
```

## How to find competitors for the whole lexicon

In the examples above, we had one target word that we were analyzing.
Often, however, we will want to find competitors for each word in our
lexicon. Using the *lapply* function, this is possible:

``` r
# define the list of target words to compute cohorts for
target_df <- slex

# specify the lexicon; here it is the same, as we want 
# to find all cohorts for all words in our lexicon
lexicon_df <- target_df

# we create "cohort_idx", a list of indices
# corresponding to each word's cohort set
target_df$cohort_idx <-
  lapply(
    target_df$Pronunciation,
    FUN = get_cohorts,
    lexicon = lexicon_df$Pronunciation
  )

# to see the forms, create cohort_str
target_df$cohort_str <- 
  lapply(
    target_df$cohort_idx, function(idx) {
      lexicon_df$Item[idx]
    }
  )

# to see frequencies for each target's cohort
target_df$cohort_freq <- 
  lapply(
    target_df$cohort_idx, function(idx) {
      lexicon_df$Frequency[idx]
    }
  )

# to get the count of cohorts for each item,
target_df$cohort_count <- lengths(target_df$cohort_str)
```

## Parallelization

In order to get faster run times, we can make use of the package
*future* to engage multiple cores. Using the same example from above
with a larger lexicon, we replace *lapply* with *future\_lapply*:

``` r
library(future.apply)

# get the total number of cores
# num_cores <- availableCores()

# using two cores for demo
num_cores <- 2

plan(multisession, workers = num_cores)

# we use a larger lexicon here
target_df <- lemmalex
lexicon_df <- target_df

# get the indices of the cohorts
target_df$cohort_idx <-
  future_lapply(
    target_df$Pronunciation,
    FUN = get_cohorts,
    lexicon = lexicon_df$Pronunciation
  )
```

As in the example about finding competitors in the whole lexicon, the
above parallelization example can be applied to get the forms of the
cohorts above, the frequencies, etc.

## Further use and information

For much more detailed discussion of the package and its features, refer
to LexFindR manusript. Preprint: <https://psyarxiv.com/8dyru/>. Open
access: <https://rdcu.be/cyH0M>. Citation: "Li, Z., Crinnion, A.M. &
Magnuson, J.S. (2021). LexFindR: A fast, simple, and extensible R
package for finding similar words in a lexicon. Behavior Research
Methods, 1-15. <https://doi.org/10.3758/s13428-021-01667-6>.
