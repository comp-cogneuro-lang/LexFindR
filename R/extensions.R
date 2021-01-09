#' Get the log Frequency Weighted Competitor Probability (FWCP)
#'
#' @param target_freq Frequency of target word
#' @param competitors_freq Numeric vector containing the frequencies of
#'   competitors (including itself)
#' @param pad Value to add to frequencies before taking log; if your
#'   minimum frequency is 0, consider adding a value between 1 and 2;
#'   if your minimum frequency is between 0 and 1, consider adding 1
#' @param add_target Boolean; set to TRUE if you want the target
#'   frequency added to the denominator; only do this if the target is
#'   not already included in the competitor set (e.g., if the target
#'   is in the lexicon, it will be captured as its own neighbor, its
#'   own cohort, etc.)
#'
#' @return log FWCP
#' @export
#'
#' @examples
#' get_fwcp(100, c(10, 50), pad = 1)
get_fwcp <- function(target_freq, competitors_freq, pad = 0, add_target = FALSE) {
  ltf <- log(target_freq + pad)
  fw <- get_fw(competitors_freq, pad)
  if (fw == 0) {
    return(1.0)
  } else {
    if (add_target) {
      fw <- fw + ltf
    }
    if (fw > 0) {
      result <- (ltf / fw)
      if (result > 1) {
        warning("log target frequency (", round(ltf, 2),
          ") > summed log competitor frequencies (", round(fw, 2),
          "); \n* FWCP (", round(result, 2), ") is > 1",
          call. = FALSE
        )
      }
      return(result)
    } else {
      return(0)
    }
  }
}

#' Get the log Frequency Weight (FW) of a competitor set
#'
#' @inheritParams get_fwcp
#'
#' @return FW
#' @export
#'
#' @examples
#' get_fw(c(10, 50), pad = 1)
get_fw <- function(competitors_freq, pad = 0) {
  if (length(competitors_freq) == 0) {
    return(0.0)
  } else {
    min_padded_freq <- min(competitors_freq) + pad
    if (min_padded_freq < 1) {
      warning("`min(competitors_freq) + pad` is ", min_padded_freq,
        " which is < 1; \n* Consider adding pad >= ",
        1 - min(competitors_freq),
        call. = FALSE
      )
    }
    sum(log(competitors_freq + pad))
  }
}

#' Get nohorts
#'
#' Items which are both cohorts and neighbors
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_nohorts("AA R K", c("AA R K", "AA R", "B AA B"), neighbors = "das")
get_nohorts <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  idx <- intersect(
    get_cohorts(target, lexicon, sep, form = FALSE, count = FALSE),
    get_neighbors(target, lexicon, neighbors, sep, form = FALSE, count = FALSE)
  )

  get_return(idx, lexicon, form, count)
}

#' Get CohortsPrime
#'
#' Cohorts that are not neighbors
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_cohortsP("AA R K", c("AA R K", "AA R", "B AA B"), neighbors = "das")
get_cohortsP <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  idx <- setdiff(
    get_cohorts(target, lexicon, sep, form = FALSE, count = FALSE),
    get_neighbors(target, lexicon, neighbors, sep, form = FALSE, count = FALSE)
  )

  get_return(idx, lexicon, form, count)
}

#' Get NeighborssPrime
#'
#' Neighbors which are not cohorts or rhymes
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_neighborsP("AA R K", c("AA R K", "AA R", "B AA B"), neighbors = "das")
get_neighborsP <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  idx <- setdiff(
    setdiff(
      get_neighbors(target, lexicon, neighbors),
      get_cohorts(target, lexicon, sep, form = FALSE, count = FALSE)
    ),
    get_rhymes(target, lexicon, sep, form = FALSE, count = FALSE)
  )

  get_return(idx, lexicon, form, count)
}



#' Get embeds-in-target PRIME
#'
#' Items embedded in the target which are not cohorts or neighbors
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_embeds_in_targetP("B AA R K IY", c("AA R K", "AA R", "AA R K IY", "B AA R"))
get_embeds_in_targetP <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  idx <- setdiff(
    setdiff(
      get_embeds_in_target(target, lexicon, sep, form = FALSE, count = FALSE),
      get_cohorts(target, lexicon, sep, form = FALSE, count = FALSE)
    ),
    get_neighbors(target, lexicon, neighbors)
  )
  get_return(idx, lexicon, form, count)
}


#' Get target-embeds-in PRIME
#'
#' Items the target embeds into which are not cohorts or neighbors
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_target_embeds_inP("B AA R K", c("AA R K", "AA R", "B AA R K IY", "B AA R"))
get_target_embeds_inP <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  idx <- setdiff(
    setdiff(
      get_target_embeds_in(target, lexicon, sep, form = FALSE, count = FALSE),
      get_cohorts(target, lexicon, sep, form = FALSE, count = FALSE)
    ),
    get_neighbors(target, lexicon, neighbors)
  )
  get_return(idx, lexicon, form, count)
}
