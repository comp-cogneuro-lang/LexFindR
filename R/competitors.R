#' Get competitors in spoken word recognition
#'
#' @param target Character string containing a target word
#' @param lexicon Character vector containing the lexical database
#' @param sep Separator in target and lexicon
#' @param form Whether to return words in lexicon
#' @param count Whether to return count of words
#' @param neighbors (\emph{get_neighbors} only) Character vector specifying the
#'   type of neighbor to return. Return the delete, add, substitute neighbors of
#'   the target when 'd', 'a', and/or 's' is in neighbors respectively
#' @param overlap (\emph{get_cohorts} only) Integer specifying the number of
#'   onset phonemes to overlap for matching with the target word
#' @param mismatch (\emph{get_rhymes} only) Integer specifying the number of
#'   onset phonemes to mismatch for matching with the target word
#'
#' @return the indexes of the competitors in the lexical database
#'
#' @keywords internal
get_generic <- function(target, lexicon, neighbors, sep, form, count, overlap, mismatch) {
  UseMethod("get_generic")
}


#' Get cohort competitors
#'
#' Cohorts overlap in onset phoneme(s).
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_cohorts("AA R K", c("AA R K", "AA R T", "B AA B"))
get_cohorts <- function(target, lexicon, sep = " ", form = FALSE, count = FALSE, overlap = 2) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  # and split target by sep.
  escaped_list <- add_escape(target, sep)
  sep <- escaped_list$sep
  split_target <- escaped_list$split_target

  # Cohorts are defined only when target length is more than overlap
  if (length(split_target) >= overlap) {
    # Regex pattern includes overlapping onset phonemes in target and `"((?=", sep, ").+)?$"`
    # allows any subsequent phonemes separated by sep.
    pattern <- paste0(split_target[1:overlap], collapse = sep)
    pattern <- paste0("^", pattern, "((?=", sep, ").+)?$")

    # Get index in lexicon
    idx <- grep(pattern, lexicon, perl = TRUE)
  } else {
    idx <- integer(0)
  }

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get rhyme competitors
#'
#' Rhymes overlap in all except onset phoneme(s)
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_rhymes("AA R K", c("AA R K", "B AA R K", "B AA B"))
get_rhymes <- function(target, lexicon, sep = " ", form = FALSE, count = FALSE, mismatch = 1) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  # and split target by sep.

  escaped_list <- add_escape(target, sep)
  sep <- escaped_list$sep
  split_target <- escaped_list$split_target

  # When the target is longer than phonemes to mismatch, rhymes can be longer or
  # shorter than target, or else rhyme can only be longer
  if (length(split_target) > mismatch) {
    # Regex pattern includes optional phonemes `^.{,mismatch}`, and "^(((?!",
    # sep, ").)+", sep, "){0,", mismatch, "}" to match rhymes longer than
    # target, optional mismatched phoneme in target dis_pat (e.g. target="A B C",
    # mismatch=2, dis_pat=((A )?B )?) and overlap after the mismatched phonemes
    # according to paste0(split_target[-(1:mismatch)], collapse = sep), "$"
    dis_pat <- paste0(
      paste0(rep("(", mismatch), collapse = ""),
      paste0(split_target[1:mismatch],
        collapse = paste0(sep, ")?", collapse = "")
      ),
      sep,
      ")?",
      collapse = ""
    )
    if (sep == "") {
      pattern <- paste0("^.{0,", mismatch, "}", dis_pat, paste0(split_target[-(1:mismatch)], collapse = sep), "$")
    } else {
      pattern <- paste0("^(((?!", sep, ").)+", sep, "){0,", mismatch, "}", dis_pat, paste0(split_target[-(1:mismatch)], collapse = sep), "$")
    }
  } else {
    # Regex pattern includes optional phonemes `^.{,mismatch}`, and "^(((?!",
    # sep, ").)+", sep, "){0,", mismatch, "}" to match rhymes longer than
    # target, optional mismatched phoneme in target dis_pat (e.g. target="A B",
    # mismatch=2, dis_pat=(A )? now excluding the last phoneme, and overlap with
    # the last phoneme according to split_target[length(split_target)], "$")
    if (length(split_target) > 1) {
      dis_pat <- paste0(
        paste0(rep("(", length(split_target) - 1), collapse = ""),
        paste0(split_target[-length(split_target)],
          collapse = paste0(sep, ")?", collapse = "")
        ),
        sep,
        ")?",
        collapse = ""
      )
    }
    else {
      dis_pat <- ""
    }
    if (sep == "") {
      pattern <- paste0("^.{0,", mismatch, "}", dis_pat, split_target[length(split_target)], "$")
    } else {
      pattern <- paste0("^(((?!", sep, ").)+", sep, "){0,", mismatch, "}", dis_pat, split_target[length(split_target)], "$")
    }
  }


  # Get index in lexicon
  idx <- grep(pattern, lexicon, perl = TRUE)

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get embedded competitors
#'
#' Embedded competitors are items which the target embedded in.
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_target_embeds_in("AA R K", c("AA R K", "B AA R K", "B AA B"))
get_target_embeds_in <- function(target, lexicon, sep = " ", form = FALSE, count = FALSE) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  escaped_list <- add_escape(target, sep, split = FALSE)
  sep <- escaped_list$sep
  target <- escaped_list$target

  # Regex pattern `^(.+(?<=", sep, "))?"` `"((?=", sep, ").+)?$"`
  # allows optional phonemes before and after the target respectively,
  # and the lookbehind `(?<=", sep, ")` and lookahead `(?=", sep, ")`
  # requires a sep to come between any phonemes
  # before and after the target respectively,
  # to avoid erroreous embeddings like 'AA' embedded in 'BAAB'
  pattern <- paste0("^(.+(?<=", sep, "))?", target, "((?=", sep, ").+)?$")

  # Get index in lexicon
  idx <- grep(pattern, lexicon, perl = TRUE)

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get embedding competitors
#'
#' Embedding competitors are items embedded in target
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_embeds_in_target("AA R K", c("AA R K", "AA R", "B AA B"))
get_embeds_in_target <- function(target, lexicon, sep = " ", form = FALSE, count = FALSE) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  # and split target by sep.
  escaped_list <- add_escape(target, sep)
  sep <- escaped_list$sep
  split_target <- escaped_list$split_target

  pattern_list <- c()

  # List all possible embeddings
  for (start in seq(length(split_target))) {
    for (end in seq(start, length(split_target))) {
      pattern <- paste(split_target[start:end], collapse = sep)
      pattern_list <- append(pattern_list, pattern)
    }
  }

  # Add start and end regex markers '^' and '$'
  # to get the exact match to all possible embeddings.
  pattern_list <- paste0("^", pattern_list, "$")

  # Join them using using regex pattern OR `|`
  pattern <- paste(pattern_list, collapse = "|")

  # Get index in lexicon
  idx <- grep(pattern, lexicon, perl = TRUE)

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get phonological neighbors
#'
#' Phonological neighbors are items which can be converted
#' to the target by one add, delete and substitute operation
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_neighbors("AA R K", c("AA R K", "AA R", "B AA B"), "d")
#' get_neighbors("AA R K", c("AA R K", "AA R", "B AA B"), "da")
#' get_neighbors("AA R K", c("AA R K", "AA R", "B AA B"), "das")
get_neighbors <- function(target, lexicon, neighbors = "das", sep = " ", form = FALSE, count = FALSE) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  # and split target by sep.
  escaped_list <- add_escape(target, sep)
  sep <- escaped_list$sep
  split_target <- escaped_list$split_target

  pattern_list <- c()

  # List all possible 'd' neighbours
  if (grepl("d", neighbors, fixed = TRUE)) {
    for (i in seq((split_target))) {
      pattern <- paste(split_target[-i], collapse = sep)
      pattern_list <- append(pattern_list, pattern)
    }
  }

  # List all possible 'a' neighbours
  if (grepl("a", neighbors, fixed = TRUE)) {
    for (i in seq(0, length(split_target))) {
      if (sep == "") {
        pattern <- append(split_target, ".", i)
      } else {
        pattern <- append(split_target, paste0("((?!", sep, ").)+"), i)
      }
      pattern <- paste(pattern, collapse = sep)
      pattern_list <- append(pattern_list, pattern)
    }
  }

  # List all possible 's' neighbours
  if (grepl("s", neighbors, fixed = TRUE)) {
    for (i in seq(split_target)) {
      if (sep == "") {
        pattern <- append(split_target[-i], ".", i - 1)
      } else {
        pattern <- append(split_target[-i], paste0("((?!", sep, ").)+"), i - 1)
      }
      # pattern <- append(split_target[-i], "\\w+", i - 1)
      pattern <- paste(pattern, collapse = sep)
      pattern_list <- append(pattern_list, pattern)
    }
  }

  # Add start and end regex markers '^' and '$'
  # to get the exact match to all possible embeddings.
  pattern_list <- paste0("^", pattern_list, "$")

  # Join them using using regex pattern OR `|`
  pattern <- paste(pattern_list, collapse = "|")

  # Get index in lexicon
  idx <- grep(pattern, lexicon, perl = TRUE)

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get homophones
#'
#' Homophones are items which sound similar to the target
#'
#' @inherit get_generic
#'
#' @export
#'
#' @examples
#' get_homoforms("AA R K", c("AA R K", "AA R", "B AA B"))
get_homoforms <- function(target, lexicon, sep = " ", form = FALSE, count = FALSE) {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  escaped_list <- add_escape(target, sep, split = FALSE)
  sep <- escaped_list$sep
  target <- escaped_list$target

  # Add start and end regex markers '^' and '$'
  # to get the exact match
  pattern <- paste0("^", target, "$")

  # Get index in lexicon
  idx <- grep(pattern, lexicon, perl = TRUE)

  # Return the appropriate type
  get_return(idx, lexicon, form, count)
}

#' Get phonological uniqueness point
#'
#' Phonological uniqueness point is the index at which the
#' target becomes unique in the lexicon
#'
#' @inherit get_generic
#'
#' @export
#'
#' @return Target is not unique: length + 1, else index where target
#'   becomes unique in lexicon
#'
#' @examples
#' get_uniqpt("AA R K", c("AA R", "B AA B", "B AA R K"))
get_uniqpt <- function(target, lexicon, sep = " ") {
  # Check input and give warning as needed
  if (!check_input(target, lexicon, sep)) {
    return(NA)
  }

  # Escape any reserved regex characters in target and sep
  # and split target by sep.
  escaped_list <- add_escape(target, sep)
  sep <- escaped_list$sep
  split_target <- escaped_list$split_target

  # Iterate through the possible substrings
  # from the start of the target
  for (i in seq(split_target)) {
    pattern <- paste0(split_target[1:i], collapse = sep)

    # Regex pattern `^` searches string which start with the substring
    # and `((?=", sep, ").+)?$` allows any overlap in subsequent phonemes
    pattern <- paste0("^", pattern, "((?=", sep, ").+)?$")

    # Get all possible items which starts with the substring
    value <- grep(pattern, lexicon, perl = TRUE, value = TRUE)

    # Substring is unique when there is no possible items
    # or when the only possible items is itself
    if (length(value) == 0 ||
      (length(value) == 1 && value == target)) {
      return(i)
    }
  }

  # When target is not unique, return num phonemes + 1
  return(i + 1)
}
