#' Check input and raise warnings as needed
#'
#' @inherit get_generic
#'
#' @keywords internal
check_input <- function(target, lexicon, sep) {
  tryCatch(
    error = function(cnd) {
      message(conditionMessage(cnd))
      FALSE
    },
    {
      if (length(target) > 1) {
        stop("`target` must be a single character element; \n* `", paste0(target, collapse = ", "), "` has length ", length(target), ".", call. = FALSE)
      }

      if (!is.character(target)) {
        stop("`target` must be a character input; \n* You've supplied a ", typeof(target), ".", call. = FALSE)
      }

      if (trimws(target) == "") {
        stop("`target` must not be an empty string", call. = FALSE)
      }


      if (length(sep) > 1) {
        stop("`sep` must be a single character element; \n* `", paste0(sep, collapse = ", "), "` has length ", length(sep), ".", call. = FALSE)
      }

      if (!is.character(sep)) {
        stop("`sep` must be a character input; \n* You've supplied a ", typeof(sep), ".", call. = FALSE)
      }


      if (length(lexicon) == 1 && trimws(lexicon) == "") {
        stop("`lexicon` must not be an empty string", call. = FALSE)
      }

      if (length(lexicon) == 1 && !is.character(lexicon)) {
        stop("`lexicon` must be a character input; \n* You've supplied a ", typeof(lexicon), ".", call. = FALSE)
      }

      check_chr <- lapply(lexicon, is.character)
      if (!suppressWarnings(all(check_chr))) {
        not_chr <- which(check_chr == FALSE)
        stop("`lexicon` must be a character vector; \n* Indices ", paste0(not_chr[1:min(5, length(not_chr))], collapse = ", "), "... are not character inputs.", call. = FALSE)
      }

      TRUE
    }
  )
}

#' Return lexical items the appropriate type
#'
#' @inherit get_generic
#'
#' @return the appropriate type
#'
#' @keywords internal
get_return <- function(idx, lexicon, form, count) {
  if (form) {
    return(lexicon[idx])
  } else if (count) {
    return(length(idx))
  }
  else {
    return(idx)
  }
}

#' Escape sep and target in regex
#'
#' @inherit get_generic
#'
#' @param split Whether to split the target based on sep
#' and escape every element in the character list
#'
#' @return escaped sep and target
#'
#' @keywords internal
add_escape <- function(target, sep, split = TRUE) {
  target <- trimws(target)

  # Escape sep in regex by enclosing them in `\\Q` and `\\E`
  if (sep != "") {
    sep <- paste0("\\Q", sep, "\\E")
  }

  # Split and then escape elements
  if (split) {
    split_target <- strsplit(target, sep)[[1]]
    split_target <- paste0("\\Q", split_target, "\\E")

    list(sep = sep, split_target = split_target)

    # Escape target as a whole
  } else {
    target <- paste0("\\Q", target, "\\E")
    list(sep = sep, target = target)
  }
}
