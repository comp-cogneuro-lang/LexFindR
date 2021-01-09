#' slex ARPAbet
#'
#' TRACE slex lexicon translated by Nenadić and Tucker into ARPAbet
#' pronunciation
#'
#' TRACE slex lexicon with Frequencies: McClelland, J. L., & Elman, J. L.
#' (1986). The TRACE model of speech perception. Cognitive psychology, 18(1),
#' 1-86.
#'
#' APRAbet transcription: Nenadić, F., & Tucker, B. V. (2020). Computational
#' modelling of an auditory lexical decision experiment using jTRACE and TISK.
#' Language, Cognition and Neuroscience, 1-29.
#'
#'  @format A table with 212 rows and 2 variables:
#' \describe{
#'   \item{Item}{TRACE slex transcription}
#'   \item{Pronunciation}{ARPAbet transcription}
#'   ...
#' }
#' @source
#'   \url{https://era.library.ualberta.ca/items/61319cc6-436a-428c-b960-545bdc9bd5d3}
#'
"slex"

#' Lemmalex dictionary
#'
#' Lemmalex is primarily based on the SUBTLEXus subtitle corpus
#' (based on American subtitles with 51 million items in total)
#' reduced to lemma using a copyrighted database (Francis and Kučera, 1982).
#' The pronunciation is given by CMU Pronouncing Dictionary
#'
#'
#' Reference: Brysbaert, M., & New, B. (2009). Moving beyond Kučera and Francis:
#' A critical evaluation of current word frequency norms and the introduction of
#' a new and improved word frequency measure for American English. Behavior
#' research methods, 41(4), 977-990.
#'
#' Kučera, H., & Francis, W. N. (1967). Computational analysis of present-day
#' American English. Brown university press.
#'
#' CMU Pronouncing Dictionary: http://www.speech.cs.cmu.edu/cgi-bin/cmudict
#'
#'  @format A table with 20,293 rows and 3 variables:
#' \describe{
#'   \item{Item}{SUBTLEXus dictionary reduced to lemmas}
#'   \item{Frequency}{Number of times the item appeared in the SUBTLEXus corpus}
#'   \item{Pronunciation}{ARPAbet transcription according to CMU}
#'   ...
#' }
#' @source
#'   \url{https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus}
#'
"lemmalex"
