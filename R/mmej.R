#' Get micro homology
#'
#' Get the micro homology around ref_end1 and ref_start2. The micro homology cannot cross the cleavage site.
#'
#' @param ref_end1 the alignment end postion in the upstream reference
#' @param ref_start2 the alignment start postion in the downstream reference
#' @param cut1 the cleavage position in the upstream reference
#' @param cut2 the cleavage position in the downstream reference
#' @param ref1 the upstream reference
#' @param ref2 the downstream reference
#' @return the micro homology sequence
#' @export
#'
#' @examples
#' ref1 <- "CTTGACGCGGCGCATGCAGAAGGGCCTGGGGTGGAAGGCGTCACCGTACT"
#' ref2 <- "TTACCGCACTTCACGGAGATCTTGCGATGCAGGGCGGGGCTCATCTCGTGTGGCAGCTTGGCCATGCTGAAGAGCACGTAGAACATCTCGTCCACGTTGGTGTTCTTCTT"
#' ref_end1 <- 3
#' ref_start2 = 61
#' cut1 <- 50
#' cut2 <- 60
#' mh <- get_micro_homology(ref_end1, ref_start2, cut1, cut2, ref1, ref2)
get_micro_homology <- function(ref_end1, ref_start2, cut1, cut2, ref1, ref2) {
  ref1 <- stringr::str_split_1(ref1, "")
  ref2 <- stringr::str_split_1(ref2, "")
  ref_start2 <- ref_start2 - length(ref1)
  cut2 <- cut2 - length(ref1)

  i1 <- ref_end1
  if (ref_end1 <= cut1) {
    for (i1 in seq(ref_end1, cut1)) {
      i2 <- ref_start2 + i1 - ref_end1
      if (i1 >= cut1 || i2 >= length(ref2)) {
        break
      }
      if (ref1[i1 + 1] != ref2[i2 + 1]) {
        break
      }
    }
  }

  j2 <- ref_start2 - 1
  if (ref_start2 - 1 >= cut2 - 1) {
    for (j2 in seq(ref_start2 - 1, cut2 - 1)) {
      j1 <- ref_end1 - ref_start2 + j2
      if (j2 < cut2 || j1 < 0) {
        break
      }
      if (ref2[j2 + 1] != ref1[j1 + 1]) {
        break
      }
    }
  }

  if (ref_end1 + 1 > i1) {
    mh_down <- ""
  } else {
    mh_down <- stringr::str_c(ref1[(ref_end1 + 1):i1], collapse = "")
  }
  if (j2 + 2 > ref_start2) {
    mh_up <- ""
  } else {
    mh_up <- stringr::str_c(ref2[(j2 + 2):ref_start2], collapse = "")
  }

  stringr::str_c(
    mh_up,
    mh_down
  )
}
