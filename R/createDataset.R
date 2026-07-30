#' Create a Dataset for Choice Experiment Analysis
#'
#' This function takes a design matrix and generates a dataset for use in choice
#' experiments. It either hands out the blocks of a blocked design, or draws a
#' random subset of choice sets for each respondent.
#'
#' @param design A data frame containing the design matrix for the choice
#'   experiment, one row per choice situation. It should include the column
#'   `Choice.situation` and optionally `Block`. Use [readdesign()] to produce it.
#'
#' @param respondents The number of respondents to generate data for.
#'
#' @param sets_per_resp Number of choice sets each respondent should see. Leave
#'   as `NULL` to use the design's own `Block` column, which is the usual case.
#'   When given, blocks are not used: each respondent instead receives a random
#'   draw of `sets_per_resp` choice sets from the design. See [draw_sets()].
#'
#' @param sample_sets How the random draw is taken when `sets_per_resp` is
#'   given. See [draw_sets()]. Ignored otherwise.
#'
#' @details
#' With a blocked design, respondents are assigned to blocks in rotation:
#' respondent 1 gets the first block, respondent 2 the second, and so on,
#' wrapping around. The number of respondents does not have to be a multiple of
#' the number of blocks; any remainder is spread over the first blocks.
#'
#' @return A data frame containing the augmented design matrix with additional columns:
#' \describe{
#'   \item{ID}{A unique identifier for each respondent.}
#'   \item{task}{The position of the choice set in that respondent's sequence, 1 to `sets_per_resp`.}
#'   \item{Choice.situation}{Which row of the design was shown.}
#'   \item{Block}{The block the respondent was assigned to, 1 if the design is not blocked.}
#'   \item{Other columns}{All original columns in the input `design` are retained.}
#' }
#'
#' @seealso [draw_sets()] for the random allocation used when `sets_per_resp` is given.
#'
#' @importFrom dplyr arrange slice row_number mutate relocate
#' @export
#'
#' @examples
#' # A blocked design: each respondent sees one block
#' design <- data.frame(
#'   Choice.situation = 1:12,
#'   Block = rep(1:3, each = 4),
#'   Attribute1 = rnorm(12),
#'   Attribute2 = sample(1:3, 12, replace = TRUE)
#' )
#' result <- createDataset(design, 9)
#' table(result$ID, result$Block)
#'
#' # An unblocked design: draw 4 of the 12 sets per respondent
#' unblocked <- design[, setdiff(names(design), "Block")]
#' drawn <- createDataset(unblocked, respondents = 9, sets_per_resp = 4)
#' table(drawn$Choice.situation)
createDataset <- function(design, respondents, sets_per_resp = NULL,
                          sample_sets = c("balanced", "random", "with_replacement")) {
  sample_sets <- match.arg(sample_sets)
  design <- check_design_frame(design)
  respondents <- check_count(respondents, "respondents")

  if (!is.null(sets_per_resp)) {
    return(draw_sets(design, respondents, sets_per_resp, sample_sets))
  }

  if (!("Block" %in% colnames(design))) {
    design$Block <- 1L # no blocks, so the whole design is one block
  }
  design$Block <- normalise_block(design$Block)

  block_levels <- sort(unique(design$Block))
  nblocks <- length(block_levels)
  block_sizes <- table(design$Block)

  if (length(unique(as.integer(block_sizes))) > 1L) {
    stop(
      "The blocks of this design have different sizes (",
      paste(sprintf("block %s: %d sets", names(block_sizes), as.integer(block_sizes)),
        collapse = ", "
      ),
      "). Every respondent must see the same number of choice sets. Either fix ",
      "the design, or ignore the blocks and draw sets at random by setting ",
      "`sets_per_resp`.",
      call. = FALSE
    )
  }

  setpp <- nrow(design) / nblocks # choice sets per respondent

  if (respondents < nblocks) {
    warning(
      "There are ", nblocks, " blocks but only ", respondents,
      " respondent(s), so block(s) ",
      and_list(block_levels[(respondents + 1L):nblocks]),
      " will not be used.",
      call. = FALSE
    )
  }

  ## Hand out blocks in rotation. Any remainder lands on the first blocks, which
  ## is why the number of respondents need not be a multiple of the block count.
  block_of_resp <- block_levels[((seq_len(respondents) - 1L) %% nblocks) + 1L]

  design <- dplyr::arrange(design, .data$Block, .data$Choice.situation)
  rows_by_block <- split(seq_len(nrow(design)), design$Block)
  row_index <- unlist(rows_by_block[as.character(block_of_resp)], use.names = FALSE)

  out <- design[row_index, , drop = FALSE]
  out$ID <- rep(seq_len(respondents), each = setpp)
  out$task <- rep(seq_len(setpp), times = respondents)

  out %>%
    dplyr::relocate("ID", "task", "Choice.situation") %>%
    `rownames<-`(NULL) %>%
    as.data.frame()
}


#' Draw a random subset of choice sets for each respondent
#'
#' @description
#' For designs that are not blocked. Instead of showing every respondent the same
#' block of choice situations, each respondent receives `sets_per_resp` sets drawn
#' from the design at random, in a random order.
#'
#' @param design A design data frame with one row per choice situation, as
#'   returned by [readdesign()]. It must not be blocked; see Details.
#' @param respondents Number of respondents to generate.
#' @param sets_per_resp Number of choice sets each respondent sees.
#' @param scheme How to draw. One of
#'   \describe{
#'     \item{`"balanced"`}{The default. Sets are handed out least-used first with
#'       ties broken at random, so every choice situation appears in the sample
#'       about equally often. The counts never differ by more than one. This keeps
#'       the design's efficiency and adds only the noise that comes from which
#'       respondent saw which set.}
#'     \item{`"random"`}{Each respondent's sets are sampled independently without
#'       replacement. Simple, but how often each set appears is itself random, so
#'       results are noisier than with `"balanced"`.}
#'     \item{`"with_replacement"`}{Sampled with replacement, so a respondent can
#'       see the same choice situation twice. Needed only when `sets_per_resp`
#'       exceeds the number of sets in the design.}
#'   }
#'
#' @details
#' Blocked designs already prescribe which sets go together, so mixing the two
#' allocation schemes makes no sense and is an error. Drop the `Block` column if
#' you want the sets drawn at random instead.
#'
#' Which sets a respondent sees is part of the data generating process, so
#' [sim_choice()] redraws the allocation for every simulation run by default. Draw
#' it once and you measure the performance of one particular lucky or unlucky
#' allocation. Set `resample = FALSE` in [sim_all()] to hold it fixed.
#'
#' Check how evenly the sets were used with `table(data$Choice.situation)`.
#'
#' @return A data frame with `respondents * sets_per_resp` rows, and the columns
#'   `ID`, `task`, `Choice.situation` and `Block` in front of the design's own
#'   columns.
#'
#' @seealso [createDataset()], which calls this when `sets_per_resp` is given.
#'
#' @export
#'
#' @examples
#' design <- data.frame(
#'   Choice.situation = 1:12,
#'   price = rep(c(1, 2, 3), 4),
#'   quality = rep(c(0, 1), 6)
#' )
#'
#' set.seed(1)
#' d <- draw_sets(design, respondents = 30, sets_per_resp = 4)
#'
#' # every respondent sees 4 distinct sets
#' table(d$ID)[1:5]
#'
#' # and the sets are used about equally often
#' table(d$Choice.situation)
draw_sets <- function(design, respondents, sets_per_resp,
                      scheme = c("balanced", "random", "with_replacement")) {
  scheme <- match.arg(scheme)
  design <- check_design_frame(design)
  respondents <- check_count(respondents, "respondents")
  sets_per_resp <- check_count(sets_per_resp, "sets_per_resp")

  if ("Block" %in% names(design)) {
    nblocks <- length(unique(design$Block))
    if (nblocks > 1L) {
      stop(
        "This design is blocked (", nblocks, " blocks), so it already says which ",
        "choice sets belong together. Drawing sets at random on top of that would ",
        "undo the blocking. Either drop `sets_per_resp` to use the blocks, or ",
        "remove the `Block` column from the design to draw at random.",
        call. = FALSE
      )
    }
  }

  n_sets <- nrow(design)

  if (sets_per_resp > n_sets && scheme != "with_replacement") {
    stop(
      "`sets_per_resp` is ", sets_per_resp, " but the design only has ", n_sets,
      " choice situation(s). Lower `sets_per_resp`, use a larger design, or set ",
      'sample_sets = "with_replacement" to let a respondent see the same set twice.',
      call. = FALSE
    )
  }

  design <- dplyr::arrange(design, .data$Choice.situation)

  row_index <- switch(scheme,
    "balanced" = draw_balanced(n_sets, respondents, sets_per_resp),
    "random" = unlist(
      lapply(seq_len(respondents), function(i) sample.int(n_sets, sets_per_resp)),
      use.names = FALSE
    ),
    "with_replacement" = sample.int(n_sets, respondents * sets_per_resp, replace = TRUE)
  )

  out <- design[row_index, , drop = FALSE]
  if (!"Block" %in% names(out)) out$Block <- 1L
  out$ID <- rep(seq_len(respondents), each = sets_per_resp)
  out$task <- rep(seq_len(sets_per_resp), times = respondents)

  out %>%
    dplyr::relocate("ID", "task", "Choice.situation") %>%
    `rownames<-`(NULL) %>%
    as.data.frame()
}


#' Deal choice sets out least-used first, ties broken at random
#'
#' Keeps the usage counts of all sets within one of each other, so the realised
#' sample is as close to balanced as the arithmetic allows, while which respondent
#' sees which set stays random.
#' @noRd
draw_balanced <- function(n_sets, respondents, sets_per_resp) {
  counts <- integer(n_sets)
  out <- integer(respondents * sets_per_resp)
  at <- 0L

  for (i in seq_len(respondents)) {
    pick <- order(counts, stats::runif(n_sets))[seq_len(sets_per_resp)]
    counts[pick] <- counts[pick] + 1L
    # randomise the order the sets are presented in
    out[(at + 1L):(at + sets_per_resp)] <- if (sets_per_resp > 1L) sample(pick) else pick
    at <- at + sets_per_resp
  }

  out
}


#' Check that a design is usable, with messages that say how to fix it
#' @noRd
check_design_frame <- function(design) {
  if (!is.data.frame(design)) {
    stop(
      "`design` must be a data frame with one row per choice situation, not ",
      describe_value(design),
      ". Read a design file with readdesign() first.",
      call. = FALSE
    )
  }
  if (nrow(design) == 0) {
    stop("`design` has no rows, so there are no choice situations to show.", call. = FALSE)
  }
  if (!"Choice.situation" %in% names(design)) {
    stop(
      "`design` needs a `Choice.situation` column numbering its rows. Columns ",
      "found: ", and_list(paste0("`", names(design), "`")),
      ". readdesign() adds this column for you.",
      call. = FALSE
    )
  }
  as.data.frame(design)
}

#' Turn a Block column of any type into something we can count and sort
#' @noRd
normalise_block <- function(block) {
  if (is.factor(block)) {
    block <- as.character(block)
  }
  if (is.character(block)) {
    numeric_try <- suppressWarnings(as.numeric(block))
    if (!anyNA(numeric_try)) block <- numeric_try
  }
  if (anyNA(block)) {
    stop(
      "The `Block` column has ", sum(is.na(block)), " missing value(s). Every ",
      "choice situation must belong to a block.",
      call. = FALSE
    )
  }
  block
}

#' Validate a positive whole number argument
#' @noRd
check_count <- function(x, arg, min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != trunc(x)) {
    stop(
      "`", arg, "` must be a single whole number, not ", describe_value(x), ".",
      call. = FALSE
    )
  }
  if (x < min) {
    stop("`", arg, "` must be at least ", min, ", not ", x, ".", call. = FALSE)
  }
  as.integer(x)
}
