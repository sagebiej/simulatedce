# simulateDCE 0.4.0

This release makes random parameters usable end to end, fixes a set of defects that
produced silently wrong results, adds random allocation of choice sets for unblocked
designs, and replaces the vignettes.

## Breaking changes

* `utility_transform_type` now defaults to `"exact"` instead of the deprecated
  `"simple"`. `"exact"` matches parameter and variable names exactly rather than
  requiring coefficients to start with `"b"` and variables with `"alt"`, and is
  otherwise compatible. `"simple"` still works and still warns.
* Design names keep their underscores. Results used to be labelled with underscores
  stripped, because the old reshape split column names on them. That restriction is
  gone, so a design called `my_design.rds` is now labelled `my_design` rather than
  `mydesign`.
* `aggregateResults()` no longer draws the density plots as a side effect. They are
  returned in `$graphs`; pass `print_plots = TRUE` for the old behaviour. This also
  stops a stray `Rplots.pdf` appearing in non-interactive sessions.
* `reshape_type` is deprecated and ignored. Results are collected from each design
  directly instead of by taking wide column names apart, so there is nothing left to
  choose.
* `createDataset()` gained a `task` column numbering each respondent's choice sets,
  placed after `ID`.
* Dropped the `psych`, `kableExtra`, `evd` and `rmarkdown` dependencies. Gumbel
  draws are now `-log(rexp(n))`, which is bit-for-bit what `evd::rgumbel()` did, so
  existing seeds still reproduce.

## New features

* **Random choice sets for unblocked designs.** New `draw_sets()`, and
  `sets_per_resp`, `sample_sets` and `resample` arguments on `createDataset()`,
  `sim_choice()` and `sim_all()`. Instead of handing every respondent a fixed block,
  each respondent receives a random draw of `sets_per_resp` choice situations in a
  random order. Three schemes: `"balanced"` (the default) hands sets out least-used
  first so every situation appears about equally often, `"random"` samples
  independently per respondent, and `"with_replacement"` allows repeats. The
  allocation is redrawn for every simulation run by default, because which sets a
  respondent saw is part of the data generating process; set `resample = FALSE` to
  hold it fixed.
* **`truncated_normal` distribution** for `bcoeff`, with `mean`, `sd`, `min` and
  `max`. Either bound may be infinite, which is the usual way to keep a coefficient
  on one side of zero.
* **`bcoeff_moments()`** reports the mean and standard deviation each `bcoeff` entry
  implies, in the units of the coefficient itself. Useful for `lognormal` and
  `neg_lognormal`, whose `meanlog` and `sdlog` describe the underlying normal rather
  than the coefficient.
* **`readdesign()` accepts `designtype = "matrix"`** for a design you built yourself
  and saved as a plain data frame.
* **Per-parameter power** in `$power_by_par` and `$powa_by_par`, alongside the
  existing joint power. One weak attribute used to drag joint power down with no
  indication of which.
* **Convergence reporting** in `$convergence`: how many models converged, how many
  failed, the optimiser codes, and how many returned non-finite estimates.
  Non-converged runs are excluded from the summaries and `sim_choice()` warns.
* **`keep_models` and `keep_utilities`** to control memory. Each estimated model
  carries its own copy of the simulated data, which is what dominates memory in a
  long run: 60 runs of 300 respondents came to 23 MB with the models kept and
  0.08 MB without.
* **`workers`** to set the number of parallel processes explicitly, and **`pattern`**
  to control which files in `designpath` count as designs.
* `summaryall` and the per-design `summary` gained a `median` column, so a single
  badly behaved run no longer hides behind the mean.
* `$estimates` gives the long table of every estimate tagged by design, which is
  what the plots are built from.

## Bug fixes

* **Random parameters could not be estimated at all.** Respondent-level draws become
  columns in the dataset, and those column names were then also treated as data
  variables when building the mixl utility script, producing `@$bcost` and leaving
  mixl with no free parameters. `estimate = TRUE` with any random coefficient failed
  with "At least one parameter must not be fixed".
* **A `triangular` specification crashed `sim_all()`** in its own startup summary,
  which read `lower` and `upper` where `make_rand_params()` requires `min` and `max`.
* **`truepar` was lost for random parameters.** It was built with `unlist(bcoeff)`, so
  a distribution spec became several character entries named `b.dist`, `b.mean` and
  `b.sd`, the join never matched, and the whole column turned character. It now
  reports the mean of the mixing distribution, with the spread in a new `truesd`
  column.
* **`readdesign()` mangled plain data frame designs.** Anything that was not `.ngd`
  and lacked class `spdesign` was read as idefix, with no error. The package's own
  `Rbook/design1.RDS` came back with 1601 junk columns instead of 17. Detection now
  tests for each format positively.
* **`createDataset()` failed when the number of respondents was not a multiple of the
  number of blocks.** The remainder is now spread over the first blocks.
* **Blocks not numbered from 1 were silently collapsed.** The block count came from
  `max(Block)`, so blocks coded 0 and 1 were read as one block and every respondent
  received both. Factor and character block labels errored. Unequal block sizes now
  give a clear error rather than a cryptic one.
* **A design variable containing `U_`, `V_` or `e_` corrupted `CHOICE`.** The utility
  columns were found with an unanchored `grep`, so a column called `U_pay` joined the
  argmax. A column named exactly `U_1` or `CHOICE` is now refused up front.
* **`sim_all()` reported the wrong runtime in sequential mode.** `simulate_choices()`
  leaked five `tictoc` timers per call, because the matching `toc()` sat inside a
  lazily evaluated message argument and never ran below verbose level 3. `sim_all()`
  then popped one of those instead of its own. The timings are no longer built on a
  shared stack, and the `verbose = 3` timings actually print a duration where before
  they printed blank lines.
* **A respondent could be split across two decision groups.** Groups were cut on row
  number, so a boundary falling inside someone's block of choice sets gave them one
  decision rule for some tasks and another for the rest. Whole respondents are now
  assigned to a group.
* **`chunks > 1` wrote scratch files to a directory that did not exist** whenever the
  design's path contained an underscore, because underscores were stripped from the
  whole path rather than the file name. Scratch files now go to the session's
  temporary directory and are cleaned up. Unescaped dots in the same pattern turned
  `xngd_1.ngd` into `1`, and a lowercase `.rds` extension was not removed at all.
* **`sim_all(savefile = )` broke if the folder held anything else.** It read every
  file in the directory and matched results to designs by position. It now reads back
  exactly the files the run wrote, matched by name.
* **A `preprocess_function` returning `NULL` crashed**, although the validation
  explicitly allowed it.
* **Parallel mode destroyed the caller's `future` plan**, resetting it to sequential
  on exit instead of restoring it. This silently undid a plan configured for a
  cluster.
* **Design names that were a prefix of one another mangled the aggregated results**,
  because the design was identified by an unanchored regex alternation.
* **A subdirectory or stray file in `designpath` broke the run.** Only files matching
  `pattern` are treated as designs.
* **Power silently dropped runs.** An NA p-value made `all()` return NA, `table()`
  discarded it, and the percentages were computed over the survivors. The table also
  had no `FALSE` entry when every run happened to be significant, so reading
  `power["FALSE"]` gave NA.
* **A separated model reported success.** maxLik returns code 0 even when a
  coefficient has run off to several hundred with a matching standard error, which
  dragged the mean of every summary. Runs with non-finite estimates are now treated as
  failures, and a run where nothing is estimable gives an error that explains what to
  check.
* **`dist = "fixed"` without a `value` silently dropped the parameter** from the
  draws rather than complaining.
* `readdesign()` read the same RDS file twice when detecting the type.
* `simulate_choices()` recompiled the utility functions on every run. They are now
  compiled once per design.
* The optimiser trace that mixl prints unconditionally is captured below verbose
  level 3.

## Other changes

* Every error and warning message was rewritten to name the argument at fault, say
  what was expected, and suggest what to do. Unknown values for `designtype` and
  `dist` now suggest the closest valid one.
* The distributions live in one registry that knows their arguments, how to draw from
  them, and their moments, so adding one is a single edit and cannot half-work.
* `verbose` is validated, and `readdesign()` follows it when called from `sim_all()`.
* Parallel mode starts its workers once for all designs rather than once per design.
* `arguments` replaces the misspelled `arguements` in the returned list. The old name
  is kept as an alias.
* **The vignettes have been replaced**, one per feature: getting started, reading
  design files, random parameters, decision groups, manipulations, unblocked designs,
  and working with results. Most build small designs on the fly so they run in
  seconds.
* The test suite grew from 89 tests to over 300, including direct coverage of
  `createDataset()`, `sim_choice()`, `draw_sets()`, the distribution registry and the
  internal helpers, plus a regression test for every defect above.
* Untracked `.DS_Store` and `.RData`, and extended `.Rbuildignore`.

# simulateDCE 0.3.2

## Bug fixes
 * removed 'setspp' argument from simulate_choices as it was not used.
 * switched to `qs2` to store data efficiently as `qs` was removed from CRAN.


# simulateDCE 0.3.1


## Major Changes:
 * speed improvements to simulate_choices function, which affects the sim_all function as well.


## Bug fixes
 * removed 'estimate' argument from simulate_choices as it was not used.


# simulateDCE 0.3.0

## Major Changes:
- **Improved `sim_all` function**:
  - Returns additional outputs: beta parameters, utility functions, manipulations, and decision group arguments.
  - Parallel processing partly implemented
  - Allows to save chunks of data to disk, which is useful for large datasets and instable machines when simulations are large
  - New functions for better modularity

# simulateDCE 0.2.0

## Enhancements:
- **`readdesign` function**: 
  - Can now automatically guess the `designtype` (useful for `sim_all` with mixed `ngene` and `spdesign` design files).
- **`sim_all` function**: 
  - Returns more information, including beta parameters and decision-related arguments.

# simulateDCE 0.1.2

## Initial Release:
- First stable version. Includes fully working examples.
- Can be used by anyone.
