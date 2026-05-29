# simulateDCE 0.3.3

## Major changes
* **Mixed logit / random parameters**: `bcoeff` in `simulate_choices()`, `sim_choice()`,
  and `sim_all()` now accepts random distribution specifications alongside fixed scalars.
  Each parameter can be a numeric scalar (fixed) or a named list with a `dist` element
  (`"normal"`, `"lognormal"`, `"neg_lognormal"`, `"uniform"`, `"triangular"`).
  Respondent-level draws are generated via the new `make_rand_params()` function and
  merged into the dataset before utility calculation.

## New functions
* `make_rand_params()`: generates a respondent-level data frame of parameter draws from
  a `bcoeff` specification list.

## Bug fixes
* `aggregateResults(fromfolder = )` now works as documented: it reads the saved `.qs`
  design outputs from a folder and merges them. Previously the loaded files were
  discarded. Saved design outputs are now self-describing (each stores its `bcoeff` and
  `designname`), so results from independent runs can be combined later (e.g. simulate
  three designs now and add a fourth afterwards).

## Other changes
* `sim_all()` now prints a parameter summary at startup listing each coefficient with its
  type and moments (fixed value, or distribution name and parameters).
* Removed unused `randtoolbox` dependency.
* Documentation for `bcoeff` is now centralised in `make_rand_params()` and inherited
  by `simulate_choices()`, `sim_choice()`, and `sim_all()` via `@inheritParams`.

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
