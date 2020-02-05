# slurmR 0.3-1

## Bug fixes

* Fixed bug when collecting failed jobs. slurmR would fail to
  correctly list failed jobs and thus to collect outputs. This would also
  would affect when trying to collect partially resubmitted jobs.
  
* Slurm options passed via `opts_slurmR` are now passed to
  `Slurm_*apply` as documented. Thanks to Gregory Penn (gregorypenn) who
  reported the bug.
  
* `slurmr_cmd` now expands the `cmd_path`.


## Misc
  
* Increased code coverage.

* The function `wait_slurm()` is now exported and documented.

* Each line in the written R script is now wrapped with a custom
  `tryCatch` function (`tcq`). On error, this new function will collect both
  the expression called, the error message, and will quit the R session, so now
  the user has a better description of possible errors, e.g. missing
  packages.



# slurmR 0.3-0

* Name changed from `sluRm` to `slurmR`.

# slurmR 0.2-0

* JOSS review (finalizing).

# slurmR 0.1-0

* Added a `NEWS.md` file to track changes to the package.

