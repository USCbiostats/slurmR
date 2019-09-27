---
title: 'slurrm: A lightweight wrapper for HPC with Slurm'
authors:
- affiliation: 1
  name: George G Vega Yon
  orcid: 0000-0002-3171-0844
- affiliation: 1
  name: Paul Marjoram
  orcid: 0000-0003-0824-7449
date: "22 April 2019"
output:
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
- hpc
- batch
- slurm
- parallel computing
affiliations:
- index: 1
  name: Department of Preventive Medicine, University of Southern California
---

# Summary

Nowadays, high-performance-computing (HPC) clusters are commonly available tools for either **in** or **out** of cloud settings. [Slurm Workload Manager](https://slurm.schedmd.com/) (see @Jette02slurm) is a program written in C that is used to efficiently manage resources in HPC clusters.

While the R programming language [@R] has not been developed for HPC settings, there are currently several ways in which R can be enhanced by means of HPC. The `slurrm` R package is one of those ways.

The `slurrm` R package provides tools for using R in HPC settings that work with Slurm. It provides wrappers and auxiliary functions that allow the user to seamlessly integrate their analysis pipeline with HPC, putting emphasis on providing the user with a family of functions similar to those that the `parallel` R package [@R] provides.

While there are other tools for integrating R in a HPC envirnment that works with Slurm--see for example `rslurm`[@Marchand2018], `batchtools` [@Bischl2015], `drake` [@Landau2018], `future.batchtools` [@Bengtsson2019], `clustermq` [@Schubert2019]--`slurrm` has some advantages regarding syntax, number of dependencies, and flexibility (in terms of the integration with Slurm itself). In particular, you may want to use `slurrm` if you:

1. Need a dependency-free tool. Besides of Slurm itself[^actually], this R package only depends on other R packages that are part of base R.,

[^actually]: In fact, users can install this R package regardless of whether they have or they don't have Slurm on their systems. The debug mode of this software allows users to setup jobs (including R scripts and batch files) without having to submit them to a Slurm job-scheduler.

2. need an R package that is fully integrated with Slurm, e.g., submitting jobs with an arbitrary set of Slurm parameters without the need of using templates, call Slurm commands from within R like `sacct`, `scancel`, `squeue`, `sbatch`, etc. with their corresponding flags, and

3. what to use an R package that is ready-to-go. Once loaded, users can submit jobs by just specifying how many cores, for example, they need.

Other features that are included with this R package, and that are available in some others, are:

4. Use a syntax similar to the apply family of functions in the parallel R package, including `Slurm_lapply`, `Slurm_sapply`, `Slurm_EvalQ`, and `Slurm_Map`, 

5. resubmit failed jobs: A very common issue with heterogenous computing clusters is the fact that some jobs succeed while others fail. Partial-job-resubmission is out-of-the-box as users can specify which jobs (as in Job Arrays) should be re-run.

Both of the latter two also available in `batchtools`. A comparison table of R packages that work with Slurm is available at https://github.com/USCbiostats/slurrm.

In summary, `slurrm` provides a dependency-free and purpose-built alternative for R users working in a HPC environment with Slurm.

# Funding and Support

This work is supported by the National Cancer Institute (NCI), Award Number 5P01CA196569.

# References
