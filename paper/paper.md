---
title: 'sluRm: A lightweight wrapper for HPC with Slurm'
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

Nowadays, high-performance-computing (HPC) clusters are commonly available tools for either **in** or **out** of cloud settings. [Slurm Workload Manager](https://slurm.schedmd.com/) (see @Jette02slurm:simple) is a program written in C that is used to efficiently manage resources in HPC clusters.

While the R programming language [@R] has not been developed for HPC settings, there are currently several ways in which R can be enhanced by means of HPC. The `sluRm` R package is one of those ways.

The `sluRm` R package provides tools for using R in HPC settings that work with Slurm. It provides wrappers and auxiliary functions that allow the user to seamlessly integrate their analysis pipeline with HPC, putting emphasis on providing the user with a family of functions similar to those that the `parallel` R package [@R] provides.

While there are other tools for integrating R in a HPC envirnment that works with Slurm, `sluRm` has some advantages regarding syntax, number of dependencies, and flexibility. 

1.  Compared to `rslurm` [@Marchand2018], this R package's workflow is closer to the `parallel` package. `sluRm` has wrappers such as `Slurm_lapply` and `Slurm_Map` that have a similar syntax to `parLapply` and `parMap` from the `parallel` package; which is not true for `rslurm` which depends on a single function `slurm_apply`.

2.  Compared to `batchtools` [@Bischl2015], and to `future.batchtools` [@Bengtsson2019] `sluRm` is significantly more lightweight as it has 0 dependencies on R packages other than those shipped with R. `batchtools` and `future.batchtools` have 20 and 24 dependencies each.[^date]

3.  In terms of flexibility, `sluRm`'s API is significantly more flexible compared to the other R packages mentioned. While both `rslurm` and `batchtools` allow the user to provide template files to create personalized job configurations for Slurm, in `sluRm` job configurations are created programatically.
    
[^date]: The number of dependencies was obtained from https://tinyverse.netlifly.com, which reports the number of recursive dependencies as of May 1st, 2019.

In summary, `sluRm` provides a purpose-built alternative for R users working in a HPC environment with Slurm.

# Funding and Support

This work is supported by the National Cancer Institute (NCI), Award Number 5P01CA196569.

# References
