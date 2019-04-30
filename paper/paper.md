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
- metropolis-hastings
- mcmc
- markov chain monte carlo
- transition kernel
- automatic convergence
affiliations:
- index: 1
  name: Department of Preventive Medicine, University of Southern California
---

# Summary

Now a days, high-performance-computing (HPC) clusters are commonly available tool for either **in** or **out** cloud settings. [Slurm Workload Manager]() is a C program writte to manage these resources in such a way that the users take the most out of it while at the same time stablishing rules so that no user abuses the use of the resource.

While the R programming language has not been developed for HPC settings, there are currently several ways in which R can be enhance by means of HPC. The `sluRm` R package is one of those ways.

The `sluRm` R package provides bidings for using R in HPC settings that work with Slurm. It provides wrappers and auxiliary functions that allow the user to seamlessly integrate they analysis pipeline with HPC, putting emphasis on providing the user with a family of functions similar to what the `parallel` R package provides.

While there are other tools for integrating R in a HPC envirnment that works with Slurm, `sluRm` has the following advantages:

1.  Compared to `rslurm`, this R package is more lightweight as there are no dependencies other than R packages that are either recommended or developed by the R Core Team.
    
    In the same line, the syntax is closer to what users may expect when calling any function from the parallel R package.
    
2.  In the case of the R package future, and more especifically, the R package batch-tools. This `sluRm` is more flexible while at the same time tailored to be used with Slurm. The package provides a user API to build wrappers to use with Slurm.



# Funding and Support

This work is supported by the National Cancer Institute (NCI), Award Number 5P01CA196569.

# References
