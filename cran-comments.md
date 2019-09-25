## Test environments

* local ubuntu, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

This R package has been tested on Linux systems, and particularly, HPC clusters
working with Slurm. It was tested on USC's HPCC center and others during a review
process of the Journal of Open Source Software. We don't include Slurm as part
of the SystemRequirements field since users without Slurm can still install this
package and use some of its functions.

## On the latest CRAN comments

1. 'Slurm' and 'HPC' single-quoted in title and description.

2. HPC acronym expanded

3. Added a link to Slurm in the description (and also as in the URL field)

4. Most of the examples should be run in a Slurm cluster, that's the reason why
   most of them have a \dontrun tag. This also implies that most examples cannot
   be run by `examples()`, so the \donttest flag is not a good replacement.

5. This is a complicated point. Using getwd() as a default is actually also used
   in another R package on CRAN (rslurm). Still, if that weren't the case, most
   users enjoy the "plug-and-play" way in which the package work (like myself),
   so forcing them to set other directory than getwd() would go against the package's
   philosophy.
   
   You could argue that a better choice would be tempdir() as a default, but in 
   the case of Slurm clusters that doesn't ensure the functions to work, since
   I have no way to know if all the nodes have access to the tmp directory where
   that folder is created, causing errors.
   
   Moreover, users can change that if needed, and when the package loads it warns
   the user that the current default `tmp_path` is getwd(), so they are adviced
   to change that.
   
   Finally, no file I/O is done until the user explicitly calls one of the functions
   that do such, e.g. Slurm_*apply functions.

6. No example using mc.cores uses more than 1 core. The only place where this may
   look like it is using more than 2 is in `makeSlurmCluster` which is wrapped
   with \dontrun since it requires Slurm to be executed. I won't change that since
   that is actually the whole point of that function, creating cluster objects with
   dozens of workers.
   
7. Again, almost none of the examples can be executed without Slurm. I cannot
   unwrap them from \dontrun, and I cannot create toy examples of those (actually
   most of them are toy examples to be run under a Slurm cluster).

8. There was one place in the code where I was writing to the console using cat,
   this has now been changed to message instead.
   
9. I've added \value when possible.

Thanks!
