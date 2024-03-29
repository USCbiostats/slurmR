% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSlurmCluster.R
\name{makeSlurmCluster}
\alias{makeSlurmCluster}
\alias{stopCluster.slurm_cluster}
\title{Create a Parallel Socket Cluster using Slurm}
\usage{
makeSlurmCluster(
  n,
  job_name = random_job_name(),
  tmp_path = opts_slurmR$get_tmp_path(),
  cluster_opt = list(),
  max_wait = 300L,
  verb = TRUE,
  ...
)

\method{stopCluster}{slurm_cluster}(cl)
}
\arguments{
\item{n}{Integer scalar. Size of the cluster object (see details).}

\item{job_name}{Character. Name of the job to be passed to \code{Slurm}.}

\item{tmp_path}{Character. Path to the directory where all the data (including
scripts) will be stored. Notice that this path must be accessible by all the
nodes in the network (See \link{opts_slurmR}).}

\item{cluster_opt}{A list of arguments passed to \link[parallel:makeCluster]{parallel::makePSOCKcluster}.}

\item{max_wait}{Integer scalar. Wait time before exiting with error while
trying to read the nodes information.}

\item{verb}{Logical scalar. If \code{TRUE}, the function will print messages on
screen reporting on the status of the job submission.}

\item{...}{Further arguments passed to \link{Slurm_EvalQ} via \code{sbatch_opt}.}

\item{cl}{An object of class \code{slurm_cluster}.}
}
\value{
A object of class \code{c("slurm_cluster", "SOCKcluster", "cluster")}. It
is the same as what is returned by \link[parallel:makeCluster]{parallel::makePSOCKcluster} with the main
difference that it has two extra attributes:
\itemize{
\item \code{SLURM_JOBID} Which is the id of the Job that initialized that cluster.
}
}
\description{
This function is essentially a wrapper of the function \link[parallel:makeCluster]{parallel::makePSOCKcluster}.
\code{makeSlurmCluster} main feature is adding node addresses.
}
\details{
By default, if the \code{time} option is not specified via \code{...},
then it is set to the value \code{01:00:00}, this is, 1 hour.

Once a job is submitted via Slurm, the user gets access to the nodes
associated with it, which allows users to star new processes within those.
By means of this, we can create Socket, also known as "PSOCK", clusters across
nodes in a Slurm environment. The name of the hosts are retrieved and passed
later on to \link[parallel:makeCluster]{parallel::makePSOCKcluster}.

It has been the case that R fails to create the cluster with the following
message in the Slurm log file:

\if{html}{\out{<div class="sourceCode">}}\preformatted{srun: fatal: SLURM_MEM_PER_CPU, SLURM_MEM_PER_GPU, and SLURM_MEM_PER_NODE are mutually exclusive
}\if{html}{\out{</div>}}

In such cases, setting the memory, for example, upfront can solve the problem.
For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{cl <- makeSlurmCluster(20, mem = 20)
}\if{html}{\out{</div>}}

If the problem persists, i.e., the cluster cannot be created, make sure that
your Slurm cluster allows Socket connections between nodes.

The method \code{stopCluster} for \code{slurm_cluster} stops the cluster doing
the following:
\enumerate{
\item Closes the connection by calling the \code{stopCluster} method for \code{PSOCK} objects.
\item Cancel the Slurm job using \code{scancel}.
}
}
\section{Maximum number of connections}{


By default, R limits the number of simultaneous connections (see this thread
in R-sig-hpc \url{https://stat.ethz.ch/pipermail/r-sig-hpc/2012-May/001373.html})
Current maximum is 128 (R version 3.6.1). To modify that limit, you would need
to reinstall R updating the macro \code{NCONNECTIONS} in the file \code{src/main/connections.c}.

For now, if the user sets \code{n} above 128 it will get an immediate warning
pointing to this issue, in particular, specifying that the cluster object
may not be able to be created.
}

\examples{
\dontrun{

# Creating a cluster with 100 workers/offpring/child R sessions
cl <- makeSlurmCluster(100)

# Computing the mean of a 100 random uniforms within each worker
# for this we can use any of the function available in the parallel package.
ans <- parSapply(1:200, function(x) mean(runif(100)))

# We simply call stopCluster as we would do with any other cluster
# object
stopCluster(ans)

# We can also specify SBATCH options directly (...)
cl <- makeSlurmCluster(200, partition = "thomas", time = "02:00:00")
stopCluster(cl)

}

}
