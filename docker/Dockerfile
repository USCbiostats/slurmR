FROM xenonmiddleware/slurm

RUN apt-get update && \
	apt-get install -y --no-install-recommends \
	software-properties-common dirmngr && \
	apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
	add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && apt-get update && \
	apt-get install -y --no-install-recommends r-base
	
COPY slurmR*.tar.gz /rpkg/slurmR.tar.gz
RUN R CMD INSTALL /rpkg/slurmR.tar.gz
