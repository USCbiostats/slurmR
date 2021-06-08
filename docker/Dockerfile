FROM xenonmiddleware/slurm

RUN apt-get update && \
	apt-get install -y --no-install-recommends \
	software-properties-common dirmngr && \
	apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
	add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && apt-get update && \
	apt-get install -y --no-install-recommends r-base
	
RUN add-apt-repository ppa:c2d4u.team/c2d4u4.0+ && \
	apt-get update && \
	apt-get install -y --no-install-recommends \
	r-cran-rmarkdown \
	r-cran-knitr \
	r-cran-covr \
	r-cran-tinytest

RUN mkdir /home/slurmr-test
ENV SLURMR_TEST_DIR=/home/slurmr-test

COPY slurmR*.tar.gz /rpkg/slurmR.tar.gz
RUN R CMD INSTALL /rpkg/slurmR.tar.gz

COPY id_rsa.pub .
RUN cat id_rsa.pub >> .ssh/authorized_keys

