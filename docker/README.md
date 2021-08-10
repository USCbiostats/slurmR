# slurmR docker images

This repository features two different Docker images. The first, labeled
`interactive`, was designed to provide a sandbox for users to test the
`slurmR` package. The second image, `rcmdcheck`, was created for testing
only, and it is part of the Continuous Integration process of the project.

Both images are available at [DockerHub](https://hub.docker.com/r/uscbiostats/slurmr)
and were built on top of
[`xenonmiddleware/slurm`](https://hub.docker.com/r/xenonmiddleware/slurm).

For ease of use, you can run these images with the help of the `Makefile`
included here.

## Interactive mode

For interactive mode, type `make interactive` and follow the instructions.
The interactive mode requires using the `id_rsa.pub` file distributed here;
otherwise, you can personalize your images by tailoring the Dockerfiles included in the repository.
