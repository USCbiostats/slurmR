find: <DOCKER_PATH> image ls uscbiostats/slurmr:interactive
pull: <DOCKER_PATH> pull uscbiostats/slurmr:interactive
start: <DOCKER_PATH> run --name="slurmR-docker" --detach --publish 10022:22 uscbiostats/slurmr:interactive
login: ssh -i <ID_RSA> xenon@localhost -p 10022
