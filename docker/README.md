To build the image:

```bash
docker build -t uscbiostats/slurmr-dev:latest .
```

To start the thing:

```bash
docker run --detach --publish 10022:22 uscbiostats/slurmr-dev
```

To login

```bash
ssh -p 10022 xenon@localhost
```

The password is `javagat`

To stop

```bash
docker stop xenodochial_kirch
```

