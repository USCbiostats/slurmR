#' slurmR docker image
#' 
#' Helper functions to use `slurmR`'s docker image. This requires
#' having an internet connection and `docker` installed in your
#' system.
#'
#' @details
#' Starting version 0.5-0, a Docker image with Slurm, R, and slurmR
#' is available at \url{https://hub.docker.com/r/uscbiostats/slurmr}.
#' The source code (Dockerfile) is available in the project GitHub
#' repository: \url{https://github.com/USCbiostats/slurmR}.
#' 
#' @examples 
#' # This example requires having Docker installed in the system
#' \dontrun{
#'   # Start the docker image. By default it will try to pull the
#'   # image from Docker Hub if available
#' 
#'   # This opens a bash session with R + Slurm + slurmR
#'   slurmr_docker_run() 
#' 
#'   # Will pull the docker image
#'   slurmr_docker_pull()
#' 
#' 
#' }
#'
#' @name slurmr_docker
NULL

slurmr_docker_cmds <- list(
    image_ls = "image ls uscbiostats/slurmr:interactive",
    container_ls = "container ls",
    pull     = "pull uscbiostats/slurmr:interactive",
    run      = 'run --detach --publish 10022:22 uscbiostats/slurmr:interactive',
    ssh      = "-i \"<ID_RSA>\" xenon@localhost -p 10022"
)

slurmr_docker <- new.env()
slurmr_docker$set_UUID <- function(UUID) {

  # OK, so there is a UUID set, is it alive?
  if (exists("UUID", envir = slurmr_docker)) {

    # Checking that docker is available!
    cmd <- slurmr_docker_available()
    if (!cmd)
      stop("docker is not available in this system.", call. = FALSE)


    # Checking if it is running
    cmd <- attr(cmd, "path")
    res <- silent_system2(
      path,
      paste0("container ls -f ID=", slurmr_docker$UUID),
      stderr = TRUE, stdout = TRUE
    )

    res <- grepl(slurmr_docker$UUID, paste(res, collapse="\n"), fixed = TRUE)

    if (!res)
      warning(
        "The container with UUID ",
        slurmr_docker$UUID,
        " was initiated in this session, but it is not running."
        )

  }

  slurmr_docker$UUID <- UUID

}

slurmr_docker$unset_UUID <- function() {
  if (exists("UUID", envir = slurmr_docker))
    rm(UUID, envir = slurmr_docker)
}

slurmr_docker$get_UUID <- function() {

  if (exists("UUID", envir = slurmr_docker))
    return(slurmr_docker$slurmr_docker)
  else
    stop(
      "No docker image to be stopped.",
      call. = FALSE
      )

}

#' @export
#' @rdname slurmr_docker
docker_available <- function(path = "") {

  if (.Platform$OS.type != "unix")
    return(FALSE)

  # It is available if R can find it!
  if (path == "") {

    x <- Sys.which("docker")
    return(structure(nchar(x) > 0L, path = x))

  } else {

    if (!file.exists(path))
      stop("The file ", path, " cannot be found.", call. = FALSE)

    return(structure(TRUE, path = path))

  }

}

slurmr_docker_available <- function(path = "") {

  # Checking that docker is installed
  if (path == "") {

    path <- docker_available()
    if (!path)
      stop("Docker seems not to be available in the system")

    path <- attr(path, "path")

  }

  # Checking if we can find it
  res <- system2(
      path, slurmr_docker_cmds$image_ls,
      stdout = TRUE, stderr = TRUE
    )

  if (length(res) == 1L)
    return(FALSE)
  else
    return(TRUE)

}

#' @export
#' @param path Path to the `docker` executable. If not specified, the function
#' will try to figure it out by itself.
#' @rdname slurmr_docker
slurmr_docker_pull <- function(path = "") {

  res <- slurmr_docker_available(path)

  if (!res)
    system2(path, slurmr_docker_cmds$pull)
  else
    message("The image is alraedy available.")

  invisible()

}

#' @export 
#' @param pull Logical scalar. When `TRUE`, if not available, it will
#' invoke `docker pull`.
#' @param timeout Integer. Number of seconds to wait for docker to start
#' the slurmR image.
#' @rdname slurmr_docker
#' 
slurmr_docker_run <- function(path = "", pull = TRUE, timeout = 60) {

  # Checking if the image is available
  pulled <- slurmr_docker_available(path)
  if (!pulled & pull)
    slurmr_docker_pull(path)
  else if (!pulled & !pull)
    stop(
      "The image uscbiostats/slurmr:interactive is not available.",
      " Try using `pull = TRUE`.", call. = FALSE
      )

  cmd  <- attr(docker_available(), "path")
  UUID <- silent_system2(
    cmd, slurmr_docker_cmds$run,
    stdout = TRUE, stderr = TRUE
  )

  on.exit({
    tryCatch({
      slurmr_docker_stop(UUID = UUID, path = cmd)
    }, error = function(e) e)

    slurmr_docker$unset_UUID()
  })

  slurmr_docker$set_UUID(UUID)

  # Finding the file, copying it, and setting the mode to 600
  # so we can use it for ssh
  id_rsa <- tempfile(pattern = "id_rsa")
  file.copy(
    from = system.file("docker/id_rsa", package = "slurmR"),
    to   = id_rsa
    )

  Sys.chmod(id_rsa, mode = "600")

  message("Starting the session...")
  args  <- gsub("<ID_RSA>", id_rsa, slurmr_docker_cmds$ssh, fixed = TRUE)
  t0    <- Sys.time()
  tdiff <- 0
  while (tdiff < timeout) {

    res <- silent_system2("ssh", args)
    if (res == 0)
      break

    tdiff <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    message("Trying to connect again (timeout in ~", floor(timeout - tdiff), " seconds).")
    Sys.sleep(1)

  }

  invisible()

}

#' @export
#' @rdname slurmr_docker
#' @param UUID String. Universally Unique Identifier.
slurmr_docker_stop <- function(UUID = "", path = "") {

  # Retrieving the UUID
  if (UUID == "")
    UUID <- slurmr_docker$get_UUID()

  if (!is.null(UUID) && UUID != "") {

    path <- docker_available(path)
    if (!path)
      stop("Docker seems not to be available in the system")

    path <- attr(path, "path")
    message("Stopping the image ", UUID, "...")
    system2(path, paste("stop", UUID))

    invisible()

  }

  invisible()

}
