JOB_STATE_CODES <- c(
  "BF", "BOOT_FAIL", "Job terminated due to launch failure, typically due to a hardware failure (e.g. unable to boot the node or block and the job can not be requeued).",
  "CA", "CANCELLED", "Job was explicitly cancelled by the user or system administrator. The job may or may not have been initiated.",
  "CD", "COMPLETED", "Job has terminated all processes on all nodes with an exit code of zero.",
  "CF", "CONFIGURING", "Job has been allocated resources, but are waiting for them to become ready for use (e.g. booting).",
  "CG", "COMPLETING", "Job is in the process of completing. Some processes on some nodes may still be active.",
  "DL", "DEADLINE", "Job terminated on deadline.",
  "F", "FAILED", "Job terminated with non-zero exit code or other failure condition.",
  "NF", "NODE_FAIL", "Job terminated due to failure of one or more allocated nodes.",
  "OOM", "OUT_OF_MEMORY", "Job experienced out of memory error.",
  "PD", "PENDING", "Job is awaiting resource allocation.",
  "PR", "PREEMPTED", "Job terminated due to preemption.",
  "R", "RUNNING", "Job currently has an allocation.",
  "RD", "RESV_DEL_HOLD", "Job is held.",
  "RF", "REQUEUE_FED", "Job is being requeued by a federation.",
  "RH", "REQUEUE_HOLD", "Held job is being requeued.",
  "RQ", "REQUEUED", "Completing job is being requeued.",
  "RS", "RESIZING", "Job is about to change size.",
  "RV", "REVOKED", "Sibling was removed from cluster due to other cluster starting the job.",
  "SI", "SIGNALING", "Job is being signaled.",
  "SE", "SPECIAL_EXIT", "The job was requeued in a special state. This state can be set by users, typically in EpilogSlurmctld, if the job has terminated with a particular exit value.",
  "SO", "STAGE_OUT", "Job is staging out files.",
  "ST", "STOPPED", "Job has an allocation, but execution has been stopped with SIGSTOP signal. CPUS have been retained by this job.",
  "S", "SUSPENDED", "Job has an allocation, but execution has been suspended and CPUs have been released for other jobs.",
  "TO", "TIMEOUT", "Job terminated upon reaching its time limit."
)

JOB_STATE_CODES <- matrix(
  JOB_STATE_CODES, ncol = 3, byrow = TRUE,
  dimnames = list(NULL, c("code", "name", "description"))
  )


JOB_STATE_CODES <- data.frame(JOB_STATE_CODES, stringsAsFactors = FALSE)

JOB_STATE_CODES$type <- ifelse(
  JOB_STATE_CODES$code == "CD", "done",
  ifelse(grepl("fail|term|err|stopped|suspended|cancell", JOB_STATE_CODES$description, ignore.case = TRUE),
         "failed", "pending"
  ))

usethis::use_data(JOB_STATE_CODES, overwrite = TRUE)
