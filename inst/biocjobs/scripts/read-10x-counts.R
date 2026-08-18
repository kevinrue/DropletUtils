## BiocJobs job script: read-10x-counts
##
## The declared interface lives in ../read-10x-counts.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts")

suppressPackageStartupMessages(library(DropletUtils))

## ---- inputs -------------------------------------------------------------



## ---- model --------------------------------------------------------------



## ---- outputs ------------------------------------------------------------

## Provenance to the job log.
message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
