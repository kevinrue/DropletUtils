## BiocJobs job script: read-10x-counts
##
## The declared interface lives in ../read-10x-counts.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

## Test command (R)
## BiocJobs::runJob(BiocJobs::readJob("inst/biocjobs/read-10x-counts.yaml"), params = list(mtx_file = "test-data/matrix.mtx", barcodes_file = "test-data/barcodes.tsv", genes_file = "test-data/genes.tsv", sample_name = "sample_name"))

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts")

suppressPackageStartupMessages(library(BiocIO))
suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(LoomExperiment))

## ---- inputs -------------------------------------------------------------

tmpdir <- tempdir()

### process test inputs

stopifnot(file.exists(params$mtx_file))
stopifnot(file.exists(params$barcodes_file))
stopifnot(file.exists(params$genes_file))
# any check on params$sample_name? character vector of length 1?

dropletutils_input_dir <- file.path(tmpdir, "tenx_input_dir")
dir.create(dropletutils_input_dir)
stopifnot(dir.exists(dropletutils_input_dir))
invisible(file.symlink(from = params$mtx_file, to = file.path(dropletutils_input_dir, "matrix.mtx")))
invisible(file.symlink(from = params$barcodes_file, to = file.path(dropletutils_input_dir, "barcodes.tsv")))
invisible(file.symlink(from = params$genes_file, to = file.path(dropletutils_input_dir, "genes.tsv")))

## ---- task ---------------------------------------------------------------

sce <- DropletUtils::read10xCounts(
  samples = dropletutils_input_dir,
  sample.names = params$sample_name,
  type = "mtx"
)

## ---- outputs ------------------------------------------------------------

scle <- as(sce, "SingleCellLoomExperiment")
BiocIO::export(scle, params$outfile)

## Provenance to the job log.
message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
