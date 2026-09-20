## BiocJobs job script: read-10x-counts
##
## The declared interface lives in ../read-10x-counts.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

## Test command (R)
## BiocJobs::runJob(BiocJobs::readJob("inst/biocjobs/read-10x-counts.yaml"), params = list(mtx_file = "test-data/matrix.mtx", barcodes_file = "test-data/barcodes.tsv", genes_file = "test-data/genes.tsv", sample_name = "sample_name", type = "mtx", outfile = "scle.loom"))

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts")

suppressPackageStartupMessages(library(BiocIO))
suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(LoomExperiment))

## ---- inputs -------------------------------------------------------------

### process test inputs

if (identical(params$type, "mtx")) {
  # check input files exist
  stopifnot(file.exists(params$mtx_file))
  stopifnot(file.exists(params$barcodes_file))
  stopifnot(file.exists(params$genes_file))
  # make sure they are all stored in the same directory with the expected names
  dropletutils_read10x_input_samples <- "tenx_input_dir"
  dir.create(dropletutils_read10x_input_samples)
  stopifnot(dir.exists(dropletutils_read10x_input_samples))
  invisible(file.symlink(from = params$mtx_file, to = file.path(dropletutils_read10x_input_samples, "matrix.mtx")))
  invisible(file.symlink(from = params$barcodes_file, to = file.path(dropletutils_read10x_input_samples, "barcodes.tsv")))
  invisible(file.symlink(from = params$genes_file, to = file.path(dropletutils_read10x_input_samples, "features.tsv")))
} else if (identical(params$type, "hdf5")) {
  # check input files exist
  stopifnot(file.exists(params$hdf5_file))
  dropletutils_read10x_input_samples <- params$hdf5_file
}

## ---- task ---------------------------------------------------------------

sce <- DropletUtils::read10xCounts(
  samples = dropletutils_read10x_input_samples,
  sample.names = params$sample_name,
  type = params$type,
  col.names = TRUE
)

## ---- outputs ------------------------------------------------------------

scle <- as(sce, "SingleCellLoomExperiment")

if (file.exists(params$outfile)) {
  file.remove(params$outfile)
}
BiocIO::export(object = scle, con = params$outfile, format = "loom")

# optional: remove dropletutils_input_dir (workflow manager should remove job working directory)

## Provenance to the job log.
message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
