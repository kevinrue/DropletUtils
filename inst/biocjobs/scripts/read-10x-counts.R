## BiocJobs job script: read-10x-counts
##
## The declared interface lives in ../read-10x-counts.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

## Test command (R)
## BiocJobs::runJob(BiocJobs::readJob("inst/biocjobs/read-10x-counts.yaml"), params = list(mtx_file = "test-data/matrix.mtx", barcodes_file = "test-data/barcodes.tsv", genes_file = "test-data/genes.tsv", sample_name = "sample_name", type = "mtx", outfile = "sce.h5ad"))

## Validation command (Bash)
## Rscript -e 'BiocJobs::biocjobsCLI()' validate .

# Rscript -e 'BiocJobs::biocjobsCLI()' galaxy   . read-10x-counts --out wrappers/read-10x-counts.xml
# Rscript -e 'BiocJobs::biocjobsCLI()' nextflow . read-10x-counts --out wrappers/read-10x-counts.nf

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts")

suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(anndataR))

## ---- inputs -------------------------------------------------------------

### process test inputs

if (identical(params$type, "mtx")) {
  # sanity check: input files exist
  stopifnot(file.exists(params$mtx_file))
  stopifnot(file.exists(params$barcodes_file))
  stopifnot(file.exists(params$genes_file))
  # create symlinks to all input files in the same directory with the expected names
  # [Galaxy]: this is necessary for testing, as test files are named '*.dat'
  dropletutils_read10x_input_samples <- "tenx_input_dir"
  dir.create(dropletutils_read10x_input_samples)
  stopifnot(dir.exists(dropletutils_read10x_input_samples))
  invisible(file.symlink(from = params$mtx_file, to = file.path(dropletutils_read10x_input_samples, "matrix.mtx")))
  invisible(file.symlink(from = params$barcodes_file, to = file.path(dropletutils_read10x_input_samples, "barcodes.tsv")))
  invisible(file.symlink(from = params$genes_file, to = file.path(dropletutils_read10x_input_samples, "features.tsv")))
} else if (identical(params$type, "hdf5")) {
  # sanity check: input files exist
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

if (file.exists(params$outfile)) {
  file.remove(params$outfile)
}

anndataR::write_h5ad(
  object = sce,
  compression = "gzip",
  path = params$outfile
)

## Provenance to the job log.
message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
