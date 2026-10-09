## BiocJobs job script: read-10x-counts-mtx
##
## The declared interface lives in ../read-10x-counts-mtx.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

## Test command (R)
## BiocJobs::runJob(BiocJobs::readJob("inst/biocjobs/read-10x-counts-mtx.yaml"), params = list(mtx_file = "test-data/matrix.mtx.gz", barcodes_file = "test-data/barcodes.tsv.gz", features_file = "test-data/features.tsv.gz", sample_name = "sample_name", outfile = "sce-mtx.h5ad"))

## Validation command (Bash)
## Rscript -e 'BiocJobs::biocjobsCLI()' validate .

# Rscript -e 'BiocJobs::biocjobsCLI()' galaxy   . read-10x-counts-mtx --out wrappers/read-10x-counts-mtx.xml
# Rscript -e 'BiocJobs::biocjobsCLI()' nextflow . read-10x-counts-mtx --out wrappers/read-10x-counts-mtx.nf

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts-mtx")

suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(anndataR))

## ---- inputs -------------------------------------------------------------

### process test inputs

# sanity check: input files exist
stopifnot(file.exists(params$mtx_file))
stopifnot(file.exists(params$barcodes_file))
stopifnot(file.exists(params$features_file))

# create symlinks to all input files in the same directory with the expected names
# [Galaxy]: this is necessary for test files that are all named with extension '*.dat'
dropletutils_read10x_input_samples <- "tenx_input_dir"
dir.create(dropletutils_read10x_input_samples)
stopifnot(dir.exists(dropletutils_read10x_input_samples))
invisible(file.symlink(
  from = normalizePath(params$mtx_file),
  to = file.path(dropletutils_read10x_input_samples, "matrix.mtx.gz"))
)
invisible(file.symlink(
  from = normalizePath(params$barcodes_file),
  to = file.path(dropletutils_read10x_input_samples, "barcodes.tsv.gz"))
)
invisible(file.symlink(
  from = normalizePath(params$features_file),
  to = file.path(dropletutils_read10x_input_samples, "features.tsv.gz"))
)

## ---- task ---------------------------------------------------------------

sce <- DropletUtils::read10xCounts(
  samples = dropletutils_read10x_input_samples,
  sample.names = params$sample_name,
  type = "mtx",
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
