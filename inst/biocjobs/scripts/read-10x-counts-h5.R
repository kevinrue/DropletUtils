## BiocJobs job script: read-10x-counts-h5
##
## The declared interface lives in ../read-10x-counts-h5.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

## Test command (R)
## BiocJobs::runJob(BiocJobs::readJob("inst/biocjobs/read-10x-counts-h5.yaml"), params = list(hdf5_file = "test-data/matrix", sample_name = "sample_name", outfile = "sce-h5.h5ad"))

## Validation command (Bash)
## Rscript -e 'BiocJobs::biocjobsCLI()' validate .

# Rscript -e 'BiocJobs::biocjobsCLI()' galaxy   . read-10x-counts-h5 --out wrappers/read-10x-counts-h5.xml
# Rscript -e 'BiocJobs::biocjobsCLI()' nextflow . read-10x-counts-h5 --out wrappers/read-10x-counts-h5.nf

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts-h5")

suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(anndataR))

## ---- inputs -------------------------------------------------------------

### process test inputs

# sanity check: input files exist
stopifnot(file.exists(params$hdf5_file))
dropletutils_read10x_input_samples <- params$hdf5_file

## ---- task ---------------------------------------------------------------

sce <- DropletUtils::read10xCounts(
  samples = dropletutils_read10x_input_samples,
  sample.names = params$sample_name,
  type = "hdf5",
  col.names = TRUE
)

# Convert from TENxMatrix to avoid a row-column bug during anndataR::write_h5ad()
counts(sce) <- as(counts(sce), "dgCMatrix")

## ---- outputs ------------------------------------------------------------

# anndataR::write_h5ad() cannot overwrite an existing output file
# [Galaxy]: this is necessary because Galaxy creates an empty output file during tests
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
