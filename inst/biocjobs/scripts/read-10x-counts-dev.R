## BiocJobs job script: read-10x-counts
##
## The declared interface lives in ../read-10x-counts.yaml.
## jobParams() parses the command line against that declaration, so by the
## time it returns, every value below is typed, validated and defaulted.

params <- BiocJobs::jobParams("DropletUtils", "read-10x-counts")

suppressPackageStartupMessages(library(DropletUtils))
suppressPackageStartupMessages(library(LoomExperiment))

## ---- inputs -------------------------------------------------------------

tmpdir <- tempdir()

### test data

library(Matrix)
my.counts <- abs(rsparsematrix(100, 10, 0.2) * 10)
cell.ids <- paste0("BARCODE-", seq_len(ncol(my.counts)))

ngenes <- nrow(my.counts)
gene.ids <- paste0("ENSG0000", seq_len(ngenes))
gene.symb <- paste0("GENE", seq_len(ngenes))

test_matrix_dir <- file.path(tmpdir, "test_matrix")

DropletUtils::write10xCounts(path = test_matrix_dir, x = my.counts, gene.id=gene.ids, 
    gene.symbol=gene.symb, barcodes=cell.ids, type = "mtx")

### process test inputs

params <- list(
    mtx_file = file.path(tmpdir, "test_matrix", "matrix.mtx"),
    barcodes_file = file.path(tmpdir, "test_matrix", "barcodes.tsv"),
    genes_file = file.path(tmpdir, "test_matrix", "genes.tsv"),
    sample_name = "sample_name",
    outfile = file.path(tmpdir, "scle.loom")
)

dropletutils_input_dir <- file.path(tmpdir, "tenx_input_dir")
dir.create(dropletutils_input_dir)

file.symlink(from = params$mtx_file, to = file.path(dropletutils_input_dir))
file.symlink(from = params$barcodes_file, to = file.path(dropletutils_input_dir))
file.symlink(from = params$genes_file, to = file.path(dropletutils_input_dir))

## ---- task ---------------------------------------------------------------

sce <- DropletUtils::read10xCounts(samples = dropletutils_input_dir, sample.names = params$sample_name, type = "mtx")

## ---- outputs ------------------------------------------------------------

scle <- as(sce, "SingleCellLoomExperiment")
export(scle, params$outfile)

## Provenance to the job log.
message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
