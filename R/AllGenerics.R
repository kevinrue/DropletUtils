# downsampleCounts ----

#' Downsample counts in a SummarizedExperiment object
#'
#' @param x A \code{\link{SummarizedExperiment}}.
#' @param target The target \code{colSums} value (i.e., library size):
#' an \code{integer} value (recycled as necessary);
#' a \code{numeric} value of ratio in the range 0-1 (recycled as necessary);
#' a \code{character} sample name that sets the target library size (integer) value;
#' \code{NULL} to automatically downsample all samples to the smallest library size.
#' @param assay_name The name of an assay in \code{assayNames(x)} that contains count data.
#'
#' @details
#' For \code{integer} and \code{character} targets, all samples that have an initial library size smaller than the target will not be downsampled.
#'
#' @return The input \code{SummarizedExperiment} with the \code{assay_name} assay substituted by downsampled count data.
#'
#' @exportMethod downsampleCounts
#' @import methods
#' @importFrom Matrix colSums
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importMethodsFrom SummarizedExperiment assay assay<-
#' 
#' @author Kevin Rue-Albrecht
#'
#' @name downsampleCounts
#' @rdname downsampleCounts-methods
#'
#' @examples
#' # Setup ----
#'
#' # Create an object of 10 genes in 10 samples
#' nGenes <- 10; nSamples <- 10
#' sampleNames <- LETTERS[seq_len(nSamples)]
#' featureNames <- letters[seq_len(nGenes)]
#' counts <- matrix(rnbinom(nGenes*nSamples, 10, 0.1), ncol=nSamples)
#' colData <- DataFrame(cluster=factor(rep(c("A", "B"), each=nSamples/2)))
#' colnames(counts) <- rownames(colData) <- sampleNames
#' rownames(counts) <- featureNames
#' se <- SummarizedExperiment(assays = list(counts=counts), colData=colData)
#'
#' # Examples ----
#'
#' Matrix::colSums(assay(downsampleCounts(se)))
#' Matrix::colSums(assay(downsampleCounts(se, 10L)))
#' Matrix::colSums(assay(downsampleCounts(se, "B")))
#' Matrix::colSums(assay(downsampleCounts(se, 0.5))) / Matrix::colSums(assay(se))
#'
#' # Integer library size targets may be given per-sample
#' # e.g. half the samples downsampled to 10 counts, half to 20 counts
#' integerTargets <- c(rep(10L, nSamples/2), rep(20L, nSamples/2))
#' Matrix::colSums(assay(downsampleCounts(se, integerTargets)))
#'
#' # Numeric downsampling ratios may be given per-sample
#' # Half the samples downsampled to 50% of original counts, half to 75%
#' numericTargets <- c(rep(0.5, nSamples/2), rep(0.75, nSamples/2))
#' Matrix::colSums(assay(downsampleCounts(se, numericTargets))) / Matrix::colSums(assay(se))
setGeneric(
    "downsampleCounts",
    signature = c("x", "target"),
    function(x, target, assay_name="counts") standardGeneric("downsampleCounts")
)
