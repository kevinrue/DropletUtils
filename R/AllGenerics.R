#' @exportMethod downsampleCounts
#' @import methods
#' @importFrom Matrix colSums
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importMethodsFrom SummarizedExperiment assay assay<-
setGeneric(
    "downsampleCounts",
    signature = c("x", "target"),
    function(x, target, assay_name="counts") standardGeneric("downsampleCounts")
)
