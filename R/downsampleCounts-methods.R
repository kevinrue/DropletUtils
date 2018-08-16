#' @rdname downsampleCounts-methods
#' @aliases downsampleCounts,SummarizedExperiment,NULL-method
#'
#' @importFrom Matrix colSums
#' @importFrom SummarizedExperiment assay
setMethod("downsampleCounts", c("SummarizedExperiment", "missing"),
          function(x, target=NULL, assay_name="counts")
          {
              # Find the minimum library size
              libsize <- as.integer(min(Matrix::colSums(assay(x, assay_name))))
              # Call the integer method
              downsampleCounts(x, libsize, assay_name)
          })

#' @rdname downsampleCounts-methods
#' @aliases downsampleCounts,SummarizedExperiment,character-method
#'
#' @importFrom SummarizedExperiment assay
setMethod("downsampleCounts", c("SummarizedExperiment", "character"),
          function(x, target=NULL, assay_name="counts")
          {
              # Only one sample can be given by name
              stopifnot(identical(length(target), 1L))
              
              # Compute the sample library size
              libsize <- as.integer(sum(assay(x, assay_name)[, target]))
              downsampleCounts(x, libsize, assay_name)
          })

#' @rdname downsampleCounts-methods
#' @aliases downsampleCounts,SummarizedExperiment,integer-method
#'
#' @importFrom Matrix colSums
setMethod("downsampleCounts", c("SummarizedExperiment", "integer"),
          function(x, target, assay_name="counts")
          {
              # Compute the downsampling ratios
              ratios <- target / Matrix::colSums(assay(x, assay_name))
              
              # Call the internal function
              .downsampleCounts(x, ratios, assay_name)
          })

#' @rdname downsampleCounts-methods
#' @aliases downsampleCounts,SummarizedExperiment,numeric-method
setMethod("downsampleCounts", c("SummarizedExperiment", "numeric"),
          function(x, target, assay_name="counts")
          {
              .downsampleCounts(x, target, assay_name)
          })

#' Downsample counts (INTERNAL)
#'
#' @param x A \code{\link{SummarizedExperiment}} object.
#' @param ratios A numeric vector of downsampling ratios, recycled as necessary.
#' @param assay_name The name of an assay in \code{assayNames(x)} that contains count data.
#'
#' @rdname INTERNAL_downsampleCounts
#'
#' @return The input \code{SummarizedExperiment} with the \code{assay_name} assay substituted by downsampled count data.
#'
#' @seealso \code{\link{downsampleCounts}}
.downsampleCounts <- function(x, ratios, assay_name="counts") {
    
    # Warn and cap ratios if greater than 1
    if (any(ratios > 1)) {
        warning(sprintf(
            "%i downsampling ratios were greater than 1 and capped",
            sum(ratios > 1)))
        ratios <- vapply(ratios, "min", numeric(1), 1)
    }
    
    new_counts <- downsampleMatrix(assay(x, assay_name), ratios)
    assay(x, assay_name) <- new_counts
    x
}
