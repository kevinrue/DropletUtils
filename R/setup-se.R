
# Make a SummarizedExperiment of 10 genes, 20 samples
nGenes <- 10; nSamples <- 20
counts <- matrix(rnbinom(nGenes*nSamples, 10, 0.1), ncol=nSamples)
# Simulate two clusters, A and B, defined in colData
colData <- DataFrame(
    class1=factor(rep(c("A", "B"), nSamples/2)),
    class2=factor(rep(c("C", "D"), nSamples/2)),
    replicate=factor(rep("0", nSamples)))
# Set row and column names
sampleNames <- LETTERS[seq_len(nSamples)]
featureNames <- letters[seq_len(nGenes)]
colnames(counts) <- rownames(colData) <- sampleNames
rownames(counts) <- featureNames
se <- SummarizedExperiment(assays = list(counts=counts), colData=colData)

# Split in 2 data sets of 10 samples each
se1 <- se[, seq_len(nSamples/2)]
se2 <- se[, (nSamples/2) + seq_len(nSamples/2)]

