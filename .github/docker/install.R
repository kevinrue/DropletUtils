## Install BiocJobs, then this package from the local checkout together with
## everything its DESCRIPTION depends on, Suggests included.

path <- commandArgs(trailingOnly = TRUE)[1L]
options(repos = BiocManager::repositories(), Ncpus = parallel::detectCores())

install.packages("remotes")
remotes::install_github("almahmoud/BiocJobs", upgrade = "never")
remotes::install_local(path, dependencies = TRUE, upgrade = "never")

package <- read.dcf(file.path(path, "DESCRIPTION"))[1L, "Package"]
for (package_name in c(remotes::local_package_deps(path, dependencies = TRUE),
                       package, "BiocJobs")) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
        cat(paste("Error: Package", package_name, "failed to install successfully.\n"))
        quit(status = 1)
    }
}
