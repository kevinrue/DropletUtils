## Install BiocJobs, then this package from the local checkout together with
## everything its DESCRIPTION depends on, Suggests included.

path <- commandArgs(trailingOnly = TRUE)[1L]
options(repos = BiocManager::repositories(), Ncpus = parallel::detectCores())

install.packages("remotes")
## The archive download is not subject to the unauthenticated GitHub API rate
## limit that install_github() runs into on shared CI runners.
remotes::install_url("https://github.com/almahmoud/BiocJobs/archive/HEAD.tar.gz",
                     upgrade = "never")
remotes::install_local(path, dependencies = TRUE, upgrade = "never")

package <- read.dcf(file.path(path, "DESCRIPTION"))[1L, "Package"]
for (package_name in c(remotes::local_package_deps(path, dependencies = TRUE),
                       package, "BiocJobs")) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
        cat(paste("Error: Package", package_name, "failed to install successfully.\n"))
        quit(status = 1)
    }
}
