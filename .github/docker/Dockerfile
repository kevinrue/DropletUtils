# syntax=docker/dockerfile:1

ARG BIOCONDUCTOR_TAG=devel
FROM ghcr.io/bioconductor/r-ver:${BIOCONDUCTOR_TAG}

# The checkout is bind-mounted rather than copied, so none of the source ends
# up in an image layer.
RUN --mount=type=bind,target=/tmp/DropletUtils,rw \
    Rscript /tmp/DropletUtils/.github/docker/install.R /tmp/DropletUtils
