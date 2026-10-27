# R 4.6.1 as in renv.lock, packages restored from the lockfile.
# the project is mounted at run time, so outputs (data/, figures/, the supplement)
# land in the project folder:
#   docker build -t osf-licenses .
#   docker run --rm -v "$(pwd):/project" osf-licenses
FROM rocker/r-ver:4.6.1

# make for the Makefile, freetype and libpng for showtext (fonts in the figure),
# poppler and libjpeg for pdftools and qpdf (readmes in 01).
# the package binaries link against system libraries: libuv for fs, libsecret
# for keyring. curl is what renv uses for downloads
RUN apt-get update \
    && apt-get install -y --no-install-recommends make curl libfreetype-dev libpng-dev \
       libpoppler-cpp-dev libjpeg-dev zlib1g-dev libuv1-dev libsecret-1-dev \
    && rm -rf /var/lib/apt/lists/*

# quarto for the supplement, same version as used locally. comes with pandoc
RUN /rocker_scripts/install_quarto.sh 1.7.34

# library outside /project, so mounting the project does not hide it.
# no renv cache, the packages are copied into the library directly
ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_CONFIG_CACHE_ENABLED=FALSE

WORKDIR /project
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json renv/
RUN R -q -e "renv::restore(prompt = FALSE)"

CMD ["make"]
