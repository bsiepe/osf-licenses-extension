#------------------------------------------------------------------------------#
# Download original materials
#
# Downloads v1.1.0 of the original deposit from Zenodo, checks the md5 and
# unzips it to materials/v1.1.0/.
# We pin the version DOI (10.5281/zenodo.22805418). The DOI in the paper
# (10.5281/zenodo.22117775) is the concept DOI and always gives the latest version.
#
#------------------------------------------------------------------------------#


# settings ----------------------------------------------------------------
VERSION <- "v1.1.0"
RECORD <- "22805418"
ZIP_NAME <- "osf-materials-downloads-v1.1.0.zip"
ZIP_MD5 <- "992b44cfe7175b78de117105d89247e1"

# file key on zenodo contains a slash, needs to be %2F
ZIP_URL <- paste0("https://zenodo.org/api/records/", RECORD,
                  "/files/dsquintana%2F", ZIP_NAME, "/content")

MAT_ROOT <- "materials"
MAT_DIR <- file.path(MAT_ROOT, VERSION)
ZIP_FILE <- file.path(MAT_ROOT, ZIP_NAME)


# download ----------------------------------------------------------------
if (dir.exists(MAT_DIR)) {
  message(MAT_DIR, " already exists, delete it to download again")
} else {
  dir.create(MAT_ROOT, showWarnings = FALSE)
  if (!file.exists(ZIP_FILE)) {
    download.file(ZIP_URL, ZIP_FILE, mode = "wb")
  }

  # make sure we have the published file
  md5 <- unname(tools::md5sum(ZIP_FILE))
  if (md5 != ZIP_MD5) {
    stop("md5 of ", ZIP_FILE, " does not match, delete it and try again")
  }

  # zip has one folder named after the github commit, rename it to the version
  top_dir <- unique(sub("/.*", "", unzip(ZIP_FILE, list = TRUE)$Name))
  unzip(ZIP_FILE, exdir = MAT_ROOT)
  file.rename(file.path(MAT_ROOT, top_dir), MAT_DIR)
}
