#------------------------------------------------------------------------------#
# Sample papers for a manual check of the license coding
#
# Draws a stratified sample of papers from data/license_by_paper.csv (from 02)
# and writes a blank coding sheet, data/manual_check.csv.
# The sheet does not show our coding so it cannot influence the manual rating;
# the strata are in a separate file, # data/manual_check_strata.csv. 
#------------------------------------------------------------------------------#


# setup -------------------------------------------------------------------
library(dplyr)

SHEET <- "data/manual_check.csv"
STRATA <- "data/manual_check_strata.csv"
if (file.exists(SHEET) || file.exists(STRATA)) {
  stop("manual check files exist already, delete them to draw a new sample")
}

papers <- read.csv("data/license_by_paper.csv")
objects <- read.csv("data/osf_licenses.csv")


# strata ------------------------------------------------------------------
# special cases of the crawler: links to registrations and to single files
special_links <- objects |>
  filter(holder_type %in% "registrations" | obj_type %in% "files") |>
  distinct(doi)

# case_when takes the first match, so the strata do not overlap
papers <- papers |>
  mutate(stratum = case_when(
    licensed & !osf_license ~ "child, file or readme only",
    !licensed & chose_no_lic ~ "no license chosen",
    doi %in% special_links$doi ~ "registration or file link",
    licensed ~ "licensed on OSF",
    .default = "unlicensed"
  ))

# papers per stratum. 10% of all papers, as in Quintana's check, with the
# tricky strata oversampled: all "child, file or readme only" papers (the
# cases the crawler gets wrong most easily), all with "no license" chosen
# and 15% of the registration or file links
n_total <- round(0.1 * nrow(papers))
stratum_n <- table(papers$stratum)
sample_size <- c(
  "child, file or readme only" = stratum_n[["child, file or readme only"]],
  "no license chosen" = stratum_n[["no license chosen"]],
  "registration or file link" = round(0.15 * stratum_n[["registration or file link"]])
)

# the rest goes to the two plain strata, in proportion to their size
n_rest <- n_total - sum(sample_size)
plain_n <- stratum_n[c("licensed on OSF", "unlicensed")]
sample_size["licensed on OSF"] <- round(n_rest * plain_n[["licensed on OSF"]] / sum(plain_n))
sample_size["unlicensed"] <- n_total - sum(sample_size)
print(sample_size)


# sample ------------------------------------------------------------------
set.seed(35037321)
sampled <- papers |>
  mutate(size = sample_size[stratum]) |>
  group_by(stratum) |>
  slice_sample(prop = 1) |>
  filter(row_number() <= size) |>
  ungroup() |>
  # random order, so the strata cannot be guessed from the position
  slice_sample(prop = 1) |>
  mutate(id = row_number())


# coding sheet ------------------------------------------------------------
links <- objects |>
  group_by(doi) |>
  summarise(osf_links = paste0("https://osf.io/", osf_project, collapse = "; "))

# left join, every paper in the sample has at least one link
sheet <- sampled |>
  left_join(links, by = "doi") |>
  mutate(id = id, doi = doi,
            paper = paste0("https://doi.org/", doi),
            osf_links = osf_links,
            license_osf = "",
            license_osf_name = "",
            license_other = "",
            license_other_where = "",
            license_other_name = "",
            checked_on = "",
            notes = "", 
            .keep = "none")

write.csv(sheet, SHEET, row.names = FALSE)
write.csv(select(sampled, id, doi, stratum), STRATA, row.names = FALSE)