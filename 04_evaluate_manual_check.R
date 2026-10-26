#------------------------------------------------------------------------------#
# Compare the manual license check with the crawled licenses
#
# Joins the filled coding sheet (data/manual_check.csv, from 03) to our coding in
# data/license_by_paper.csv. Checks two things:
# 1. Does the crawler read the OSF license field correctly (license_osf)?
# 2. How often is a license stated somewhere we do not look (license_other)?
#------------------------------------------------------------------------------#


# setup -------------------------------------------------------------------
library(dplyr)
library(psych)

# load data ---------------------------------------------------------------
manual <- read.csv("data/manual_check.csv")
strata <- read.csv("data/manual_check_strata.csv")
papers <- read.csv("data/license_by_paper.csv")

coded <- filter(manual, tolower(trimws(license_osf)) %in% c("yes", "no"))
message(nrow(coded), " of ", nrow(manual), " papers coded")
if (nrow(coded) == 0) stop("no papers coded yet")

# inner joins, both files cover all papers of the sample
check <- coded |>
  inner_join(strata, by = c("id", "doi")) |>
  inner_join(select(papers, doi, osf_license, child_license, license_file,
                    licensed, osf_license_types, license_types), by = "doi") |>
  mutate(manual_osf = tolower(trimws(license_osf)) == "yes",
         manual_other = tolower(trimws(license_other)) %in% "yes",
         manual_any = manual_osf | manual_other,
         # crawler reads the license field of the linked objects and
         # their components. the sheet asks only for direct components here,
         # deeper ones go to license_other. no paper is licensed only through
         # a deeper component, so this does not matter for the agreement
         ours_osf = osf_license | child_license)


# 1. OSF license field ----------------------------------------------------
print(table(manual = check$manual_osf, crawler = check$ours_osf))
cat("agreement:", round(100 * mean(check$manual_osf == check$ours_osf)), "%,",
    "kappa:", round(cohen.kappa(cbind(check$manual_osf, check$ours_osf))$kappa, 2), "\n")

# license names, to compare by eye
check |>
  filter(manual_osf | ours_osf) |>
  select(id, osf_license_types, license_osf_name) |>
  print()


# 2. licenses stated elsewhere --------------------------------------------
# the sample is stratified, so we look at this per stratum
# the share in "unlicensed" estimates how many unlicensed papers state a
# license somewhere we do not look
check |>
  group_by(stratum) |>
  summarise(n = n(), licensed_ours = sum(licensed), licensed_manual = sum(manual_any),
            other_only = sum(manual_other & !manual_osf)) |>
  print()

print(count(filter(check, manual_other), license_other_where, sort = TRUE))

# license names from all sources
check |>
  filter(manual_other) |>
  select(id, license_types, license_other_where, license_other_name) |>
  print()


# disagreements -----------------------------------------------------------
check |>
  filter(manual_osf != ours_osf | manual_any != licensed) |>
  select(id, doi, stratum, ours_osf, manual_osf, licensed, manual_any,
         license_other_where, notes) |>
  print()
