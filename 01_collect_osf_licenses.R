#------------------------------------------------------------------------------#
# Collect licenses of linked OSF objects
# Project: license extension to Quintana, "Shared research materials in
# psychology are widely downloaded but often difficult to reuse"
# Author: Björn Siepe
#
# Reads the OSF license of every OSF object that a paper in the original deposit
# links (osf_file_downloads.csv) and writes data/osf_licenses.csv.
# Files have no license, so for a file link we use the project or registration
# holding the file. Direct child components can have their own license and are
# checked as well.
# Needs an OSF token, stored with keyring::key_set("OSF", "OSF_PAT").
# A full run takes about an hour. Results are cached in data/osf_license_cache.rds,
# so an interrupted run continues where it stopped; delete the cache to start over.
# For a test run, set e.g. N_TEST <- 20 before sourcing.
#------------------------------------------------------------------------------#


# setup -------------------------------------------------------------------
library(httr2)
library(dplyr)

MAT_DIR <- file.path("materials", "v1.1.0")
if (!dir.exists(MAT_DIR)) stop("run 00_get_materials.R first")

API <- "https://api.osf.io/v2/"
CACHE_FILE <- "data/osf_license_cache.rds"
OUT_FILE <- "data/osf_licenses.csv"
if (!exists("N_TEST")) N_TEST <- Inf

osf_pat <- keyring::key_get("OSF", "OSF_PAT")


# helpers -----------------------------------------------------------------
# rate limits and server errors are retried. if they persist we throw an error,
# and the object is tried again on the next run.
# other errors (private or deleted objects) are returned and recorded
osf_get <- function(url, ...) {
  request(url) |>
    req_url_query(...) |>
    req_auth_bearer_token(osf_pat) |>
    req_retry(max_tries = 5, retry_on_failure = TRUE,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
    req_error(is_error = \(resp) resp_status(resp) == 429 || resp_status(resp) >= 500) |>
    req_perform()
}

# needs a project or registration requested with embed = "license"
# NA if the authors never chose a license
license_name <- function(d) d$embeds$license$data$attributes$name %||% NA_character_

# "No license" is an explicit option on OSF and counts as unlicensed.
# "Other" means a custom license and counts as licensed
is_licensed <- function(name) !is.na(name) & name != "No license"

check_object <- function(id) {
  checked <- format(Sys.Date())

  # guids/ works for any type of OSF id
  g <- osf_get(paste0(API, "guids/", id, "/"))
  if (resp_status(g) >= 400) {
    return(tibble(osf_project = id, http_status = resp_status(g), checked = checked))
  }
  obj <- resp_body_json(g)$data

  # for a file, switch to the project or registration holding it
  # (same rule as Quintana's collect_osf_documentation.R)
  if (obj$type == "files") {
    holder <- obj$relationships$target$data %||% obj$relationships$node$data
  } else {
    holder <- obj
  }
  if (!holder$type %in% c("nodes", "registrations")) {
    return(tibble(osf_project = id, http_status = 200L, obj_type = obj$type, checked = checked))
  }

  node_url <- paste0(API, holder$type, "/", holder$id, "/")
  n <- osf_get(node_url, embed = "license")
  if (resp_status(n) >= 400) {
    return(tibble(osf_project = id, http_status = resp_status(n), obj_type = obj$type,
                  checked = checked))
  }
  node <- resp_body_json(n)$data

  # an incomplete child listing could miss licenses, so it counts as a failure
  # and the object is tried again on the next run.
  # max. 16 children in the 2026-10-16 crawl, so one page of 100 is enough
  k <- osf_get(paste0(node_url, "children/"), embed = "license", "page[size]" = 100)
  if (resp_status(k) != 200) stop("child listing returned ", resp_status(k))
  kids <- resp_body_json(k)
  if (kids$links$meta$total > 100) stop("more than 100 children")
  kid_lic <- vapply(kids$data, license_name, "")

  lic <- license_name(node)
  # copyright holders are names of people, so only keep whether there is one
  holders <- unlist(node$attributes$node_license$copyright_holders)

  tibble(
    osf_project = id,
    http_status = 200L,
    obj_type = obj$type,
    holder_type = node$type,
    node_id = node$id,
    license_name = lic,
    license_set = is_licensed(lic),
    has_copyright_holder = any(nzchar(trimws(holders))),
    license_year = node$attributes$node_license$year %||% NA_character_,
    n_children = length(kid_lic),
    n_children_licensed = sum(is_licensed(kid_lic)),
    child_licenses = paste(sort(unique(kid_lic[is_licensed(kid_lic)])), collapse = "; "),
    checked = checked
  )
}


# load links --------------------------------------------------------------
# which paper links which OSF id. 3 ids are linked by two papers,
# so we check each id once and join back to the papers at the end
pairs <- read.csv(file.path(MAT_DIR, "osf_file_downloads.csv")) |>
  distinct(doi, osf_project) |>
  filter(!is.na(osf_project), osf_project != "")
ids <- unique(pairs$osf_project)

if (is.finite(N_TEST)) {
  ids <- head(ids, N_TEST)
  pairs <- filter(pairs, osf_project %in% ids)
}


# crawl -------------------------------------------------------------------
cache <- if (file.exists(CACHE_FILE)) readRDS(CACHE_FILE) else list()
todo <- setdiff(ids, names(cache))
message(length(ids), " objects, ", length(todo), " not in cache yet")

for (i in seq_along(todo)) {
  id <- todo[i]
  res <- tryCatch(check_object(id), error = function(e) {
    message(id, " failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(res)) cache[[id]] <- res

  # save regularly so an interrupted run loses little
  if (i %% 25 == 0) {
    saveRDS(cache, CACHE_FILE)
    message(i, "/", length(todo))
  }
}
saveRDS(cache, CACHE_FILE)


# save --------------------------------------------------------------------
# left join so objects that failed in this run stay in with http_status NA
out <- left_join(pairs, bind_rows(cache[ids]), by = "osf_project")
write.csv(out, OUT_FILE, row.names = FALSE)

res <- distinct(out, osf_project, .keep_all = TRUE)
message("wrote ", nrow(out), " rows (", nrow(res), " objects) to ", OUT_FILE)
message("failed, re-run to retry: ", sum(is.na(res$http_status)))
print(count(res, http_status))
print(count(res, license_name, sort = TRUE))
