#------------------------------------------------------------------------------#
# Collect licenses of linked OSF objects
#
# Reads the OSF license of every OSF object that a paper in the original deposit
# links (osf_file_downloads.csv).
# Files have no license, so for a file link we use the project or registration
# holding the file. Components can have their own license, so all components
# below a linked object are read as well (data/osf_components.csv), with the
# storage each one uses. For a GitHub add-on, the license GitHub detects in the
# mirrored repository is recorded.
# A license can also be stated in the description or a README, so these are read
# too. Texts with "license" or "licence" go to data/license_mentions.csv and are
# coded by hand (rules in the supplement); the coding is kept on a re-run. The
# passages themselves are other researchers' text, so they go to
# data/license_passages.csv, which is not shared.
# Needs an OSF token, stored with keyring::key_set("OSF", "OSF_PAT") or set as
# the environment variable OSF_PAT.
#
# Terms used below:
# - linked id (osf_project): the OSF id a paper links to. a project, component,
#   registration or file
# - object (node_id): the project or registration whose license we read. the
#   linked id itself, or for a file the node holding it
# - component: any node below an object, at any depth
#------------------------------------------------------------------------------#


# setup -------------------------------------------------------------------
library(httr2)
library(dplyr)

MAT_DIR <- file.path("materials", "v1.1.0")
if (!dir.exists(MAT_DIR)) stop("run 00_get_materials.R first")

API <- "https://api.osf.io/v2/"
if (!exists("OUT_DIR")) OUT_DIR <- "data"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
CACHE_FILE <- file.path(OUT_DIR, "osf_license_cache.rds")
OUT_FILE <- file.path(OUT_DIR, "osf_licenses.csv")
MENTION_FILE <- file.path(OUT_DIR, "license_mentions.csv")
PASSAGE_FILE <- file.path(OUT_DIR, "license_passages.csv")
COMP_CACHE_FILE <- file.path(OUT_DIR, "osf_component_cache.rds")
COMP_FILE <- file.path(OUT_DIR, "osf_components.csv")
if (!exists("N_TEST")) N_TEST <- Inf

# readme file names as in Quintana's analyse_osf_readme.R
README_PATTERN <- "readme|read[_ ]me"
# broad on purpose, the passages are coded by hand
LICENSE_PATTERN <- "licen[sc]"
# characters kept on each side of a mention
PASSAGE_WIDTH <- 200
# stop walking a very large project after this many listings
MAX_LISTINGS <- 300

# the environment variable is for docker, where there is no keyring
osf_pat <- Sys.getenv("OSF_PAT")
if (!nzchar(osf_pat)) osf_pat <- keyring::key_get("OSF", "OSF_PAT")


# osf api -----------------------------------------------------------------
# rate limits and server errors are retried. if they persist we throw an error,
# and the object is tried again on the next run.
# other errors (private or deleted objects) are returned and recorded.
# the readme walks make many requests in a row, so requests are paced to stay
# below osf's rate limit (about one per second)
osf_get <- function(url, ...) {
  request(url) |>
    req_url_query(...) |>
    req_auth_bearer_token(osf_pat) |>
    req_throttle(capacity = 60, fill_time_s = 60) |>
    # osf sends no retry-after header and its limit lasts longer than the
    # default backoff, so we wait up to about 15 minutes before giving up
    req_retry(max_tries = 8, retry_on_failure = TRUE,
              backoff = \(i) 30 * i,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
    req_error(is_error = \(resp) resp_status(resp) == 429 || resp_status(resp) >= 500) |>
    req_perform()
}

# all items of a listing, following the next links
osf_list <- function(url) {
  resp <- osf_get(url, "page[size]" = 100)
  items <- list()
  repeat {
    if (resp_status(resp) >= 400) stop("listing returned ", resp_status(resp))
    body <- resp_body_json(resp)
    items <- c(items, body$data)
    if (is.null(body$links$`next`)) break
    resp <- osf_get(body$links$`next`)
  }
  items
}

node_url <- function(type, id) paste0(API, type, "/", id, "/")

# link to the contents of a storage or folder
files_url <- function(x) x$relationships$files$links$related$href

# needs a project or registration requested with embed = "license".
# NA if the authors never chose a license
license_name <- function(d) d$embeds$license$data$attributes$name %||% NA_character_

# "No license" is an explicit option on OSF and counts as unlicensed.
# "Other" means a custom license and counts as licensed
is_licensed <- function(name) !is.na(name) & name != "No license"


# readmes -----------------------------------------------------------------
readme_entry <- function(f) {
  tibble(text_id = f$id,
         file = f$attributes$name,
         path = f$attributes$materialized_path %||% NA_character_,
         download = f$links$download %||% NA_character_)
}

# items of the queue in find_readmes()
node_item <- function(type, id) list(kind = "node", type = type, id = id)
folder_item <- function(provider, url) list(kind = "folder", provider = provider, url = url)

# storages and components of a node, as new queue items
open_node <- function(item) {
  url <- node_url(item$type, item$id)
  storages <- lapply(osf_list(paste0(url, "files/")),
                     \(p) folder_item(p$attributes$provider, files_url(p)))
  components <- lapply(osf_list(paste0(url, "children/")),
                       \(k) node_item(k$type, k$id))
  c(storages, components)
}

# add-on storage (github, dropbox, ...) often fails, then we get NULL.
# a failure in osfstorage is thrown, so the object is retried
list_folder <- function(item) {
  tryCatch(osf_list(item$url), error = function(e) {
    if (item$provider == "osfstorage") stop(e)
    NULL
  })
}

# readme files of an object. the deposit's file listing has names but no ids,
# so we walk the file tree, breadth first through folders and components.
# stops once as many readmes are found as the deposit lists, or after
# MAX_LISTINGS listings
find_readmes <- function(type, id, n_expected) {
  queue <- list(node_item(type, id))
  found <- list()
  n_listings <- 0
  n_skipped <- 0

  while (length(queue) > 0 && length(found) < n_expected && n_listings < MAX_LISTINGS) {
    item <- queue[[1]]
    queue <- queue[-1]
    n_listings <- n_listings + 1

    if (item$kind == "node") {
      queue <- c(queue, open_node(item))
      next
    }

    entries <- list_folder(item)
    if (is.null(entries)) {
      n_skipped <- n_skipped + 1
      next
    }
    is_folder <- vapply(entries, \(e) e$attributes$kind == "folder", logical(1))
    subfolders <- lapply(entries[is_folder], \(e) folder_item(item$provider, files_url(e)))
    queue <- c(queue, subfolders)
    readmes <- Filter(\(e) grepl(README_PATTERN, e$attributes$name, ignore.case = TRUE),
                      entries[!is_folder])
    found <- c(found, lapply(readmes, readme_entry))
  }

  list(files = bind_rows(found), n_skipped = n_skipped)
}

# text of a file. pdf and docx are converted, everything else (txt, md, rtf,
# html, no extension) is read as text
raw_to_text <- function(raw, file) {
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("pdf", "docx")) {
    tmp <- tempfile(fileext = paste0(".", ext))
    on.exit(unlink(tmp))
    writeBin(raw, tmp)
    if (ext == "pdf") return(paste(suppressMessages(pdftools::pdf_text(tmp)), collapse = "\n"))
    # docx is a zip with the text in word/document.xml. a word can be split
    # over several tags, so tags are removed without adding a space
    xml <- paste(readLines(unz(tmp, "word/document.xml"), warn = FALSE, encoding = "UTF-8"),
                 collapse = "")
    return(gsub("<[^>]+>", "", gsub("</w:p>", "\n", xml)))
  }
  # nul bytes (utf-16 files) are dropped, invalid characters replaced
  text <- rawToChar(raw[raw != as.raw(0)])
  iconv(text, "UTF-8", "UTF-8", sub = " ")
}

# a file that cannot be downloaded or converted has text NA
download_text <- function(url, file) {
  if (is.na(url)) return(list(status = NA_integer_, text = NA_character_))
  resp <- osf_get(url)
  text <- NA_character_
  if (resp_status(resp) < 400) {
    text <- tryCatch(raw_to_text(resp_body_raw(resp), file), error = function(e) NA_character_)
  }
  list(status = resp_status(resp), text = text)
}

# readme texts of an object. for a link to a single file, the file itself is
# the readme
check_readmes <- function(linked, ref, n_expected) {
  if (linked$type == "files") {
    found <- list(files = readme_entry(linked), n_skipped = 0)
  } else {
    found <- find_readmes(ref$type, ref$id, n_expected)
  }
  files <- found$files
  if (nrow(files) == 0) return(list(texts = NULL, n_found = 0L, n_skipped = found$n_skipped))

  downloads <- lapply(seq_len(nrow(files)), \(i) download_text(files$download[i], files$file[i]))
  files$status <- sapply(downloads, \(d) d$status)
  files$text <- sapply(downloads, \(d) d$text)

  texts <- files |>
    select(-download) |>
    mutate(source = "readme", .before = 1)
  list(texts = texts, n_found = nrow(files), n_skipped = found$n_skipped)
}

# passages around each mention. overlapping passages are merged, several are
# joined with " [...] "
license_passages <- function(text) {
  if (is.na(text)) return(NA_character_)
  hits <- gregexpr(LICENSE_PATTERN, text, ignore.case = TRUE)[[1]]
  if (hits[1] == -1) return(NA_character_)
  from <- pmax(1, hits - PASSAGE_WIDTH)
  to <- pmin(nchar(text), hits + PASSAGE_WIDTH)
  group <- cumsum(c(TRUE, from[-1] > to[-length(to)]))
  from <- tapply(from, group, min)
  to <- tapply(to, group, max)
  passages <- gsub("\\s+", " ", trimws(substring(text, from, to)))
  paste(passages, collapse = " [...] ")
}


# objects -----------------------------------------------------------------
# the object of a linked id. for a file, the project or registration holding it
# (same rule as Quintana's collect_osf_documentation.R)
object_ref <- function(linked) {
  if (linked$type == "files") {
    return(linked$relationships$target$data %||% linked$relationships$node$data)
  }
  linked
}

# direct children of an object, for their descriptions. an incomplete listing
# could miss texts, so it throws and the object is tried again on the next run.
# max. 16 children in the 2026-10-16 crawl, so one page of 100 is enough
direct_children <- function(ref) {
  resp <- osf_get(paste0(node_url(ref$type, ref$id), "children/"), "page[size]" = 100)
  if (resp_status(resp) != 200) stop("child listing returned ", resp_status(resp))
  body <- resp_body_json(resp)
  if (body$links$meta$total > 100) stop("more than 100 children")
  body$data
}

description_texts <- function(node, children) {
  tibble(
    source = "description",
    text_id = c(node$id, vapply(children, \(k) k$id, "")),
    file = NA_character_,
    path = NA_character_,
    text = c(node$attributes$description %||% "",
             vapply(children, \(k) k$attributes$description %||% "", ""))
  )
}

# one row per linked id. n_readme is the number of readmes the deposit lists
# for it, NA if none
check_object <- function(id, n_readme) {
  checked <- format(Sys.Date())
  failed <- function(status, linked_type = NA_character_) {
    tibble(osf_project = id, http_status = status, obj_type = linked_type, checked = checked)
  }

  # guids/ works for any type of OSF id
  g <- osf_get(paste0(API, "guids/", id, "/"))
  if (resp_status(g) >= 400) return(failed(resp_status(g)))
  linked <- resp_body_json(g)$data

  ref <- object_ref(linked)
  if (!ref$type %in% c("nodes", "registrations")) return(failed(200L, linked$type))

  n <- osf_get(node_url(ref$type, ref$id), embed = "license")
  if (resp_status(n) >= 400) return(failed(resp_status(n), linked$type))
  node <- resp_body_json(n)$data

  texts <- description_texts(node, direct_children(ref))
  # readmes only where the deposit's file listing has one
  readmes <- list(texts = NULL, n_found = NA_integer_, n_skipped = NA_integer_)
  if (!is.na(n_readme)) readmes <- check_readmes(linked, ref, n_readme)
  texts <- bind_rows(texts, readmes$texts)

  lic <- license_name(node)
  # copyright holders are names of people, so only keep whether there is one
  holders <- unlist(node$attributes$node_license$copyright_holders)

  tibble(
    osf_project = id,
    http_status = 200L,
    obj_type = linked$type,
    holder_type = node$type,
    node_id = node$id,
    license_name = lic,
    license_set = is_licensed(lic),
    has_copyright_holder = any(nzchar(trimws(holders))),
    license_year = node$attributes$node_license$year %||% NA_character_,
    n_readme_expected = unname(n_readme),
    n_readme_found = readmes$n_found,
    n_storage_skipped = readmes$n_skipped,
    checked = checked,
    # full texts stay in the cache, they are not written to the csv
    texts = list(texts)
  )
}


# components --------------------------------------------------------------
# storage providers of a node, e.g. "osfstorage" or "github"
storage_providers <- function(type, id) {
  providers <- osf_list(paste0(node_url(type, id), "files/"))
  vapply(providers, \(p) p$attributes$provider, "")
}

# repository of a github add-on as "user/repo". osf adds the branch after a colon
github_repo <- function(id) {
  r <- osf_get(paste0(node_url("nodes", id), "addons/github/"))
  if (resp_status(r) >= 400) stop("github add-on returned ", resp_status(r))
  sub(":.*$", "", resp_body_json(r)$data$attributes$folder_id %||% NA_character_)
}

# license github detects in a repository, as SPDX id. "NOASSERTION" is a license
# file github cannot match, which we count as licensed (like "Other" on osf).
# "none" if there is no license file, NA if the repository is gone
github_license <- function(repo) {
  req <- request(paste0("https://api.github.com/repos/", repo)) |>
    req_error(is_error = \(resp) FALSE)
  # unauthenticated works too, github allows 60 requests per hour
  if (nzchar(Sys.getenv("GITHUB_PAT"))) req <- req_auth_bearer_token(req, Sys.getenv("GITHUB_PAT"))
  resp <- req_perform(req)
  if (resp_status(resp) == 404) return(NA_character_)
  if (resp_status(resp) >= 400) stop("github returned ", resp_status(resp))
  resp_body_json(resp)$license$spdx_id %||% "none"
}

# storage of a node, and the github repository and its license if there is one.
# registrations archive add-ons into osfstorage, so only nodes can have one
node_storage <- function(type, id) {
  providers <- storage_providers(type, id)
  repo <- NA_character_
  repo_license <- NA_character_
  if ("github" %in% providers && type == "nodes") {
    repo <- github_repo(id)
    if (!is.na(repo)) repo_license <- github_license(repo)
  }
  tibble(providers = paste(providers, collapse = "; "),
         github_repo = repo,
         github_license = repo_license)
}

# all components below an object, at any depth, with their license and storage.
# the first row is the object itself, its license is in osf_licenses.csv.
# a failed listing throws, so the object is tried again on the next run
check_components <- function(type, id) {
  nodes <- list(tibble(component_id = id, component_type = type, parent_id = NA_character_,
                       depth = 0L, license_name = NA_character_))
  queue <- list(list(type = type, id = id, depth = 0L))
  while (length(queue) > 0) {
    item <- queue[[1]]
    queue <- queue[-1]
    depth <- item$depth + 1L
    for (k in osf_list(paste0(node_url(item$type, item$id), "children/?embed=license"))) {
      nodes <- c(nodes, list(tibble(component_id = k$id, component_type = k$type,
                                    parent_id = item$id, depth = depth,
                                    license_name = license_name(k))))
      queue <- c(queue, list(list(type = k$type, id = k$id, depth = depth)))
    }
  }
  nodes <- bind_rows(nodes)

  storage <- lapply(seq_len(nrow(nodes)),
                    \(i) node_storage(nodes$component_type[i], nodes$component_id[i]))
  bind_cols(nodes, bind_rows(storage)) |>
    mutate(node_id = id,
           license_set = if_else(depth == 0, NA, is_licensed(license_name)),
           checked = format(Sys.Date()))
}


# crawl with a cache ------------------------------------------------------
# runs check() for every id that is not in the cache yet. a failed id is
# reported and left out, so it is tried again on the next run. the cache is
# saved regularly so an interrupted run loses little
run_cached <- function(ids, check, cache_file, label) {
  cache <- if (file.exists(cache_file)) readRDS(cache_file) else list()
  todo <- setdiff(ids, names(cache))
  message(length(ids), " ", label, ", ", length(todo), " not in cache yet")

  for (i in seq_along(todo)) {
    id <- todo[i]
    res <- tryCatch(check(id), error = function(e) {
      message(id, " failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(res)) cache[[id]] <- res
    if (i %% 25 == 0) {
      saveRDS(cache, cache_file)
      message(i, "/", length(todo))
    }
  }
  saveRDS(cache, cache_file)
  cache
}


# load links --------------------------------------------------------------
# which paper links which OSF id. 3 ids are linked by two papers,
# so we check each id once and join back to the papers at the end
pairs <- read.csv(file.path(MAT_DIR, "osf_file_downloads.csv")) |>
  distinct(doi, osf_project) |>
  filter(!is.na(osf_project), osf_project != "")
ids <- unique(pairs$osf_project)

# readme files the deposit lists per object, for the papers in its readme
# analysis (the 672). a project linked by two papers is listed twice, so
# files are counted per paper and object and the larger count is kept
readme_papers <- read.csv(file.path(MAT_DIR, "osf_readme_presence.csv")) |>
  filter(has_readme)
readme_counts <- read.csv(file.path(MAT_DIR, "osf_file_downloads.csv")) |>
  filter(doi %in% readme_papers$doi,
         grepl(README_PATTERN, file, ignore.case = TRUE)) |>
  count(doi, osf_project) |>
  group_by(osf_project) |>
  summarise(n = max(n))
readme_expected <- setNames(readme_counts$n, readme_counts$osf_project)

if (is.finite(N_TEST)) {
  ids <- head(ids, N_TEST)
  pairs <- filter(pairs, osf_project %in% ids)
}


# crawl -------------------------------------------------------------------
cache <- run_cached(ids, \(id) check_object(id, readme_expected[id]), CACHE_FILE, "objects")


# save --------------------------------------------------------------------
crawled <- bind_rows(cache[ids])

# older cache entries still have counts of direct children. they are replaced
# by the component walk below, so they are not written
old_cols <- c("n_children", "n_children_licensed", "child_licenses")

# left join so objects that failed in this run stay in with http_status NA
out <- left_join(pairs, select(crawled, -any_of(c("texts", old_cols))), by = "osf_project")
write.csv(out, OUT_FILE, row.names = FALSE)


# license mentions --------------------------------------------------------
# one row per text with a mention. distinct, because a file and its project
# can both be linked and then share their texts. node_id is the object whose
# license we read, 02 joins on it
mentions <- crawled |>
  select(node_id, texts) |>
  tidyr::unnest(texts) |>
  mutate(passages = vapply(text, license_passages, "")) |>
  filter(!is.na(passages)) |>
  distinct(source, node_id, text_id, .keep_all = TRUE) |>
  mutate(osf_link = paste0("https://osf.io/", if_else(source == "readme", node_id, text_id)),
         states_license = "", license_name = "", coding_note = "") |>
  select(source, node_id, text_id, file, path, osf_link, passages,
         states_license, license_name, coding_note)

# keep the coding of an earlier run. left join, new passages stay uncoded
key <- c("source", "node_id", "text_id")
coding <- c("states_license", "license_name", "coding_note")
if (file.exists(MENTION_FILE)) {
  coded <- read.csv(MENTION_FILE, colClasses = "character") |>
    select(all_of(c(key, coding)))
  lost <- anti_join(coded, mentions, by = key)
  if (nrow(lost) > 0) warning(nrow(lost), " coded passages are not found any more and are dropped")
  mentions <- mentions |>
    select(-all_of(coding)) |>
    left_join(coded, by = key) |>
    mutate(across(all_of(coding), \(x) coalesce(x, "")))
}
# the passages are quoted from other researchers' pages, which we have no
# license to share. the shared file has the coding and a link to each text,
# the passages go to a local file for reading while coding
write.csv(select(mentions, -passages), MENTION_FILE, row.names = FALSE, na = "")
write.csv(select(mentions, all_of(key), osf_link, passages), PASSAGE_FILE,
          row.names = FALSE, na = "")

objects <- distinct(out, osf_project, .keep_all = TRUE)
message("wrote ", nrow(out), " rows (", nrow(objects), " objects) to ", OUT_FILE)
message("failed, re-run to retry: ", sum(is.na(objects$http_status)))
print(count(objects, http_status))
print(count(objects, license_name, sort = TRUE))

# readme objects where the walk found fewer files than the deposit lists
objects |>
  filter(!is.na(n_readme_expected)) |>
  count(all_found = n_readme_found >= n_readme_expected, storage_skipped = n_storage_skipped > 0) |>
  print()
message("wrote ", nrow(mentions), " passages to ", MENTION_FILE,
        ", not coded yet: ", sum(mentions$states_license == ""))
print(count(mentions, source))


# components and storage --------------------------------------------------
# a separate walk with its own cache, so it can run on top of an existing crawl.
# objects that could not be read above (no node_id) are skipped
holders <- crawled |>
  filter(!is.na(node_id)) |>
  distinct(holder_type, node_id)
holder_types <- setNames(holders$holder_type, holders$node_id)

comp_cache <- run_cached(holders$node_id, \(id) check_components(holder_types[[id]], id),
                         COMP_CACHE_FILE, "objects for the component walk")

components <- bind_rows(comp_cache[holders$node_id]) |>
  select(node_id, component_id, component_type, parent_id, depth, license_name,
         license_set, providers, github_repo, github_license, checked)
write.csv(components, COMP_FILE, row.names = FALSE)

message("wrote ", nrow(components), " nodes to ", COMP_FILE,
        ", failed, re-run to retry: ", length(setdiff(holders$node_id, names(comp_cache))))
print(count(components, depth, license_set))
print(count(components, providers, sort = TRUE))
print(filter(components, !is.na(github_repo)) |> select(node_id, component_id, github_repo, github_license))
