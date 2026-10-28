#------------------------------------------------------------------------------#
# Check whether your OSF projects have a license
#
# check_osf_licenses() takes OSF links or IDs (projects, components,
# registrations or files) or an OSF user ID, and returns one row per project
# with its license and owners (admin contributors). For links, direct
# components are checked too.
# Public projects need no token. To include your private projects, create a
# read-only token at https://osf.io/settings/tokens, run
# Sys.setenv(OSF_PAT = "...") and use user = "me".
#------------------------------------------------------------------------------#
library(httr2)

# helpers -----------------------------------------------------------------
osf_get <- function(url, ..., token = Sys.getenv("OSF_PAT")) {
  req <- request(url) |>
    req_url_query(..., .multi = "explode") |>
    req_retry(max_tries = 3)
  if (nzchar(token)) req <- req_auth_bearer_token(req, token)
  resp_body_json(req_perform(req))
}

node_embeds <- c("license", "contributors")

# all pages of a listing
osf_list <- function(url, embed = node_embeds) {
  out <- list()
  while (!is.null(url)) {
    res <- osf_get(url, embed = embed, `page[size]` = 100)
    out <- c(out, res$data)
    url <- res$links$`next`
  }
  out
}

# owners are the admin contributors, as only they can set the license.
# the node request embeds the first 10 contributors, get the rest if there are more
owners <- function(d) {
  ctb <- d$embeds$contributors
  if (is.null(ctb$data)) return(NA_character_)
  ctb_list <- ctb$data
  if (isTRUE(ctb$links$meta$total > length(ctb_list))) {
    ctb_list <- osf_list(d$relationships$contributors$links$related$href, embed = "users")
  }
  admins <- Filter(\(x) identical(x$attributes$permission, "admin"), ctb_list)
  nms <- vapply(admins, \(x) x$embeds$users$data$attributes$full_name %||% NA_character_, "")
  paste(nms, collapse = "; ")
}

# "No license" is an explicit choice on OSF, so it counts as unlicensed
license_row <- function(d) {
  lic <- d$embeds$license$data$attributes$name %||% NA_character_
  data.frame(id = d$id, type = d$type, title = d$attributes$title,
             public = d$attributes$public, license = lic,
             licensed = !is.na(lic) & lic != "No license", owners = owners(d),
             url = paste0("https://osf.io/", d$id, "/"), note = NA_character_)
}

error_row <- function(id, msg) {
  data.frame(id = id, type = NA, title = NA, public = NA, license = NA,
             licensed = NA, owners = NA, url = paste0("https://osf.io/", id, "/"),
             note = msg)
}

check_one <- function(id) {
  api <- "https://api.osf.io/v2/"
  obj <- osf_get(paste0(api, "guids/", id, "/"))$data
  # files have no license, so check the project holding the file
  if (obj$type == "files") {
    obj <- obj$relationships$target$data %||% obj$relationships$node$data
  }
  if (!obj$type %in% c("nodes", "registrations")) {
    return(error_row(id, paste("not a project:", obj$type)))
  }
  node_url <- paste0(api, obj$type, "/", obj$id, "/")
  nodes <- c(list(osf_get(node_url, embed = node_embeds)$data),
             osf_list(paste0(node_url, "children/")))
  do.call(rbind, lapply(nodes, license_row))
}


# main function -----------------------------------------------------------
check_osf_licenses <- function(ids = NULL, user = NULL) {
  out <- list()
  if (!is.null(ids)) {
    # accept full links like https://osf.io/abcde/files/ as well as bare IDs
    ids <- tolower(sub("^(https?://)?(www\\.)?osf\\.io/", "", trimws(ids)))
    ids <- sub("[/?].*$", "", ids)
    out <- lapply(ids, \(id) tryCatch(check_one(id), error = \(e) error_row(id, conditionMessage(e))))
  }
  if (!is.null(user)) {
    # all projects and components the user contributes to, plus registrations.
    # without a token only public ones are listed
    base <- paste0("https://api.osf.io/v2/users/", user, "/")
    nodes <- c(osf_list(paste0(base, "nodes/")), osf_list(paste0(base, "registrations/")))
    out <- c(out, lapply(nodes, license_row))
  }
  res <- unique(do.call(rbind, out))
  rownames(res) <- NULL
  res
}


# example -----------------------------------------------------------------
# res <- check_osf_licenses(c("https://osf.io/abcde/", "fghij"))
# res <- check_osf_licenses(user = "abcde")  # the ID in your OSF profile link
# res[!res$licensed %in% TRUE, c("title", "url", "note")]
