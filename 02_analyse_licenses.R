#------------------------------------------------------------------------------#
# Analyse licenses of shared OSF materials
# Project: License extension to Quintana, "Shared research materials in
# psychology are widely downloaded but often difficult to reuse"
# Author: Björn Siepe
#
# Builds a license indicator per paper from data/osf_licenses.csv (from 01) and
# describes licensing by year, journal, code sharing and documentation. Then adds
# licensing to the paper's download models and draws figures/plot_licenses.pdf.
# A linked OSF object counts as licensed if it has a license, or one of its direct children has
# a license, or if its file listing has a LICENSE file. A paper is licensed if
# any of its objects is. "No license" on OSF counts as unlicensed, "Other"
# (a custom license) as licensed.
#------------------------------------------------------------------------------#


# setup -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(patchwork)

MAT_DIR <- file.path("materials", "v1.1.0")
if (!dir.exists(MAT_DIR)) stop("run 00_get_materials.R first")

# Quintana's scripts read files by relative path, so run them inside the deposit
in_materials <- function(expr) {
  old <- setwd(MAT_DIR)
  on.exit(setwd(old))
  expr
}

# load the paper's functions for the download models (pred_prepare_data(),
# pred_build_dm()) and its plot theme. SKIP_SAVE stops the script from running
# its own analyses, as in plot_download_predictors_overview.R
SKIP_SAVE <- TRUE
in_materials(source("analyse_download_predictors.R"))

# number and share of licensed papers per group
share_licensed <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(n = n(), n_licensed = sum(licensed), pct = round(100 * mean(licensed)),
              .groups = "drop")
}


# load data ---------------------------------------------------------------
object_licenses <- read.csv("data/osf_licenses.csv")
osf_files <- read.csv(file.path(MAT_DIR, "osf_file_downloads.csv"))
paper_downloads <- read.csv(file.path(MAT_DIR, "osf_paper_downloads.csv"))
documentation <- read.csv(file.path(MAT_DIR, "documentation_levels.csv"))
prereg_flags <- read.csv(file.path(MAT_DIR, "prereg_flags.csv"))
article_types <- read.csv(file.path(MAT_DIR, "article_type.csv"))

# population: the 672 papers of the paper's documentation analysis, i.e. empirical
# papers sharing on OSF with a readable file listing.
# osf_files has one journal and year per paper
population <- read.csv(file.path(MAT_DIR, "osf_readme_presence.csv")) |>
  select(doi) |>
  left_join(distinct(osf_files, doi, journal, year), by = "doi")

# download population and predictors of the paper, from its own function
download_data <- in_materials(pred_prepare_data(verbose = FALSE))


# populations -------------------------------------------------------------
# how the deposit's paper counts relate. the paper reports 670 empirical papers
# with download counts; its download models exclude repositories with partial
# (mostly untracked external storage) or incomplete download data.
# doi matching as in pred_prepare_data(): lower case, trimmed
empirical <- tolower(trimws(article_types$doi[article_types$empirical %in% TRUE]))
is_empirical <- function(doi) tolower(trimws(doi)) %in% empirical

downloads_emp <- filter(paper_downloads, is_empirical(doi))
downloads_complete <- filter(downloads_emp, !download_data_partial %in% TRUE)

tibble(
  step = c("file listing",
           "file listing, empirical (license population)",
           "download counts",
           "download counts, empirical",
           "- partial download data",
           "- incomplete download data (download models)"),
  n = c(n_distinct(osf_files$doi),
        nrow(population),
        nrow(paper_downloads),
        nrow(downloads_emp),
        nrow(downloads_complete),
        nrow(download_data))
) |>
  print()


# license per object and paper --------------------------------------------
# LICENSE file. "license" can be anywhere in the name (e.g. "6. License.txt"),
# copying and copyright have to start it, so "copying_task.csv" does not count
is_license_file <- function(file) {
  grepl("licen[sc]e", file, ignore.case = TRUE) |
    grepl("^(copying|copyright)([._ -]|$)", file, ignore.case = TRUE)
}
license_files <- osf_files |>
  group_by(doi, osf_project) |>
  summarise(license_file = any(is_license_file(file)), .groups = "drop")

# left join, objects without a file listing have no row in license_files
object_licenses <- object_licenses |>
  left_join(license_files, by = c("doi", "osf_project")) |>
  mutate(license_file = license_file %in% TRUE,
         licensed = license_set %in% TRUE | coalesce(n_children_licensed, 0L) > 0 |
           license_file)

# papers link up to 12 OSF objects. licensed if at least one of them is
paper_summary <- object_licenses |>
  group_by(doi) |>
  summarise(
    n_objects = n(),
    n_unresolved = sum(is.na(node_id)),
    osf_license = any(license_set %in% TRUE),
    child_license = any(n_children_licensed > 0, na.rm = TRUE),
    chose_no_lic = any(license_name %in% "No license"),
    license_types = paste(sort(unique(license_name[license_set %in% TRUE])), collapse = "; "),
    license_file = any(license_file),
    licensed = any(licensed)
  )

# inner join keeps only the population (3 non-empirical papers drop out)
papers <- population |>
  inner_join(paper_summary, by = "doi") |>
  # drop papers where no linked object could be read (none in the current crawl)
  filter(n_unresolved < n_objects)

write.csv(papers, "data/license_by_paper.csv", row.names = FALSE)


# how many papers and objects are licensed --------------------------------
# objects are the distinct OSF ids linked by the 672 papers (936 of the 939,
# the other 3 belong to the non-empirical papers)
linked_objects <- object_licenses |>
  filter(doi %in% papers$doi) |>
  distinct(osf_project, .keep_all = TRUE)

tibble(unit = c("papers", "linked OSF objects"),
       n = c(nrow(papers), nrow(linked_objects)),
       licensed = c(sum(papers$licensed), sum(linked_objects$licensed))) |>
  mutate(not_licensed = n - licensed, pct_licensed = round(100 * licensed / n)) |>
  print()

# where the license of a paper comes from. exclusive, so each paper counts once
papers |>
  summarise(
    osf_object = sum(osf_license),
    child_only = sum(!osf_license & child_license),
    license_file_only = sum(!osf_license & !child_license & license_file),
    no_license_chosen = sum(!licensed & chose_no_lic),
    any_license_file = sum(license_file)
  ) |>
  tidyr::pivot_longer(everything(), names_to = "route", values_to = "n") |>
  mutate(pct = round(100 * n / nrow(papers))) |>
  print()

# license types, counted per OSF object whose license we read (a project and
# a file in it can both be linked by the same paper)
license_counts <- object_licenses |>
  filter(license_set %in% TRUE, doi %in% papers$doi) |>
  distinct(node_id, license_name) |>
  count(license_name, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n)))
print(license_counts)


# year and journal --------------------------------------------------------
# licenses are read at the crawl date, so older papers had more time to get one
# and trends over years are hard to interpret. we only compare before and after
# Psychological Science's open license requirement (submissions from 2024).
# 2015 has only 45 papers (42 in Psych Sci), so it is pooled with 2020
papers$period <- if_else(papers$year == 2025, "2025", "2015 + 2020")

print(share_licensed(papers, journal, year), n = Inf)
by_period <- share_licensed(papers, journal, period)
print(by_period)


# registrations -----------------------------------------------------------
# OSF asks for a license when a registration is created, which raises the
# 2025 rates as well. holder_type is the object whose license we read (for
# files, the project or registration holding the file)
object_licenses |>
  inner_join(select(papers, doi, year), by = "doi") |>
  group_by(holder_type, year) |>
  summarise(n = n(), pct = round(100 * mean(license_set %in% TRUE)), .groups = "drop") |>
  print()

# same comparison without registrations and files held in them. papers that
# only link registrations drop out via the inner join. %in% keeps unreadable
# objects (holder_type NA), as in papers
no_reg <- object_licenses |>
  filter(!holder_type %in% "registrations") |>
  group_by(doi) |>
  summarise(licensed = any(licensed))
papers_no_reg <- papers |>
  select(doi, journal, period) |>
  inner_join(no_reg, by = "doi")
print(share_licensed(papers_no_reg, journal, period))


# code sharing and documentation ------------------------------------------
# n_code_files counts code in any language
papers |>
  mutate(shares_code = doi %in% paper_downloads$doi[paper_downloads$n_code_files > 0]) |>
  share_licensed(shares_code) |>
  print()

# documented as in the paper: README or substantive reuse guidance in the OSF
# description or wiki
papers_doc <- inner_join(papers, select(documentation, doi, documented), by = "doi")
doc_table <- table(licensed = papers_doc$licensed, documented = papers_doc$documented)
print(doc_table)
print(fisher.test(doc_table))


# download models ---------------------------------------------------------
# doi case can differ in prereg_flags.csv
download_data$licensed <- download_data$doi %in% papers$doi[papers$licensed]
download_data$documented <- download_data$doi %in% documentation$doi[documentation$documented %in% TRUE]
download_data$prereg <- tolower(download_data$doi) %in% tolower(prereg_flags$doi[prereg_flags$prereg %in% TRUE])

print(share_licensed(download_data, prereg))
download_data |>
  group_by(licensed) |>
  summarise(n = n(), median_downloads = median(total_downloads)) |>
  print()

# adds standardised predictors (z_cit, z_att, z_files) and log downloads
# (l_total for all files, l_max for the most downloaded file).
# rhs is the paper's right-hand side: citations, attention, repository size,
# journal and year
model_data <- pred_build_dm(download_data)
model_df <- model_data$dm
# effect coding for journal as in the paper
contrasts(model_df$journal) <- contr.sum(nlevels(model_df$journal))

model_formulas <- c(
  total = "l_total ~ RHS + licensed",
  total_doc = "l_total ~ RHS + licensed + documented",
  total_prereg = "l_total ~ RHS + licensed + prereg",
  total_doc_prereg = "l_total ~ RHS + licensed + documented + prereg",
  peak = "l_max ~ RHS + licensed",
  peak_doc_prereg = "l_max ~ RHS + licensed + documented + prereg"
)
download_models <- lapply(model_formulas, \(f) {
  lm(as.formula(sub("RHS", model_data$rhs, f)), data = model_df)
})

# licensed is 0/1, so exp(b) - 1 is the % difference in downloads
lapply(download_models, \(m) {
  broom::tidy(m, conf.int = TRUE) |>
    filter(term == "licensedTRUE") |>
    mutate(adj_r2 = summary(m)$adj.r.squared)
}) |>
  bind_rows(.id = "model") |>
  mutate(pct = round(100 * (exp(estimate) - 1)),
         across(c(estimate, conf.low, conf.high, adj_r2), \(x) round(x, 2)),
         p.value = signif(p.value, 2)) |>
  select(model, estimate, conf.low, conf.high, pct, p.value, adj_r2) |>
  print()

print(round(coef(summary(download_models$total_doc_prereg)), 3))


# figure ------------------------------------------------------------------
# News Cycle from Google Fonts, downloaded on every run (needs internet)
library(showtext)
FONT <- "News Cycle"
font_add_google(FONT)
showtext_auto()

# journal abbreviations and colours as in the paper's Figure 2b
journal_abbr <- c("Psychological Science" = "Psych Sci",
                  "Journal of Experimental Social Psychology" = "JESP",
                  "Journal of Memory and Language" = "JML",
                  "Cognition" = "Cognition")
journal_cols <- c(Cognition = OKABE_ITO[["orange"]],
                  `Psych Sci` = OKABE_ITO[["skyblue"]],
                  JESP = OKABE_ITO[["green"]],
                  JML = OKABE_ITO[["yellow"]],
                  `All journals` = OKABE_ITO[["black"]])

# panel a: share licensed before and after the Psych Sci requirement, per
# journal and for all journals together (dashed)
by_period_all <- by_period |>
  mutate(journal = unname(journal_abbr[journal])) |>
  bind_rows(mutate(share_licensed(papers, period), journal = "All journals")) |>
  mutate(overall = journal == "All journals")

# one row per journal with both periods, for the labels and the dumbbells
before_after <- by_period_all |>
  mutate(period = if_else(period == "2025", "after", "before")) |>
  select(journal, overall, period, pct, n) |>
  tidyr::pivot_wider(names_from = period, values_from = c(pct, n))

# version 1: slope chart, labelled only at the right end
slope_labels <- by_period_all |>
  filter(period == "2025") |>
  left_join(select(before_after, journal, pct_before), by = "journal") |>
  mutate(lab = sprintf("%s  %d%% → %d%%", journal, pct_before, pct))

plot_trend <- ggplot(by_period_all, aes(period, pct / 100, colour = journal, group = journal)) +
  geom_line(aes(linetype = overall), linewidth = 1) +
  geom_point(size = 2.5) +
  ggrepel::geom_text_repel(data = slope_labels, aes(label = lab),
                           hjust = 0, nudge_x = 0.15, direction = "y", segment.color = NA,
                           # keep the labels right of the 2025 points
                           xlim = c(2.13, NA),
                           size = 3.9, colour = "grey20", family = FONT) +
  scale_colour_manual(values = journal_cols, guide = "none") +
  scale_linetype_manual(values = c("solid", "22"), guide = "none") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  # room on the right for the labels
  scale_x_discrete(expand = expansion(add = c(0.25, 1.5))) +
  labs(x = "Publication year", y = "OSF-sharing papers with a license") +
  theme_dv("h") +
  theme(axis.line.x = element_blank())

# version 2: dumbbell chart, open dot 2015 + 2020, filled dot 2025.
# rows sorted by the 2025 share, all journals at the bottom
dumbbell <- before_after |>
  mutate(up = pct_after >= pct_before) |>
  arrange(!overall, pct_after) |>
  mutate(row = factor(journal, levels = journal))
dumbbell_points <- dumbbell |>
  select(row, journal, up, pct_before, pct_after, n_before, n_after) |>
  tidyr::pivot_longer(c(pct_before, pct_after, n_before, n_after),
                      names_to = c(".value", "period"), names_sep = "_") |>
  mutate(period = if_else(period == "after", "2025", "2015 + 2020"),
         # put each label on the outer side of its dot
         outer = (period == "2025") == up,
         lab_x = pct / 100 + if_else(outer, 0.045, -0.045),
         lab = sprintf("%d%% (%d)", pct, n))

plot_dumbbell <- ggplot(dumbbell, aes(y = row, colour = journal)) +
  geom_segment(aes(x = pct_before / 100, xend = pct_after / 100, yend = row,
                   linetype = overall), linewidth = 1) +
  geom_point(data = dumbbell_points, aes(x = pct / 100, shape = period),
             size = 3, stroke = 1.2, fill = "white") +
  geom_text(data = dumbbell_points,
            aes(x = lab_x, label = lab, hjust = if_else(outer, 0, 1)),
            size = 3.9, colour = "grey20", family = FONT) +
  scale_colour_manual(values = journal_cols, guide = "none") +
  scale_linetype_manual(values = c("solid", "22"), guide = "none") +
  scale_shape_manual(values = c("2015 + 2020" = 21, "2025" = 19), name = NULL) +
  # room for the labels left of 0% and right of 100%
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = scales::percent,
                     expand = expansion(add = c(0.25, 0.22))) +
  labs(x = "OSF-sharing papers with a license", y = NULL) +
  theme_dv("v") +
  theme(legend.position = "top", axis.line = element_blank())

# panel b: license types as SPDX identifiers (spdx.org/licenses). names not in
# the list are kept as they are. the full names go in the figure note
license_abbr <- c(
  "CC-By Attribution 4.0 International" = "CC-BY-4.0",
  "CC0 1.0 Universal" = "CC0-1.0",
  "CC-By Attribution-NonCommercial-NoDerivatives 4.0 International" = "CC-BY-NC-ND-4.0",
  "CC-By Attribution-ShareAlike 4.0 International" = "CC-BY-SA-4.0",
  "CC-BY Attribution-NonCommercial 4.0 International" = "CC-BY-NC-4.0",
  "MIT License" = "MIT",
  "BSD 2-Clause \"Simplified\" License" = "BSD-2-Clause",
  "GNU Lesser General Public License (LGPL) 3.0" = "LGPL-3.0",
  "GNU General Public License (GPL) 3.0" = "GPL-3.0",
  "Apache License 2.0" = "Apache-2.0",
  "Academic Free License (AFL) 3.0" = "AFL-3.0"
)
license_counts$short <- coalesce(unname(license_abbr[license_counts$license_name]),
                                 license_counts$license_name)
print(select(license_counts, short, license_name, n))

plot_types <- ggplot(license_counts, aes(n, reorder(short, n))) +
  geom_col(fill = OKABE_ITO[["blue"]], width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.9, colour = "grey20", family = FONT) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Licensed OSF objects", y = NULL) +
  theme_dv("v")

dir.create("figures", showWarnings = FALSE)
fig <- plot_trend + plot_types + plot_layout(widths = c(1.3, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(text = element_text(family = FONT))
# close to the text width of the paper, so the fonts are not scaled down much
ggsave("figures/plot_licenses.pdf", fig, width = 8, height = 4.5)

fig_dumbbell <- plot_dumbbell + plot_types + plot_layout(widths = c(1.4, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(text = element_text(family = FONT))
ggsave("figures/plot_licenses_dumbbell.pdf", fig_dumbbell, width = 8, height = 4.5)
