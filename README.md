# Licenses of shared OSF materials in psychology

This repository contains code and data for the project: 
> TODO ADD FINAL TITLE

It extends a previous analysis by Quintana:

> Quintana, D. S. *Shared research materials in psychology are widely downloaded but often
> difficult to reuse*. Materials: https://doi.org/10.5281/zenodo.22805418 (v1.1.0)


The method, coding schemes, results and limitations are described in
[`05_supplement.qmd`](05_supplement.qmd).

# Overview

### Scripts

| Script | Description |
| --- | --- |
| `00_get_materials.R` | Downloads Quintana's materials (v1.1.0) from Zenodo into `materials/` |
| `01_collect_osf_licenses.R` | Crawls OSF and GitHub for licenses, LICENSE files, READMEs and descriptions |
| `02_analyse_licenses.R` | Main analysis: license rates and types, journals, documentation, download models, figures |
| `03_sample_manual_check.R` | Draws the sample for the manual check (run once) |
| `04_evaluate_manual_check.R` | Compares the manual check with the crawled licenses |
| `05_supplement.qmd` | Supplement: method, coding schemes, results tables |

### `data/`

- `osf_licenses.csv`: one row per paper and linked OSF object, with the object's OSF license (crawl snapshot)
- `osf_components.csv`: one row per node below each object (the object itself at depth 0), with license, storage and GitHub add-on
- `license_mentions.csv`: one row per README or description that mentions a license, coded by hand
- `license_by_paper.csv`: one row per paper, whether and through which source it is licensed
- `manual_check.csv`, `manual_check_strata.csv`: one row per paper in the manual check sample
- `*.rds`: caches of the crawl, used by `01` only

[`data/codebook.csv`](data/codebook.csv) describes every column. Files join on `doi` (paper)
and `node_id` (OSF object). In its `source` column, `deposit` means Quintana's materials,
`OSF API` and `GitHub API` mean the value as the API returned it, and `01`, `02`, `03` the
script that computed it.

### `figures/`

Share of licensed papers per journal before and after 2025, and license types.

# Reproducibility

The crawl results are in `data/`, so the analysis does not query OSF. It does need internet
access to download Quintana's materials from Zenodo and the figure font from Google Fonts.
There are two ways to run it, both from the repository root.

### Option 1: Docker and make (recommended)

```sh
make docker
```

This builds an image with R 4.6.1 and the package versions in `renv.lock`, and runs
`00`, `02` and `04` inside it. The outputs are written to `data/` and `figures/`.

### Option 2: local R

With R 4.6.1, start R in the repository root (this activates renv) and run:

```r
renv::restore()
source("00_get_materials.R")
source("02_analyse_licenses.R")
source("04_evaluate_manual_check.R")
```

If make is installed, `make` runs the three scripts instead.

### Supplement

`quarto render 05_supplement.qmd` renders the supplement. It needs Quarto, which is not in
the Docker image, and the packages from option 2.

### New crawl

This is not needed to reproduce the results. `01_collect_osf_licenses.R` queries OSF and
GitHub again, which takes a few hours and needs an OSF token stored with
`keyring::key_set("OSF", "OSF_PAT")`. Licenses on OSF change, so a new crawl will not match
the snapshot. By default `01` overwrites the files in `data/` (as does `make crawl`), keeping
the hand coding of passages it finds again. Set `OUT_DIR <- "data/new_crawl"` before
sourcing it to keep the snapshot.

# License

Code is licensed under MIT ([LICENSE](LICENSE)). Data, figures and the text of the
supplement are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
`data/license_mentions.csv` quotes short passages from other researchers' OSF pages, which
are not covered by this license.
