# Licenses of shared OSF materials in psychology

This repository contains code and data for the project: 
> Siepe, B.S., Heck, D.W., Quintana, D. S. *Open research materials need explicit reuse licences*. 

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
| `02_analyse_licenses.R` | Main analysis: license rates and types, journals, documentation, figures |
| `03_sample_manual_check.R` | Draws the sample for the manual check (run once) |
| `04_evaluate_manual_check.R` | Compares the manual check with the crawled licenses |
| `05_supplement.qmd` | Supplement: method, coding schemes, results tables |

### `data/`

- `osf_licenses.csv`: one row per paper and linked OSF object, with the object's OSF license (crawl snapshot)
- `osf_components.csv`: one row per node below each object (the object itself at depth 0), with license, storage and GitHub add-on
- `license_mentions.csv`: one row per README or description that mentions a license, with a link to it and our hand coding
- `license_by_paper.csv`: one row per paper, whether and through which source it is licensed
- `manual_check.csv`, `manual_check_strata.csv`: one row per paper in the manual check sample
- `*.rds`: caches of the crawl, used by `01` only

[`data/codebook.csv`](data/codebook.csv) describes every column. Files join on `doi` (paper)
and `node_id` (OSF object); for descriptions, `license_mentions.csv` joins
`osf_components.csv` on `text_id` = `component_id`, and `manual_check_strata.csv` joins the sheet on `id`. In the `source` column,
`deposit` means Quintana's materials, `OSF API` and `GitHub API` the value as the API
returned it, `01`, `02`, `03` the script that computed it, and `hand coding` and
`manual check` our own coding.

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

This builds an image with R 4.6.1, the package versions in `renv.lock` and Quarto 1.7.34.
Inside it, it downloads Quintana's materials and renders the supplement,
`05_supplement.html`. Rendering runs `02` and `04`, so `data/` and `figures/` are
updated as well.

### Option 2: local R and Quarto

With R 4.6.1 and Quarto, start R in the repository root (this activates renv) and run:

```r
renv::restore()
source("00_get_materials.R")
```

Then render the supplement from the terminal:

```sh
quarto render 05_supplement.qmd
```

If make is installed, `make` downloads the materials and renders the supplement after
`renv::restore()`. `make analysis` and `make manual` run only the scripts.

Alternatively, you can also re-run the other scripts mentioned above with `source()` to 
re-run our crawl, analysis, and manual re-code.


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
