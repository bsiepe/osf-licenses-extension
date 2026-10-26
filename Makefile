# reproduce the analysis from the crawl snapshot in data/
#   make            download materials and render the supplement (05_supplement.html)
#   make analysis   only run 02, make manual 02 and 04
#   make crawl      query OSF again (needs a token, overwrites data/osf_licenses.csv,
#                   data/osf_components.csv and data/license_mentions.csv, the coding
#                   of known passages is kept)
#   make docker     the same as make, inside the docker image
# 03 is not run: it drew the manual check sample once, the coding is based on it

RSCRIPT ?= Rscript
QUARTO ?= quarto
MAT_DIR := materials/v1.1.0
IMAGE := osf-licenses

.PHONY: all analysis manual supplement crawl docker clean

all: supplement

$(MAT_DIR):
	"$(RSCRIPT)" 00_get_materials.R

analysis: $(MAT_DIR)
	"$(RSCRIPT)" 02_analyse_licenses.R

# 04 reads data/license_by_paper.csv from 02
manual: analysis
	"$(RSCRIPT)" 04_evaluate_manual_check.R

# the supplement runs 02 and 04 itself, so it only needs the materials
supplement: $(MAT_DIR)
	"$(QUARTO)" render 05_supplement.qmd

crawl: $(MAT_DIR)
	"$(RSCRIPT)" 01_collect_osf_licenses.R

docker:
	docker build -t $(IMAGE) .
	docker run --rm -v "$(CURDIR):/project" $(IMAGE)

# only removes the downloaded materials, outputs in data/ and figures/ are kept
clean:
	rm -rf materials
