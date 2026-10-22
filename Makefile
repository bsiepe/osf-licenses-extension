# reproduce the analysis from the crawl snapshot in data/
#   make          download materials, run 02 and 04
#   make crawl    query OSF again (needs a token, overwrites data/osf_licenses.csv,
#                 data/osf_components.csv and data/license_mentions.csv, the coding
#                 of known passages is kept)
#   make docker   the same as make, inside the docker image
# 03 is not run: it drew the manual check sample once, the coding is based on it

RSCRIPT ?= Rscript
MAT_DIR := materials/v1.1.0
IMAGE := osf-licenses

.PHONY: all analysis manual crawl docker clean

all: analysis manual

$(MAT_DIR):
	"$(RSCRIPT)" 00_get_materials.R

analysis: $(MAT_DIR)
	"$(RSCRIPT)" 02_analyse_licenses.R

# 04 reads data/license_by_paper.csv from 02
manual: analysis
	"$(RSCRIPT)" 04_evaluate_manual_check.R

crawl: $(MAT_DIR)
	"$(RSCRIPT)" 01_collect_osf_licenses.R

docker:
	docker build -t $(IMAGE) .
	docker run --rm -v "$(CURDIR):/project" $(IMAGE)

# only removes the downloaded materials, outputs in data/ and figures/ are kept
clean:
	rm -rf materials
