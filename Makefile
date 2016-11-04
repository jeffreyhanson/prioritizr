all: clean install document readme site vignettes check build

clean:
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*
	rm -rf README/*
	rm -f README.md

install:
	R -e "devtools::install_local('../prioritizr')"

document:
	R -e "devtools::load_all()"
	R -e "devtools::document()"

readme: install
	cd inst/vign;\
	R -e "knitr::knit('README.Rmd')";
	mv -f inst/vign/README.md README.md
	mv -f inst/vign/README README

vignettes: install
	rm -rf vignettes/*
	mkdir -p vignettes
	cd inst/vign;\
	R -e "knitr::knit('prioritizr-full.Rmd')";\
	R -e "knitr::knit('prioritizr-quickstart.Rmd')"
	mv inst/vign/prioritizr-full.md vignettes/prioritizr-full.Rmd
	mv inst/vign/prioritizr-quickstart.md vignettes/prioritizr-quickstart.Rmd
	mv -f inst/vign/figures vignettes/
	R -e "devtools::build_vignettes()"
	rm -rf vignettes/*
	cp -f inst/vign/placeholder-quickstart.Rmd vignettes/prioritizr-quickstart.Rmd
	cp -f inst/vign/placeholder-full.Rmd vignettes/prioritizr-full.Rmd
	touch inst/doc/prioritizr-quickstart.*
	touch inst/doc/prioritizr-full.*

site: document readme vignettes
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cp -f inst/vign/prioritizr-full.Rmd vignettes/prioritizr-full.Rmd
	cp -f inst/vign/prioritizr-quickstart.Rmd vignettes/prioritizr-quickstart.Rmd
	R -e "pkgdown::build_site()"
	rm -rf vignettes/*
	rm -rf inst/doc/*

check:
	R -e "devtools::check()"
	R -e "devtools::build_win()"

build:
	R -e "devtools::build()"

.PHONY: clean readme site vignettes check build
