{\rtf1\ansi\ansicpg1252\cocoartf2513
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fmodern\fcharset0 Courier;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs24 \cf2 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 # prepare the package for release\
\
NEWS     = NEWS\
PKGNAME := $(shell sed -n "s/Package: *\\([^ ]*\\)/\\1/p" DESCRIPTION)\
PKGVERS := $(shell sed -n "s/Version: *\\([^ ]*\\)/\\1/p" DESCRIPTION)\
PKGSRC  := $(shell basename `pwd`)\
\
all: news check clean\
\
deps:\
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.r-project.org")'\
\
docs:\
	R -q -e 'library(Rd2roxygen); rab(".", build = FALSE)'\
\
roxy:\
	R -q -e 'library(roxygen2); roxygenize(".")'\
test:\
	R -q -e "library(devtools); test('.')"\
\
build: roxy test\
	cd ..;\\\
	R CMD build $(PKGNAME)\
\
install: build\
	cd ..;\\\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz\
\
build_no_vignettes: roxy test\
	cd ..;\\\
	R CMD build --no-build-vignettes $(PKGNAME)\
\
install_no_vignettes: build_no_vignettes\
	cd ..;\\\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz\
\
check: build\
	cd ..;\\\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran\
\
clean:\
	cd ..;\\\
	$(RM) -r $(PKGNAME).Rcheck/;\\\
        $(RM) $(PKGNAME)_$(PKGVERS).tar.gz\
}