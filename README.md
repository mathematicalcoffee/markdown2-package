
# FOR THE USER

Get a zip/tar file with the package already generated, instead of installing from source.

Then to install, go into R:

    > install.packages("markdown2_1.0.tar.gz", repos=NULL)

# FOR THE DEVELOPER
R files are in `markdown2/R`.
Roxygen is used to generate documentation: all Rd files are generated and get written over.
A Makefile is provided for building.

From the terminal/command line [need to be able to run `R CMD ....`]

    make
    make install

This essentially runs (from directory bin):

    R CMD roxygen -d markdown2
    cd bin
    R CMD build ../markdown2
    R CMD INSTALL markdown2*.tar.gz
    cd ..

If you're developing and want to check the package (ie `R CMD check`), run:

    make check

To make a windows .zip file, run:

    make zip

NOTE: if you've set up a mercurial repository, there is a file
.precommit.sh that increments version numbers on every commit -- to disable
this feature edit .hg/hgrc and remove the "pre-commit = ../.precommit.sh"
line.

# FROM WITHIN R
This may or may not work...

Go to R, and set working dir to this directory.

    install.packages("markdown2", repos=NULL, type="source")

There is a possibility the documentation will be out of sync with the R files
 as I regenerate them afresh every time I modify the package -- if you want to
 be sure that the documentation is fresh, then:

    library(roxygen2)
    roxygenise("markdown2")
    install.packages("markdown2", repos=NULL, type="source")

Then, if all goes well:

    library(markdown2)
