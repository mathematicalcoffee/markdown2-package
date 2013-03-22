default_target: all
#=============================================================================
RCMD:=$(shell /usr/bin/which R)
RSCRIPT:=$(shell /usr/bin/which Rscript)
BUILD_DIR=bin
PACKAGE_NAME=markdown2
PACKAGE_DIR=$(PACKAGE_NAME)
VSN:=(shell grep -oP '(?<=^Version:)(\d+\.\d+(?:\.\d+)?) *$$' $(PACKAGE_DIR)/DESCRIPTION)
#=============================================================================
# Set environment variables for the build.
all: clean package
doc:
    @echo "Making documentation..."
    "$(RSCRIPT)" -e "library(utils); library(methods); library(roxygen2); roxygenise('$(PACKAGE_DIR)')";
# remove dir inst if empty (roxygen bug)
    @rmdir --i $(PACKAGE_DIR)/inst;

package: doc
    @if [ ! -d "$(BUILD_DIR)" ]; then \
        mkdir $(BUILD_DIR); \
    fi
    (cd $(BUILD_DIR); "$(RCMD)" CMD build ../$(PACKAGE_DIR);)

check: package
    "$(RCMD)" CMD check -o $(BUILD_DIR) $(PACKAGE_DIR);

install: package
    @(if [ "${R_LIBS_USER}" != "" ] ; then \
        echo "** Installing library" ;\
        "$(RCMD)" CMD REMOVE -l "${R_LIBS_USER}" $(PACKAGE_NAME) ;\
        "$(RCMD)" CMD INSTALL -l "${R_LIBS_USER}" $(BUILD_DIR)/$(PACKAGE_NAME)_$(VSN).tar.gz ;\
    else \
        echo "** Installing library as root" ;\
        "$(RCMD)" CMD REMOVE $(PACKAGE_NAME) ;\
        "$(RCMD)" CMD INSTALL $(BUILD_DIR)/$(PACKAGE_NAME)_$(VSN).tar.gz ;\
    fi)
dev-install: clean check install
zip: package
    "$(RCMD)" CMD INSTALL -l $(BUILD_DIR) $(BUILD_DIR)/$(PACKAGE_NAME)_$(VSN).tar.gz
    (cd $(BUILD_DIR); zip -r $(PACKAGE_NAME)_$(VSN).zip $(PACKAGE_NAME); rm -rf $(PACKAGE_NAME);)

clean:
    rm -f $(PACKAGE_DIR)/man/*.Rd;
    rm -rf $(BUILD_DIR)/*;
.PHONY: clean
