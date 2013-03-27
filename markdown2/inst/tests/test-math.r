# source('test-includes.r')
library(testthat)
library(markdown)
library(XML)

md <- renderMarkdown(file='fixtures/math.md')
# OK, renderMarkdown converts ALL delimiters to \(\) (inline) and \[ \] (
md <- paste0('<root>', md, '</root>')

# PROBLEM: cannot do replacement or modification of the tree directly, because
#          with useInternalNodes=T and handlers, you can't return the tree
XML <- xmlTreeParse(md, asTree=T, asText=T, useInternalNodes=T)
                    handlers = list(p=function(node, ...) {
                        cat(xmlValue(node), '\n')
                    }))

library(utilitiesR) # regescape, regexp, ...
# TODO: namespace/module?

mathjax <- data.frame(type=rep(c('inline', 'display'), each=2),
                      pos=rep.int(c('start', 'end'), 2),
                      delim=c('\\(', '\\)', '\\[', '\\]'),
                      stringsAsFactors=F)

# Pre-processing that needs to be done:
# 1. <a href=><code></code></a> --> <a href=></a> (we will do the code ourselves?)
#   Note: we do this in the profile as it is Rd-specific: most other languages
#   would keep the \code{} inside the \link{} as it occurs in the HTML...
# TODO: we could do profile$link.internal and profile$link.external? cross-references v links?
# links to equations etc?
profile$code.inline <- function(x, node, ...) {
    parent <- xmlParent(node)
    # see if it's in a link to an internal URL.
    if (!is.null(parent) && xmlName(parent) == 'a' && is.internal.link(xmlGetAttr(parent, 'href'))) {
        # then skip
    }
    # CONTINUE
}

# 2. <pre><code> --> <blockcode> (<pre> on its own is \preformatted{})
#   Note: do this with the handlers as the profile should just use the code.block
#   and code.inline handlers.
handlers$code <- function(node, ...) {
    parent <- xmlParent(node)
    if (!is.null(parent) && xmlName(parent) == 'pre') {
        # SKIP ? (inline code)
    }
    # rest.
}
handlers$pre <- function(node, ...) {
    ch <- xmlChildren(node)
    # TODO: filter out blank text nodes from the <pre>
    #   V---- i.e. only node-child is a '<code>' with only spaces surrounding it (?)
    if (length(ch) == 1) {
        profile$code.block(...)
    } else {
        profile$preformatted(...) # NOTE BIGTODO isn't \preformatted{} just code.block?
    }
}

# 2. inline maths --> <math style="inline"> <math style="block">
# We do this in the handlers so we can fire the profile$math.inline event
# (not Rd-specific)
handlers$FOREACHNODE <- function(node, ...) {
    if (foundMaths) {
        profile$math.[inline|block](...)
    } # err what to do with the rest? paste it? what if it spans multiple nodes?
}


# BAH: how to modify the tree while using internal nodes?
# Maybe I use R-level nodes and just do flatten(node=, parent=)?

mathjax.start <- '(' %+% regescape('\\(') %+% '|' %+% regescape('\\[') %+% ')'
mathjax.end   <- '(' %+% regescape('\\)') %+% '|' %+% regescape('\\]') %+% ')'
mathjax.info <- list(
                     `\\(`=
                     )

# Hmmm....
onAllNodes <- function(node, ...) {
    f
}
