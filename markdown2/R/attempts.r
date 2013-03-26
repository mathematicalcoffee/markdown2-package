
table.cell <- function(node) {
    profile$table.cell(xmlValue(node), is.heading=(node$name == 'th'), node=node)
}

passthrough <- function(node, ...) {
    unlist(xmlChildren(node), use.names=F)
}

toCharacterVector <- function (node, children) {
    if (!missing(node)) {
        children <- xmlChildren(node)
    }
    unname(sapply(children, function (ch) {
                       if (is.character(ch))
                           return(ch)
                       xmlValue(ch)
                       # flatten(ch) # <-- TODO: necessary since we go from the bottom-up?
                  }))
}

collapse <- function(..., collapse='', sep=' ') {
    paste(..., sep=sep, collapse=collapse)
}

flatten <- function(node, children, FUN=collapse, ...) {
    FUN(toCharacterVector(node, children), ...)
}

# GROSS ! should not be using regex for this...
preProcessHTML <- function(html) {
    # <pre><code> to <blockcode>
}

passthrought <- function (node) node












# -------------------------------------------------------------
# playing around
md <- renderMarkdown('test.md')
# need to surround in some tag to be the "root" element
md <- paste0('<root>', md, '</root>')

xml <- xmlTreeParse(md, asText=T, trim=T)
root <- xmlRoot(xml)

# work out tables
tbl.html <- '<root>
<table>
  <thead>
    <tr>
      <th>First Header</th>
      <th>Second Header</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Content cell</td>
      <td>second cell</td>
    </tr>
    <tr>
      <td>Third cell</td>
      <td>fourth cell</td>
    </tr>
  </tbody>
</table>
</root>
'

handlers <- list()

# col
handlers$td <- function(node, ...) {
    xmlValue(node)
}
handlers$th <- handlers$td

# row
handlers$tr <- function(node, ...) {
    cols <- unlist(xmlChildren(node), use.names=F)
    # equivalently paste(xmlChildren(node)) will do
    paste(cols, collapse=' & ')
}

# thead/tbody: pass through the vector of children?
# (if not, xmlChildren(node) for the table is thead and tbody)
# COULD DO handlers$table(rows.head, rows.body)
handlers$thead <- function(node, ...) {
    cat(node$name)
    xmlChildren(node)
}
handlers$tbody <- handlers$thead

# table
handlers$table <- function(node, ...) {
    rows <- unlist(xmlChildren(node), use.names=F)
    paste('\\tabular{ll}{', paste(rows, collapse=' \\cr\n'), '}', '\n')
}

tbl.xml <- xmlTreeParse(tbl.html, asText=T, handlers=handlers, asTree=T)
xmlRoot(tbl.xml)[[1]] # is what we want.


ul <- '<ul><li>one</li><li>two</li></ul>'
x <- xmlRoot(xmlTreeParse(ul, asText=T, handlers=list(li=xmlValue), asTree=T))

# code stuff
code.xml <- '
<root>
<p>Here is some <code>inline code</code>.</p>

<p>Here is some block code with pre/code (like from markdown):</p>

<pre><code class="R">
a &lt;- a + 1
</code></pre>

<p>Here is some preformatted code:</p>

<pre>
Preformatted!
</pre>
</root>
'
handlers <- list()
handlers$code <- function(node, ...) {
    cat('code\n')
    language <- xmlGetAttr(node, 'class')
    # docs suggest I can *ONLY* do this with useInternalNodes=T, but then I can't get my tree (as a string) back out!
    parent <- xmlParent(node) # BIG TODO: node is a XMLNode not a XMLTreeNode so xmlParent doesn't work
    if (xmlName(parent) == 'pre') {
        # <pre><code>: BLOCK CODE
        # (in R just use \\code but this is for testing)
        paste('\\blockcode{', xmlValue(node), '}', sep='\n')
    } else {
        # INLINE CODE
        paste0('\\code{', xmlValue(node), '}')
    }
}
handlers$pre <- function(node, ...) {
    cat('pre\n')
    # if it's <pre><code></code></pre> then ignore the 'pre'
    ch <- xmlChildren(node)
    if (is.character(ch[[1]])) {
        unlist(ch, use.names=F)
    } else {
        paste('\\preformatted{', xmlValue(node), '}', sep='\n')
    }
}
#handlers$startElement <- function(node, ...) {
#}

xmlTreeParse(code.xml, asText=T, asTree=T, useInternalNodes=T, handlers=handlers)

# ----- 
###############################################################
latexTag <- function(name, contents, optionalcontents=NULL) {
    if (!is.null(optionalcontents)) {
        return(paste0('\\', name, '{', contents, '}{', optionalcontents, '}'))
    }
    return(paste0('\\', name, '{', contents, '}'))
}

toCharacterVector <- function (node, children) {
    if (!missing(node)) {
        children <- xmlChildren(node)
    }
    sapply(children, function (ch) {
               if (is.character(ch))
                   return(ch)
               xmlValue(ch)
          })
}

collapse <- function(..., collapse='', sep=' ') {
    paste(..., sep=sep, collapse=collapse)
}

flatten <- function(node, children, FUN=collapse, ...) {
    FUN(toCharacterVector(node, children), ...)
}

code.xml <- '<p>This is <bold>some</bold> <code>inline code</code>.</p>'
profile <- list(bold = function(x) latexTag('bold', x),
                code = function (x) latexTag('code', x),
                p = function(x) x)

handlers <- list()
handlers$bold <- function (node) {
    txt <- flatten(node)
    profile$bold(txt)
}
handlers$code <- function(node) {
    xmlParent(node)
    txt <- flatten(node)
    profile$code(txt)
}
handlers$p <- function(node) {
    txt <- flatten(node)
    profile$p(txt)
}

xmlRoot(xmlTreeParse(code.xml, asText=T, asTree=T, handlers=handlers, trim=F))


###################################################################
# Works on internal nodes:
# Add the bits as text nodes surrounding the contents, and then as
# a final thing do xmlValue(tree) to recursively get the text out?
# Need internal to add text nodes.....BAH Looks like I can't supply handlers
# and get the modified tree back out.
dl.html <-'
<p> Some text. A nested <bold><em>italic</em> description</bold> tag.
<dl>
  <dt>Definition list</dt>
  <dd>Is something <bold>people</bold> use sometimes.</dd>

  <dt>Markdown in HTML</dt>
  <dd>Does *not* work **very** well. Use HTML <em>tags</em>.</dd>
</dl>
</p>
'

insertAt <- function(node, ..., at=NA) {
    suppressWarnings(addChildren(node, ..., at=at))
}
surround <- function(node, before, after) {
    insertAt(node, before, after, at=c(0, NA))
}

latexTag <- function(name, node, optionalcontents=NULL) {
    pref <- paste0('\\', name, '{')
    suff <- '}'
    if (!is.null(optionalcontents)) {
        suff <- paste0(suff, '{', optionalcontents, '}')
    }
    surround(node, pref, suff)
}

# replace xmlValue(node) with something else.
# Then we can do xmlValue(tree) and it will recursively extract everything (?)
profile <- list(bold = function(x) latexTag('bold', x),
                italic = function (x) latexTag('emph', x),
                code = function (x) latexTag('code', x),
                # bold the titles
                definition.list.title = function (x) latexTag('bold', x),
                definition.list = function(x, terms, definitions) {
                    # (really i want to add the decorations into the <dl> item don't i?)
                    # surround each title in \\item{ }
                    sapply(terms, latexTag, 'item')
                    # surround each definition in {}
                    sapply(definitions, surround, '{', '}')
                    # surround entire list in '\describe{}'
                    latexTag('describe', x)
                }
                )
# TODO: whati f we want to make non-mutable changes? or are they all mutable as this is internalNodes
handlers <- list()
handlers$bold <- function (node) {
    profile$bold(node)
    node
}
handlers$em <- function (node) {
    profile$italic(node)
    node
}
handlers$dt <- function (node) {
    profile$definition.list.title(node)
    node
}
handlers$dl <- function(node) {
    tms <- xmlElementsByTagName(node, 'dt')
    defs <- xmlElementsByTagName(node, 'dd')
    profile$definition.list(node, tms, defs)
    node
}
# WHY?? attempt to set attribute on NULL
# Documentation suggests I only get the tree back out if I have NO handlers (references
# to volatile C-level objects, disappear once processing is complete)
xmlRoot(xmlTreeParse(dl.html, asText=T, asTree=T, handlers=handlers, trim=F, useInternalNodes=T))

tr <- xmlRoot(xmlTreeParse(dl.html, asText=T, asTree=T, useInternalNodes=T, trim=F))
bold <- tr[[2]]
suppressWarnings(addChildren(bold, '\\bold{', '}', at=c(0, NA)))
bold

# -------------------------------------------------------------------
# Use SAX parsing to dump to a string...
# You only have the ability to specify what gets printed before and after
# the contents of the tag...
latexTag <- function (name, block=F) {
    cr <- ifelse(block, '\n', '')
    c(paste0(cr, '\\', name, '{', cr),
      paste0(cr, '}', cr))
}

handlers <- 
    list(
         .startElement=function(name, attrs, .state) {
             if (!is.null(bits[[name]]))
                 paste0(.state, bits[[name]][1])
             else
                 .state
         },

         .endElement=function(name, attrs, .state) {
             if (!is.null(bits[[name]]))
                 paste0(.state, bits[[name]][2])
             else
                 .state
         },

         .text=function(txt, .state) {
             paste0(.state, txt)
         })
bits <- list(bold=latexTag('bold'),
             em=latexTag('emph'),
             dt=c('\\item{\\bold{', '}}'),
             dd=c('{', '}'),
             dl=latexTag('describe'),
             p=c('', '\n\n'),
             br=c('', '\\cr')
             )

out <- xmlEventParse(dl.html, asText=T, handlers=handlers, state='', trim=F)
cat(out, '\n')

# handlers <- list(.startElement=function (name, attrs, .state) { message("STATE:", .state, "||"); .state })
# Bah but say I need to know what the next text node is....?
# (Example: need to know if the current text node is the caption of a link)
# How to do this without many state variable (.inLink <- T)

