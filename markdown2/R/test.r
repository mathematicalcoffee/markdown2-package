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

# ----- NOTE: am going to have to allow for each node needing concatenating like the `p` case...
code.xml <- '<p>This is some <code>inline code</code>.</p>'
handlers <- list()
handlers$code <- function (node) {
        paste0('\\code{', xmlValue(node), '}')
}
# paste together the bits of text
handlers$p <- function(node) {
        bits <- sapply(xmlChildren(node), function (ch) {
                               if (is.character(ch)) return(ch)
        xmlValue(ch)
            })
    paste(bits, collapse='')
}


xmlRoot(xmlTreeParse(code.xml, asText=T, asTree=T, handlers=handlers, trim=F))
