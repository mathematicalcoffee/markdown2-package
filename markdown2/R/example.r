rm(list=ls())
library(XML)
library(markdown)
source('markdown2.r')
# ----------------------
# Another attempt: walk the entire tree, flattening as we go
# Downside: have to parse the tree entirely once, then again.


test.md <-'
A simple text node

I do not like green eggs and Ham.
I **do not like them**, Sam I am!

* one
* two
 + a
 + b
* last
Here is a [link *read it*](asdf.html).
'
test.html.markdown <- renderMarkdown(text=test.md)
test.html.markdown <- paste0('<root>', test.html.markdown, '</root>')

# NOTE: the renderMarkdown converter always puts double spacing in between
# paragraphs so I don't need to worry about adding them myself.
# However if I write my own it doesn't do it UNLESS the <p> opening tag
# starts on a newline with no prior spaces
test.html <-
'<p>A simple text node</p>
<p>I do not like green eggs and Ham.
     I <strong>do not like them</strong>, Sam I am!
</p>
<p>
    <ul>
      <li>one</li>
      <li><strong>two</strong> <em>sublist</em>
        <ul>
          <li>a</li>
          <li>b</li>
        </ul></li>
      <li><em>last</em></li>
    </ul>
    Here is a <a href="asdf.html">link <em>read it</em></a>.
</p>'
test.html <- renderMarkdown(text=test.html)
test.html <- paste0('<root>', test.html, '</root>')

# TODO: we really want to standardise the spacing of the markdown
# before we parse.
# If you use the output from markdown->HTML, you need a newline after paragarphs.
#
# or maybe block.elements <- c('p', ...) # li, ul, .. ?
#          inline.elements <- c('code', 'em', 'strong', 'a', 'strikethrough', ...)
#          

# BIG TODO: Having problem with too many newlines.
# Really I want to trim whitespace, EXCEPT legitimate whitesapce
# e.g. "A <bold>happy</bold> day" should be "A ", "<bold>happy</bold>", " day" 
# NOT "A", "<bold>happy</bold>", "day"
# remove nodes that are only whitespace?
#cat(flatten(test.html, profile))

# OK: if you use HTML from the markdown package it's fine as-is (lines in between paragraphs, neatly formatted, ...)
# Otherwise we will have to do some work formatting everything nicely before it goes into the parser
# (Especially for newline-sensitive languages like latex)
#cat(flatten(test.html.markdown, profile))
cat(XMLTree2(test.html.markdown, RdProfile))
