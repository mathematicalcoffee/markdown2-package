#' Convert markdown to some to some other format, with custom rules.
#'
#' @param file
#' @param text
# TODO: how to save your own profile?
#' @param to name of a pre-existing profile to use for the conversion?
#' @param profile a profile to use for the conversion?
#' @param ... a key=value pair of event handlers, see DETAILS for potential ones (?)
#' @note
#' This isn't a proper markdown parser.
#' What we do is use the `markdown` package's [markdownToHTML](markdown:markdownToHTML)
#' function to convert the input to HTML, and *then* fire events to convert
#' the HTML according to the specified profile.
# TODO: markdownExtensions (passed to markdown2)
# TODO: markdownToHTML vs renderMarkdown?
# TODO: use roxygen to make .Rd, then convert .Rd to HTML, then markdown that, then HTML back to .Rd?
# R CMD Rdconv: Rd --> HTML
# HTML --> Rd: ???
markdown2 <- function(file, output, text,
                      renderer.options=NULL,                       # passed to markdown
                      extensions=getOption('markdown.extensions'), # passed to markdown
                      profile='HTML', ...) {

    html <- renderMarkdown(file=file,
                           text=text,
                           renderer='HTML',
                           renderer.options=c('fragment_only', renderer.options), # TODO: fragment_only? (eliminate JS/header/body components)
                           extensions=extensions)
    # xmlEventParse?
    # now get a parse tree
    # tree <- xmlTreeParse(html, asText=T, handlers=myHandlersToXMLHandlers(getProfile(profile)),
    #                       asTree=T)
    # use handlers=list(...) # function (node) {}
    # todo: xmlEventParse?
    # now fire events
}

# NOTE: do I just define my own print.XMLNode?

# xmlEventParse: doesn't create nodes unless you use the branches argument which
#                doesn't work with nested stuff.
# xmlParse:

# this one parses in order of occurence
# BAH xm
x <- xmlParse(txt, asText=T, useInternalNodes=T,
              list(ol = function(x, attrs) {
                       cat('ol\n')
                   },
                   li = function(li, attrs) {
                       cat('li\n')
                       recover()
                   })
              )

# Need something that uses the lookups better...
# todo: heading wants the contents until the next heading too...
header <- function(node) {
    lvl <- switch(node$name, h1=1, h2=2, h3=3, h4=4, h5=5, h6=6, NA)
    profile$heading(xmlValue(node), level=lvl, node=node)
}
table.cell <- function(node) {
    profile$table.cell(xmlValue(node), is.heading=(node$name == 'th'), node=node)
}

passthrough <- function(node, ...) {
    unlist(xmlChildren(node), use.names=F)
}

## TODO: do I really want to modify a *copy* of the tree so subsequent handlers are guaranteed
## to get nodes only?
# function node, text ? function(text, node)?
myHandlersToXMLHandlers <- function(profile) {
    # xmlValue(node)

    handlers <- list(
        # lists
        # nested stuff is converted before unnested stuff
        ul=function(node) {
            # by now `li` has converted xmlChildren(node) to text.
            children <- unlist(xmlChildren(node), use.names=F)
            profile$list.unordered(children)
        },

        ol=function(node) {
            children <- unlist(xmlChildren(node), use.names=F)
            profile$list.unordered(children)
        },

        dl=function(node) {
            # TODO
        },

        a=function(node) {
            caption <- xmlValue(node)
            if (!length(caption) || !nchar(caption)) {
                caption <- NULL
            }
            url <- xmlGetAttr(node, 'href')
            profile$link(url, caption=caption, node=node)
        },

        # NOTE: this covers <pre><code> as well as <code>
        code=function(node) {
            language <- xmlGetAttr(node, 'class')
            parent <- xmlParent(node)
            if (parent && parent$name == 'pre') {
                # <pre><code>: BLOCK CODE
                profile$code.block(xmlValue(node), language=language, node=node)
            } else {
                # INLINE CODE
                profile$code.inline(xmlValue(node), language=language, node=node)
            }
        },

        pre=function(node) {
            if (is.character(xmlChildren(node)[[1]])) {
            } else {
                # pre all on its own...
                profile$code.block(xmlValue(node), language=language, node=node)
            }
            if (is.character
            # code block (language)
        },

        h1=header,
        h2=header,
        h3=header,
        h4=header,
        h5=header,
        h6=header,

        img=function(node) {
            # caption, alt, link, ...
        },

        # TODO: supply coordinates with these: table, th, tr, td
        tr=, td=function(node) {
        }

        td=function(node) {
        },
        table=

        # blockquote, li, br, p, code(inline), strong, italic, strikethrough
        # TOFIX: tr, td, th
        # TODO: superscript? (the thing under the superscript)
        # TODO: li(text, index)?
        startElement=function(node) {
            tag <- node$name
            pname <- XMLMapping2MyHandlers[[tag]]
            if (profile[[pname]]) {
                profile[[pname]](xmlValue(node), node=node)
            } else {
                # unrecognised tags just return xmlValue(node) (?)
                # thead, tbody, ...
                xmlValue(node)
            }
        },

    # tables
    table='table',
    th=,   # text, col
    thead=, # <-- ?
    tbody=, # <-- ?
    tr=, # row
    td=, # row, col

    sup='superscript', # <-- TODO: do *not* want this? (if inside math environment?)
        # all others just return xmlValue
        # TODO: ignore all others?
        # UPTO
    )

    # restrict to where profile$[asdf] exists.
    handlersInProfile <- names(profile)
    handlersInXML <- MyHandlers2XMLMapping[handlersInProfile]
    handlers <- handlers[handlersInXML]

    handlers
}

# Math (Ajax): \( \) (inline); $$ and \[ \] (block)
# MathML: <math></math> or <math display="inline"></math> for inline
# and <math display="block"></math>
# 
#
#
# TODO: HTML ENTITIES
MyHandlers2XMLMapping=list(
)
XMLMapping2MyHandlers = list(
    # NOTE: mathjax is left as \(\) and \[ \], so no HTML element is made for it
    # TODO: math, form, figure, fieldset, script, noscript, style, iframe, ins
    # ENTITY REPLACEMENT
    p='pagaraph',
    br='linebreak',
    blockquote='quote',

    dl='list.definition',
    ul='list.unordered',
    ol='list.ordered',
    li='list.item',    # <-- ???

    a='link',


    code='code.inline',
    pre='code.block', # <-- TODO: <pre><code> for code.block

    h1='heading',
    h2='heading',
    h3='heading',
    h4='heading',
    h5='heading',
    h6='heading',

    # tables
    table='table',
    thead='table.heading',
    tbody='table.body',
    tr='table.row',
    td='table.cell',
    th='table.cell', # isHeading=TRUE

    strong='bold',
    em='italic',

    del='strikethrough',
    sup='superscript', # <-- TODO: do *not* want this? (if inside math environment?)

    img='image', # <--

)


getProfile <- function(profile) {
    .Profiles[[profile]]
}

# TODO: how to save forever
registerProfile <- function(name, handlers) {
    .Profiles[[name]] <- handlers
}

.Profiles <- list()


registerProfile(HTML, list())

# http://cran.r-project.org/doc/manuals/R-exts.html


# \name{contents}{optionalcontents}
latexTag <- function(name, contents, optionalcontents=NULL) {
    if (!is.null(optionalcontents)) {
        return(paste0('\\', name, '{', contents, '}{', optionalcontents, '}'))
    }
    return(paste0('\\', name, '{', contents, '}'))
}

is.internal.link <- function(url) {
    # if the url is =dest or package: or package:page or a function name
    # then that is the URL too
    grepl('^(=[\\w.-]+|[\\w.-]+:[\\w.-]*|[\\w.]+)$', url, perl=T)
}
# TODO: provide parse tree?
# PROFILES
# NULL = "leave as-is (as in the source markdown)"
# TODO: *MUST* provide a way to access context (in the parse tree)?
RdProfile <- list(
                  # STRUCTURING ELEMENTS
                  # x: the text in the paragraph
                  paragraph=function (x) {
                    x
                  },

                  linebreak=function () {
                      # \cr
                      '\\cr'
                  },

                  # Note: should roxygen comments really have internal headings?!
                  # TODO: what about the *contents* of that heading?
                  # e.g. "# Usage" should be converted to "\usage{ .... }" ?
                  # (this will allow Rd files to be written ENTIRELY in markdown
                  #  rather than roxygen->markdown)
                  heading=function(text, contents, level) {

                      tagName <- switch(tolower(text),
                                        usage='usage',
                                        description='description',
                                        seealso=,
                                        `see also`='seealso',
                                        arguments='arguments',  # TODO: \describe context
                                        value='value',          # TODO: \describe context
                                        references='references',
                                        note=,
                                        notes='note',
                                        authors=,
                                        author='author',        # TODO: one per author
                                        keywords=,
                                        keyword='keyword',        # TODO: one per keyword
                                        format='format',
                                        source='source',
                                        example=,
                                        examples='examples',
                                        'unknown')

                      if (tagName == 'unknown') {
                          if (level == 1) {
                              return(latexTag('section', text, contents))
                          } else {
                              return(latexTag('subsection', text, contents))
                          }
                      }
                      latexTag(tagName, contents)
                  },

                  quote=function () {
                      # TODO
                  },

                  # TODO: [`asdf`](fdsa)
                  #  turns into link(code(asdf), fdsa)
                  #  turns into \link{\code{}}
                  # but we want \code{\link{}}

                  # LINKS
                  # \link{name}                 [name](name)
                  # \link[=dest]{name}          [name](=dest)
                  # \link[package]{name}        [name](package:)
                  # \link[package:page]{name}   [name](package:page)
                  # Note: for internal links, there are NO CAPTIONS.
                  # (But if the user specifies [asdf]() we will treat as internal)
                  link=function(url, caption=NULL) {
                      # If they did [asdf]() copy over the caption to the url
                      if (!nchar(url) && !is.null(caption) && nchar(caption))
                          url <- caption

                      if (!is.internal.link(url)) {
                          # external link
                          if (!is.null(caption) && nchar(caption)) {
                              return(latexTag('href', url, caption))
                          } else {
                              return(latexTag('url', url))
                          }
                      } else {
                          tName <- 'link'
                          if (name != url) {
                              tName <- paste0(tName, '[', url, ']')
                          }
                          return(latexTag(tname, caption))
                      }
                  },

                  # TODO: is there markdown for email?
                  email=function(email) {
                      latexTag('email', email)
                  },

                  # LISTS
                  # TODO: \value{...} is a \describe environment already
                  list.unordered=function(items) {
                      items <- paste('\\item', items, collapse='\n')
                      latexTag('itemize', items)
                  },

                  list.ordered=function(items) {
                      items <- paste('\\item', items, collapse='\n')
                      latexTag('enumerate', items)
                  },

                  list.describe=function(definitions, items) {
                      items <- paste0('\\item{', definitions, '}{', items, '}', collapse='\n')
                      latexTag('describe', items)
                  },

                  # TABLES
                  # cells is a matrix row/cols and rownames/colnames (?)
                  table=function(cells) {
                      nr <- nrow(cells)
                      nc <- ncol(cells)

                      if (rownames(cells) != as.character(1:nr)) {
                          cells <- cbind(rownames(cells), cells)
                      }
                      if (colnames(cells) != as.character(1:nr)) {
                          cells <- rbind(colnames(cells), cells)
                      }

                      rows <- apply(x, 1, paste, collapse=' \\tab ')
                      contents <- paste(rows, collapse='\\cr\n')

                      alignment <- paste(rep('l', ncol(cells), collapse=''))

                      latexTag('tabular', alignment, contents)
                  },
                  # TODO: table.row, table.cell, ... ?

                  # MARKING TEXT
                  code.block=function(x, language) {
                      latexTag('preformatted', x)
                  },

                  # TODO \code{link} but markdown is [`asdf`]{} ?
                  code.inline=function (x) {
                      latexTag('code', x)
                  },

                  bold=function(x) {
                      latexTag('strong', x)
                  },

                  italic=function(x) {,
                      latexTag('emph', x)
                  },

                  # MATHS
                  maths.inline=function(latex, ascii=NULL) {
                      latexTag('eqn', latex, ascii)
                  },

                  maths.display=function(latex, ascii=NULL) {
                      latexTag('eqn', latex, ascii)
                  },

                  # IMAGES
                  # TODO: can do \figure{filename}{options: string}
                  image=function(url, caption=NULL) {
                      latexTag('figure', url, caption)
                  }
)
