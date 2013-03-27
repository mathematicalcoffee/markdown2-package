# http://cran.r-project.org/doc/manuals/R-exts.html
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
                      profile='Rd', ...) {

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

    XML2(html, profile)
}

XMLTree2 <- function(xml.text, profile) {
    tree <- xmlRoot(xmlTreeParse(xml.text, asText=T, ignoreBlanks=T, trim=F, useInternalNodes=T))
    # "If internal nodes are used and the internal tree returned directly, all the nodes are
    # returned as-is and no attempt to trim white space, remove "empty" nodes, etc.
    # is done."
    # Remove "empty" nodes
    # TODO: DON'T WANT THIS TO BE TRIMMED:
    # A <bold>strange</bold> <em>new</em> world.
    # this space -----------^
    # So just filter all things that do not expect text to remove the whitespace???? TODO
    # only top-level?
    # nodes <- xmlElementsByTagName(tree, 'text', recursive=F)
    # hmmm.....
    # empty <- grep('^\\s+$', sapply(nodes, xmlValue))
    # removeNodes(nodes[empty])
    # ^-------------- the above removes **too much**

    flatten.node(tree, getHandlers(profile))
}

# TODO: .profile ?
#' Recursively flattens an XMLInternalNode out to a string, using the provided
#' handlers.
#' @param node - node to flatten
#' @param handlers - list of handlers, take in a vector of strings, node and index and returns a string.
#' @param index - the index of the current child in its parent.
#' @param ... - futureproofing
#' @internal
flatten.node <- function(node, .handlers, index=-1, ...) {
    # TODO: later XMLTextNode should be removed (only use useInternalNodes=T)
    if (inherits(node, 'XMLInternalTextNode') || inherits(node, 'XMLTextNode')) {
        xmlValue(node)
    } else {
        ch <- xmlChildren(node)
        # TODO: also provide the contents to the next heading?
        # BIG TODO: must provide a way for a node to indicate that another
        #           node should be skipped?
        strings <- sapply(seq_along(ch),
                          function (i) {
                              flatten.node(ch[[i]], .handlers, index=i)
                          })

        tag <- xmlName(node)
        handler <- .handlers[[tag]]
        if (!is.null(handler)) {
            handler(strings, node=node, index=index, ...)
        } else {
            # assume collapse with profile[[]]?
            x <- collapse(strings)
            featName <- XML2Me[[tag]]
            if (!is.null(featName) && !is.null(profile[[featName]])) {
                profile[[featName]](x, node=node, index=index, ...)
            } else {
                # do nothing, just return the flattened string
                x
            }
        }
    }
}

# Math (Ajax): \( \) (inline); $$ and \[ \] (block)
# MathML: <math></math> or <math display="inline"></math> for inline
# and <math display="block"></math>
# 
#
#
# TODO: HTML ENTITIES
Me2XML=list(
)
XML2Me = list(
    # NOTE: mathjax is left as \(\) and \[ \], so no HTML element is made for it
    # TODO: math, form, figure, fieldset, script, noscript, style, iframe, ins
    # ENTITY REPLACEMENT
    p='paragraph',
    br='linebreak',
    blockquote='quote',

    dl='list.definition',
    dt='list.definition.title',
    dd='list.definition.definition',
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

    b='bold',
    bold='bold',
    strong='bold',
    em='italic',

    del='strikethrough',
    sup='superscript', # <-- TODO: do *not* want this? (if inside math environment?)

    img='image' # <--
)

# STUBS TODO LATER
getProfile <- function(profile) {
    .Profiles[[profile]]
}

# TODO: how to save forever
registerProfile <- function(name, handlers) {
    .Profiles[[name]] <- handlers
}

.Profiles <- list()

#' @internal
collapse <- function(..., sep='', collapse='') {
    paste(..., sep='', collapse='')
}

# \name{contents}{optionalcontents}
#' @internal
latexTag <- function(name, contents, optionalcontents=NULL) {
    if (!is.null(optionalcontents)) {
        return(paste0('\\', name, '{', contents, '}{', optionalcontents, '}'))
    }
    return(paste0('\\', name, '{', contents, '}'))
}

#' @internal
is.internal.link <- function(url) {
    # if the url is =dest or package: or package:page or a function name
    # then that is the URL too
    grepl('^(=[\\w.-]+|[\\w.-]+:[\\w.-]*|[\\w.]+)$', url, perl=T)
}

# --- handlers (generic/helpers)
handler.list <- function(list.type.profile) {
    function(node) {
        ch <- xmlChildren(node) # still XML nodes
        children <- sapply(seq_along(ch),
                           function (i) {
                               profile$list.item(flatten(ch[i]), i, list.type=list.type.profile, node=node)
                           })
        children <- unname(children)
        profile[[list.type.profile]](children, node=node)
    }
}

#' checks that `node` is the *only* child node its parent, optionally specifying
#' that the parent node's name be a particular value and that we ignore text
#' node siblings that consist of whitespace only.
isOnlyChild <- function (node, parentName=NULL, index=NULL, ignore.white.space=T) {
    # find all siblings

    parent <- xmlParent(node)
    # Easy case: parent name doesn't match and we require it to
    if (!is.null(parentName) && (is.null(parent) || xmlName(parent) != parentName)) {
        return(FALSE)
    }

    # Easy case: index is known and ignore.white.space=F and index != 1
    if (!ignore.white.space && !is.null(index) && index != 1) {
        return(FALSE)
    }

    # Get all children
    children <- NULL
    if (!is.null(parent)) {
        children <- xmlChildren(parent)
    } else {
        # multiple root nodes with no parent...(probably won't happen for me?)
        ch <- node
        # find siblings before
        while (!is.null((ch <- getSibling(ch, after=F)))) {
            children <- c(ch, children)
        }
        children <- c(children, node)
        # find siblings after
        ch <- node
        while (!is.null((ch <- getSibling(ch, after=T)))) {
            children <- c(children, ch)
        }
    }
    n <- length(children)

    # Easy case: if n == 1, then we are the only child
    if (n == 1) {
        return(TRUE)
    }

    # Easy case: if !ignore.white.space, test if nchildren == 1
    if (!ignore.white.space) {
        return(n == 1)
    }

    # If we got this far ignore.white.space is TRUE and n > 1
    for (ch in children) {
        if (ch == node) next
        # if any of the children (that isn't the original) has non-space value
        # we fail
        if (!grepl('^\\s+$', xmlValue(ch)))
            return(FALSE)
    }
    return(TRUE)
}

# UPTO UPTO (converts tag-level to feature-level functions)
# TODO: MUST REMOVE NODES THAT ARE "\n        " (e.g. in the <li>)
#' This takes a profile and provides a list of functions mapping tag names
#' to features (e.g. 'p' to 'paragraph').
#'
#' TODO: can provide a defaultHandler?
#' The signature of each handler is `(x, node, index, ...)`, where:
#' 
#' * `x` is a vector of strings being the processed contents of the child nodes
#'       (including text and HTML) of the current handler.
#'       Each element of `x` has already been run through its appropriate
#'       profile handler for the feature it represents.
#' * `node` is a XMLInternalNode representing the node for the current tag.
#'          You cannot modify it but you can query it with things like [](XML:xmlParent).
#' * `index` is the index (1-based) of this node within its parent.
#'           This node is the `index`th child of the parent (including text nodes
#'           which may consist of whitespace only (!))
#' * `...` for future-proofing.
#'
#' It is expected that the tag handler will call the appropriate profile/feature
#' handler (the expected signatures for these is in [.signatures]()).
#' 
#' In particular, you should *always* pass named arguments `node` and `index`
#' to the corresponding profile/feature handler, along with `...` (for future-proofing).
#'
#' If a tag is encountered that does NOT have a handler, ....
#' we see if XML2Me has a direct translation of tag to feature, and call that
#' feature's handler in the profile with `x`, `node`, `index`, and `...`.
#'
#' TODO: a customHandlers/overrideHandlers arg?
getHandlers <- function(profile, TODOmappings, TODOextras) {
    handlers <- list()

    handlers$FOR_ALL <- function() {}

    # Map h1, ..., h6 to profile$heading
    # TODO: also provide the contents to the next heading?
    for (i in 1:6) {
        hi <- paste0('h', i)
        handlers[[hi]] <- function(x, node, index, ...) {}
        body(handlers[[hi]]) <- bquote(
            profile$heading(collapse(x), level=.(i), node=node, index=index, ...)
        )
    }

    # the <code> block could be <pre><code> in which case we fire the code.block
    # handler, or it could be use <code> so we fire the code.inline handler,
    # or if it's just <pre> we fire the preformatted handler.
    # At the moment to avoid firing twice we skip <pre><code> in the code handler
    # and fire it from the <pre> handler.
    #
    # TODO: can I handle in <code> and then REMOVE the parent <pre> or will that break?
    # Hmm..Need a way for the child to indicate that the parent should not
    # be handled, perhaps. OR at least some way to mark nodes as not
    #
    # ****************************
    # BIG BIG BIG BIG BIG TODO BREAKING NEWS: I CAN DO xmlValue() <- ASDF
    # as long as I don't do it in xmlTreeParse!!!!!!!!!
    # Why don't we set an attribute ".markdown2-package='skip'" ?
    # Or could just carry around a local variable, a vector skip=c(node, node, ...)
    # ****************************
    #
    # That will break.
    handlers$code <- function(x, node, index, ...) {
        # 1. check if it's <pre><code>. If so, fire profile$code.block
        if (isOnlyChild(node, parentName='pre', index=index)) {
            # skip, we'll trigger profile$code.block in handlers$pre.
            ''
        }
        language <- xmlGetAttr(node, 'class')

        # fire code.inline.
        profile$code.inline(collapse(x), language=language, node=node, index=index, ...)
    }

    handlers$pre <- function(node, ...) {
        language <- xmlGetAttr(node, 'class')
        profile$code.block(collapse(x), language=language, node=node, index=index, ...)
        # TODO: remove profile$preformatted (unless I want to distinguish
        # between preformatted.block and preformatted.code)

        # NOTE TODO: isn't profile$preformattted the same feature as profile$code.block?
        # Otherwise I'd need a profile$preformatted.block and profile$preformatted.inline?
        # BIG TODO: will REMOVE preformatted.
        profile
            ch <- xmlChildren(node)
        # TODO: filter out blank text nodes from the <pre>
        #   V---- i.e. only node-child is a '<code>' with only spaces surrounding it (?)
        if (length(ch) == 1) {
                    profile$code.block(...)
            } else {
                        profile$preformatted(...) # NOTE BIGTODO isn't \preformatted{} just code.block?
                }
    }
    

    # Code
    code.inline=.defaultSig,
    code.block=alist(x=, language=, node=, index=, ...=),

    # lists
    list.item=alist(x=, list.type=, node=, index=, ...=),
    list.unordered=alist(items=, node=, index=, ...=),
    list.ordered=alist(items=, node=, index=, ...=),

    # definition list
    list.definition=alist(terms=, definitions=, node=, index=, ...=),
    list.definition.title=.defaultSig, # TODO: provide the corresponding title/text?
                                       # list.definition.item=alist(term=,text=???)
    list.definition.text=.defaultSig,

    # quotes
    quote.block=.defaultSig,  # <blockquote><p>x</p></blockquote> (TODO: get rid of that <p> so paragraph handler isn't confused?)
    # quote.inline=.defaultSig, # TODO: are there inline quotes?

    # links
    link=alist(url=, caption=, node=, index=, ...=),
    email=.defaultSig,
    image=alist(url=, caption=, node=, index=, ...=),

    # maths
    maths.inline=alist(latex=, ascii=NULL, node=, index=, ...=),
    maths.display=alist(latex=, ascii=NULL, node=, index=, ...=),

    # tables
    # TODO: rows/cells get is.in.header (/is.in.body)?
    # TODO: rows/cells/tables get alignment?
    # TODO: table gets a cells.head and cells.body argument?
    table.cell=alist(x=, is.heading=, in.tbody=, node=, index=, ...=),
    table.row=alist(cells=, in.tbody=, node=, index=, ...=),
    table=alist(rows=, alignment=NULL, node=, index=, ...=)
)
    handlers$a <- function (x, node, ...) {
        caption <- collapse(x)
        url <- xmlGetAttr(node, 'href')
        profile$link(url, caption=caption, node=node, ...)
    }
    handlers$li <- function(x, node, index, ...) {
        x <- collapse(x) # the text for the list item
        parent <- xmlParent(node)
        list.type <- 'ul' # default
        if (is.null(parent)) {
            # Note: since this is from markdown the lists are always well-formed
            # so I wouldn't expect to get a <li> at root level
            warning("List item found not in a list")
        } else {
            list.type <- xmlName(parent)
        }
        list.type <- XML2Me[[list.type]]

        # list.type is 'list.unordered', 'list.ordered', ...
        profile$list.item(x, list.type=list.type, node=node, index=index, ...)
    }

    # pass through without collapsing
    passthroughs <- c('ul', 'ol')
    handlers$ul <- function(...) {
        # pass through without collapsing
        profile$list.unordered(...)
    }
    handlers$ol <- function(...) {
        # pass through without collapsing
        profile$list.ordered(...)
    }
    handlers
}

## TODO: do I really want to modify a *copy* of the tree so subsequent handlers are guaranteed
## to get nodes only?
# function node, text ? function(text, node)?
profile2XMLHandlers <- function(profile) {
    handlers <- list()

    # blockquote, li, br, p, code(inline), strong, italic, strikethrough
    # TOFIX: tr, td, th
    # TODO: superscript? (the thing under the superscript)
    # TODO: li(text, index)?
    handlers$startElement <- function(node) {
        tag <- node$name
        pname <- XML2Me[[tag]]
        txt <- flatten(node)

        # If the handler exists in the profile, we use that with the flattened text.
        # Otherwise, if the tag name is one known to us, we flatten it.
        # If it is unknown to us, we pass it through (????)
        # (For example: dt, dd, ... vs thead, tbody, ... ?)
        if (profile[[pname]]) {
            profile[[pname]](flatten(node), node=node)
        } else {
            toCharacterVector(node)
        }
    }

    # -- Special cases --
    # lists. They fire profile$list.item(text, index, list.type, node) on
    # each child.
    # To do this (since we can't do xmlParent) we explicitly only flatten
    # handlers$li and then fire profile$list.item manually from the parent list.
    # preserve all li as nodes until the
    # TODO: OH, but lists within lists?
    # BIG TODO: If I have <li>a <bold>bold</bold> move,
    #           the li node will NOT be a proper XML node: bits of it will be replaced
    # Options:
    # 1. don't provide ancestry information
    # 2. don't provide the node
    # 3. don't replace each node with a string, but replace the node's *value* or text with
    #    a string, and then do some sort of xmlText(recursive=T) at the end <-- TRY THIS
    #
    # So - *don't* pass in the node?
    handlers$li <- identity
    handlers$ul <- handler.list('list.unordered')
    handlers$ol <- handler.list('list.ordered')
    # definition lists need to know about dd and dt
    handlers$dd <- identity
    handlers$dt <- identity
    handlers$dl <- function (node) {
        # UPTO
    }

    # link
    handlers$a <- function(node) {
        caption <- flatten(node)
        url <- xmlGetAttr(node, 'href')
        profile$link(url, caption=caption, node=node)
    }

    # code. NOTE:
    # pre-parse the XML? replace <pre><code> with <blockcode> # <-- this requires two parses!
    # - code *blocks* are <pre><code>.
    # - inline code is <code>
    # - preformatted text is <pre>
    handlers$code <- function(node) {
        language <- xmlGetAttr(node, 'class')
        # TODO: cannot get parent unless we useInternalNodes=T which doesn't seem
        # to work (the .Call returns NULL, probably beclause I'm replacing internal
        # nodes with R strings)
        # parent <- xmlParent(node)
        if (parent && parent$name == 'pre') {
            # <pre><code>: BLOCK CODE
            profile$code.block(xmlValue(node), language=language, node=node)
        } else {
            # INLINE CODE
            profile$code.inline(xmlValue(node), language=language, node=node)
        }
    }

    # TODO
    handlers$pre <- function(node) {
        if (is.character(xmlChildren(node)[[1]])) {
        } else {
            # pre all on its own...
            profile$code.block(xmlValue(node), language=language, node=node)
        }
        # code block (language)
    }


    # images
    handlers$img <- function (node) {
        caption <- xmlGetAttr(node, 'alt', default=xmlGetAttr(node, 'title'))
        url <- xmlGetAttr(node, 'src')
        profile$image(url, caption=caption, node=node)
    }

    # tables
    # TODO: tr, td, th supply coordinates
    # (for now: td/th should flatten)
    # TODO: could suppress tr/td (ie do nothing) and then handle in table?
    # restrict to where profile$[asdf] exists.
    handlersInProfile <- names(profile)
    handlersInXML <- Me2XML[handlersInProfile]
    handlers <- handlers[handlersInXML]

    handlers
}

# TODO: provide signatures?!
# formals(myHandler) <- .supportedFeatures('bold')?
# TODO: rename 'x' to 'text'?
# .signatures are for reference
.defaultSig <- alist(x=, node=, index=, ...=)

.signatures <- list(
    # Headings, paragraphs
    paragraph=.defaultSig,
    heading=alist(x=, level=, node=, index=, ...=), # TODO: contents to next heading
    linebreak=alist(node=, index=, ...=),

    # Text decoration
    bold=.defaultSig,
    italic=.defaultSig,
    strikethrough=.defaultSig,

    # Code
    code.inline=.defaultSig,
    code.block=alist(x=, language=, node=, index=, ...=),

    # lists
    list.item=alist(x=, list.type=, node=, index=, ...=),
    list.unordered=alist(items=, node=, index=, ...=),
    list.ordered=alist(items=, node=, index=, ...=),

    # definition list
    list.definition=alist(terms=, definitions=, node=, index=, ...=),
    list.definition.title=.defaultSig, # TODO: provide the corresponding title/text?
                                       # list.definition.item=alist(term=,text=???)
    list.definition.text=.defaultSig,

    # quotes
    quote.block=.defaultSig,  # <blockquote><p>x</p></blockquote> (TODO: get rid of that <p> so paragraph handler isn't confused?)
    # quote.inline=.defaultSig, # TODO: are there inline quotes?

    # links
    link=alist(url=, caption=, node=, index=, ...=),
    email=.defaultSig,
    image=alist(url=, caption=, node=, index=, ...=),

    # maths
    maths.inline=alist(latex=, ascii=NULL, node=, index=, ...=),
    maths.display=alist(latex=, ascii=NULL, node=, index=, ...=),

    # tables
    # TODO: rows/cells get is.in.header (/is.in.body)?
    # TODO: rows/cells/tables get alignment?
    # TODO: table gets a cells.head and cells.body argument?
    table.cell=alist(x=, is.heading=, in.tbody=, node=, index=, ...=),
    table.row=alist(cells=, in.tbody=, node=, index=, ...=),
    table=alist(rows=, alignment=NULL, node=, index=, ...=)
)
.supportedFeatures <- names(.signatures)
# TODO: SOME NODES ARE node-only (no text) and we MUST filter out all child nodes of spaces
# e.g. <ul> or <table> otherwise we end up with more children than intended.

# TODO: \pkg for linking to a package, \email, check if we can do \strikethorugh in the terminal
#' Generates the Rd profile, should be cached after that.
#' @internal
.makeRdProfile <- function() {
    prof <- list()
    # skip quotes

    # ----- Headings/paragraphs ----- #
    # skip paragraph
    prof$heading <- function(x, level, ...) {
        # TODO: GET CONTENTS (getSibling...) AND CANCEL THE PARSING OF THE
        # SIBLING WHEN WE ARE DONE
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
  
        #if (tagName == 'unknown') {
        #    if (level == 1) {
        #        return(latexTag('section', text, contents))
        #    } else {
        #        return(latexTag('subsection', text, contents))
        #    }
        #}
        #latexTag(tagName, contents)
        # TODO: NEED TO ALSO INCLUDE CONTENTS HERE
        x
    }

    prof$linebreak <- function(...) {
        '\\cr'
    }

    # ----- text formatting ----- #
    prof$bold <- function(x, ...) {
        latexTag('strong', x)
    }
    prof$italic <- function(x, ...) {
        latexTag('emph', x)
    }
    # skip strikethrough

    # ----- code ----- #
    prof$code.inline <- function(x, ...) {
        latexTag('code', x)
    }

    prof$code.block <- function(x) {
        latexTag('preformatted', x) # TODO: preformatted(code) ?
    }

    # ----- ordered/unordered lists ----- #
    prof$list.item <- function(x, list.type, node, index, ...) {
        paste('\\item', x)
    }
    prof$list.unordered <- function(items, ...) {
        latexTag('itemize', collapse(items))
    }
    prof$list.ordered <- function(items, ...) {
        latexTag('enumerate', collapse(items))
    }

    # ----- definition lists ----- #
    prof$list.definition <- function(terms, definitions, ...) {
        items <- paste0('\\item{', definitions, '}{', items, '}')
        latexTag('describe', items)
    }

    prof$list.definition.title <- function(x, ...) {
        latexTag('strong', x) # bold the title
    }
    # skip list.definition.text

    # ----- links ----- #
    # \link{name}                 [name](name)
    # \link[=dest]{name}          [name](=dest)
    # \link[package]{name}        [name](package:)
    # \link[package:page]{name}   [name](package:page)
    # Note: for internal links, there are NO CAPTIONS.
    # (But if the user specifies [asdf]() we will treat as internal)
    prof$link <- function(url, caption=NULL, ...) {
        # If they did [asdf]() copy over the caption to the url
        if (!nchar(url) && !is.null(caption) && nchar(caption))
            url <- caption

        if (!is.internal.link(url)) {
            # external link
            if (!is.null(caption) && nchar(caption)) {
                latexTag('href', url, caption)
            } else {
                latexTag('url', url)
            }
        } else {
            tName <- 'link'
            if (caption != url) {
                tName <- paste0(tName, '[', url, ']')
            }
            latexTag(tname, caption)
        }
    }

    prof$email <- function(x, ...) {
        latexTag('email', x)
    }

    prof$image <- function(url, caption=NULL, ...) {
        latexTag('figure', url, caption)
    }

    # ----- maths ----- #
    prof$maths.inline <- function(latex, ascii=NULL, ...) {
        latexTag('eqn', latex, ascii)
    }
    prof$maths.display <- prof$maths.inline # TODO check that it's display

    # ----- tables ----- #
    # TODO: does `table` get table.rows or individual cells or does it depend
    # on what you return in the handlers?
    prof$table.cell <- function(x, is.heading, in.tbody, ...) {
        # bold it if it's in the heading....
        if (is.heading)
            x <- latexTag('strong', x)
        x
    }
    prof$table.row <- function(cells, in.tbody, ...) {
        collapse(cells, collapse=' \\tab ')
    }
    prof$table <- function(rows, ncols, alignment=NULL, ...) {
        # BIG TODO: NEED THE NUMBER OF COLUMNS
        # assume x is rows...
        flat <- collapse(rows, sep=' \\cr')

        if (is.null(alignment)) {
            alignment <- 'l'
        }
        alignment <- collapse(rep(alignment, length.out=ncols))
        latexTag('tabular', alignment, flat)
    }

    prof
}

RdProfile <- .makeRdProfile()
