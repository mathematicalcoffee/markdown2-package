# external link with caption
'Here is a [link](http://google.com)'

# external link with markup in the caption
'Here is a [marked *up* `link`](http://google.com)'

# external link with just a code caption, should be \href{http://google.com}{\code{code}}
'Here is some [`code`](http://google.com)'

# INTERNAL link, SHOULD BE \code{\link{}} not \link{code}
# (NOTE: internal links can't have captions separate to the URL??)
# These should all do \link[base]{paste} (OR \link[base:paste](paste) ????)
c('[base:paste]()', '[](base:paste)', '[paste](base:paste)', '[`paste`](base:paste)')
# These should link to paste0 (\link[base:paste]{paste0} ??)
c('[paste0](base:paste)')
# These will NOT link to paste0 but instead to paste
c('???')
