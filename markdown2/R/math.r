# mathjax:
# does not scan tags 'script', 'noscript', 'style', 'textarea', 'pre', 'code' for delimiters
# find delimiters: \( \) for inline, \[ \] for display (the markdown package converts $$ $$ to \[ \] I think)
#
# this.start = regex for the start delimiters
#
# recursive down through all XML nodes (not those in skipTags):
#  * if the element is a text node:
#    1 if the element matches the start pattern, store position and look for the end pattern
#    2 if we then find the end pattern:
#          (?) if this is the first time we've found the start/end, do encloseMath(element)
#      and REPEAT.
#
# I'M CONFUSED. I think there's some fanciness to ensure there's the same number
# of opening and closing delimiters.
#
# Also something about skipping over internal br and comment nodes so:
# \[
#   some maths here <!-- comment -->
# \]
# which ends up being 3 nodes (stuff before comment, comment, stuff after comment)
# still matches.
# (we need to skip over such nodes while looking for the delimiter)
# I think we store the start/end nodes so that we can encloseMath better.
#
# encloseMath:
# Loop over all nodes that make up the math (comment nodes, etc) and concatenate
#  into one. Remove those other nodes.
# Enclose the maths with:
# <script type="math/tex; mode=display" or "math/tex;"></script>
# done.

# unpacked/extensions/tex2jax.js
