# should be \preformatted{text blah blah blah \code{...}} NOT fire the block code.
# i.e. should fire profile$preformatted and profile$code.inline
'<pre>
  text blah blah blah
  <code>bit of code in the pre</code>
</pre>'

# should fire JUST profile$code.block
'<pre><code>Some script</code></pre>'

'<pre>
 <code>Some script</code>
</pre>'

# should detect language=R
'<pre><code language="R">asdf</code></pre>
