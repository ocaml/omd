## Definition lists

```````````````````````````````` example
First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.
which is multiline

: This is not a correct definition list

# With multiple paragraphs

First Term
: This is the definition of the first term.

Second Term
: This is one paragraph of the second term.

  This is another paragraph of the second term.

# Nesting

Root
: Level one

  Nested
  : Level two
    ```
    code
    ```

    More
    : Level three

Root again
: Level one again

.
<dl><dt>First Term</dt>
<dd>
This is the definition of the first term.
</dd>
<dt>Second Term</dt>
<dd>
This is one definition of the second term.
</dd>
<dd>
This is another definition of the second term.
which is multiline
</dd>
</dl>
<p>: This is not a correct definition list</p>
<h1>With multiple paragraphs</h1>
<dl><dt>First Term</dt>
<dd>
<p>This is the definition of the first term.</p>
</dd>
<dt>Second Term</dt>
<dd>
<p>This is one paragraph of the second term.</p>
<p>This is another paragraph of the second term.</p>
</dd>
</dl>
<h1>Nesting</h1>
<dl><dt>Root</dt>
<dd>
<p>Level one</p>
<dl><dt>Nested</dt>
<dd>
<p>Level two</p>
<pre><code>code
</code></pre>
<dl><dt>More</dt>
<dd>
Level three
</dd>
</dl>
</dd>
</dl>
</dd>
<dt>Root again</dt>
<dd>
<p>Level one again</p>
</dd>
</dl>
````````````````````````````````
