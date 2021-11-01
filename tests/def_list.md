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

Third term
: This is one paragraph using lazy wrapping,
so this should be part of the first paragraph
of the third term.

  But this should be the second paragraph of
  the second paragram

  Note that, currently, subsequent paragraphs cannot lazy wrap.
For example, this line will be part of a new paragraph outside
of the definitoin list entirely.

Fourth term
: > There is a block quote in the 1st definition of the fourth term.
: But not in the second.

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

# A heading
: Followed by a line beginning with a `:` is not a definition.

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
<dt>Third term</dt>
<dd>
<p>This is one paragraph using lazy wrapping,
so this should be part of the first paragraph
of the third term.</p>
<p>But this should be the second paragraph of
the second paragram</p>
<p>Note that, currently, subsequent paragraphs cannot lazy wrap.</p>
</dd>
</dl>
<p>For example, this line will be part of a new paragraph outside
of the definitoin list entirely.</p>
<dl><dt>Fourth term</dt>
<dd>
<blockquote>
<p>There is a block quote in the 1st definition of the fourth term.</p>
</blockquote>
</dd>
<dd>
But not in the second.
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
<h1>A heading</h1>
<p>: Followed by a line beginning with a <code>:</code> is not a definition.</p>
````````````````````````````````
