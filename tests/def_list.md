## Definition lists

```````````````````````````````` example
First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.
which is multiline
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
````````````````````````````````

# With multiple paragraphs

Definition lists support "loose" formatting, in which each definition is wrapped
as a paragraph. This is triggered by including multiple-paragraph definitions,
as in the definition for the `Second Term`, below, but then effects the entire
definition list.

```````````````````````````````` example
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
  the second paragraph

  Note that, currently, subsequent paragraphs cannot lazy wrapped.  For example,
this line will be part of a new paragraph outside of the definition list
entirely.

Fourth term
: > There is a block quote in the 1st definition of the fourth term.
: But not in the second.
.
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
the second paragraph</p>
<p>Note that, currently, subsequent paragraphs cannot lazy wrapped.  For example,</p>
</dd>
</dl>
<p>this line will be part of a new paragraph outside of the definition list
entirely.</p>
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
````````````````````````````````

When multiple definitions are separated by empty lines, this also triggers a
loose formatting:

```````````````````````````````` example
First term
: Definition 1

: Definition 2
.
<dl><dt>First term</dt>
<dd>
<p>Definition 1</p>
</dd>
<dd>
<p>Definition 2</p>
</dd>
</dl>

````````````````````````````````

# Nesting

Definition lists can be nested.

```````````````````````````````` example
First Root Term
: Level one definition.

  L1 Nested Term
  : L2 Definition
    ```
    with some code
    ```

    L2 Nested Term
    : L3 Definition

Second Root Term
: Level one definition for second root.
.
<dl><dt>First Root Term</dt>
<dd>
<p>Level one definition.</p>
<dl><dt>L1 Nested Term</dt>
<dd>
<p>L2 Definition</p>
<pre><code>with some code
</code></pre>
<dl><dt>L2 Nested Term</dt>
<dd>
L3 Definition
</dd>
</dl>
</dd>
</dl>
</dd>
<dt>Second Root Term</dt>
<dd>
<p>Level one definition for second root.</p>
</dd>
</dl>
````````````````````````````````

# Non-definition lists

A heading does not count a defined term:

```````````````````````````````` example
# A heading
: Followed by a line beginning with a `:` is not a definition.
.
<h1>A heading</h1>
<p>: Followed by a line beginning with a <code>:</code> is not a definition.</p>
````````````````````````````````
