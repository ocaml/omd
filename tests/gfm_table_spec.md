*Note on the Omd implementation:* Table specification excerpted from
[this commit][0] in the GitHub-Flavored Markdown repository. There is
one alteration (noted below), where the GFM spec appears to contradict
the CommonMark spec (and itself) on the treatment of escape characters
in code spans.

[0]: https://github.com/github/cmark-gfm/blob/6a6e335709ef68cf2c616eeaf61b09ed4c654669/test/spec.txt

## Tables (extension)

GFM enables the `table` extension, where an additional leaf block type is
available.

A [table](@) is an arrangement of data with rows and columns, consisting of a
single header row, a [delimiter row] separating the header from the data, and
zero or more data rows.

Each row consists of cells containing arbitrary text, in which [inlines] are
parsed, separated by pipes (`|`).  A leading and trailing pipe is also
recommended for clarity of reading, and if there's otherwise parsing ambiguity.
Spaces between pipes and cell content are trimmed.  Block-level elements cannot
be inserted in a table.

The [delimiter row](@) consists of cells whose only content are hyphens (`-`),
and optionally, a leading or trailing colon (`:`), or both, to indicate left,
right, or center alignment respectively.

```````````````````````````````` example table
| foo | bar |
| --- | --- |
| baz | bim |
.
<table>
<thead>
<tr>
<th>foo</th>
<th>bar</th>
</tr>
</thead>
<tbody>
<tr>
<td>baz</td>
<td>bim</td>
</tr>
</tbody>
</table>
````````````````````````````````

Cells in one column don't need to match length, though it's easier to read if
they are. Likewise, use of leading and trailing pipes may be inconsistent:

```````````````````````````````` example table
| abc | defghi |
:-: | -----------:
bar | baz
.
<table>
<thead>
<tr>
<th align="center">abc</th>
<th align="right">defghi</th>
</tr>
</thead>
<tbody>
<tr>
<td align="center">bar</td>
<td align="right">baz</td>
</tr>
</tbody>
</table>
````````````````````````````````

Include a pipe in a cell's content by escaping it, including inside other
inline spans:

**Omd alteration**: the escape `\` in the code span is preserved here,
in accordance with the rules for code spans in the CommonMark and GFM
specs.

```````````````````````````````` example table
| f\|oo  |
| ------ |
| b `\|` az |
| b **\|** im |
.
<table>
<thead>
<tr>
<th>f|oo</th>
</tr>
</thead>
<tbody>
<tr>
<td>b <code>\|</code> az</td>
</tr>
<tr>
<td>b <strong>|</strong> im</td>
</tr>
</tbody>
</table>
````````````````````````````````

The table is broken at the first empty line, or beginning of another
block-level structure:

```````````````````````````````` example table
| abc | def |
| --- | --- |
| bar | baz |
> bar
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
<blockquote>
<p>bar</p>
</blockquote>
````````````````````````````````

```````````````````````````````` example table
| abc | def |
| --- | --- |
| bar | baz |
bar

bar
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
<tr>
<td>bar</td>
<td></td>
</tr>
</tbody>
</table>
<p>bar</p>
````````````````````````````````

The header row must match the [delimiter row] in the number of cells.  If not,
a table will not be recognized:

```````````````````````````````` example table
| abc | def |
| --- |
| bar |
.
<p>| abc | def |
| --- |
| bar |</p>
````````````````````````````````

The remainder of the table's rows may vary in the number of cells.  If there
are a number of cells fewer than the number of cells in the header row, empty
cells are inserted.  If there are greater, the excess is ignored:

```````````````````````````````` example table
| abc | def |
| --- | --- |
| bar |
| bar | baz | boo |
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td></td>
</tr>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
````````````````````````````````

If there are no rows in the body, no `<tbody>` is generated in HTML output:

```````````````````````````````` example table
| abc | def |
| --- | --- |
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
</table>
````````````````````````````````
