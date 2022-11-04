## Additional Table Tests

Complete table

```````````````````````````````` example
| abc | def   | **ghi**   |
|:----|:-----:|----------:|
| 1   | 2     | [link][0] |
| 3   | 4     | `code`    |
| 5   | `6`   | \| `|`    |

[0]: https://example.com
.
<table>
<thead>
<tr>
<th align="left">abc</th>
<th align="center">def</th>
<th align="right"><strong>ghi</strong></th>
</tr>
</thead>
<tbody>
<tr>
<td align="left">1</td>
<td align="center">2</td>
<td align="right"><a href="https://example.com">link</a></td>
</tr>
<tr>
<td align="left">3</td>
<td align="center">4</td>
<td align="right"><code>code</code></td>
</tr>
<tr>
<td align="left">5</td>
<td align="center"><code>6</code></td>
<td align="right">| <code>|</code></td>
</tr>
</tbody>
</table>
````````````````````````````````


Not a table (no delimiter)

```````````````````````````````` example
| abc |
| def |
.
<p>| abc |
| def |</p>
````````````````````````````````

Too few columns in a row gets expanded

```````````````````````````````` example
| a | b |
|---|---|
| 1 |
.
<table>
<thead>
<tr>
<th>a</th>
<th>b</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td></td>
</tr>
</tbody>
</table>
````````````````````````````````

Table with no columns not allowed

````````````````````````````````example
|
|
.
<p>|
|</p>
````````````````````````````````

Minimal table 1

```````````````````````````````` example
h|
-|
.
<table>
<thead>
<tr>
<th>h</th>
</tr>
</thead>
</table>
````````````````````````````````

Minimal table 2

```````````````````````````````` example
|h
|-
.
<table>
<thead>
<tr>
<th>h</th>
</tr>
</thead>
</table>
````````````````````````````````

Minimal table 3

```````````````````````````````` example
||
||
.
<table>
<thead>
<tr>
<th></th>
</tr>
</thead>
</table>
````````````````````````````````

Escaped `|` characters

```````````````````````````````` example
\||\|
-|-
|
.
<table>
<thead>
<tr>
<th>|</th>
<th>|</th>
</tr>
</thead>
<tbody>
<tr>
<td></td>
<td></td>
</tr>
</tbody>
</table>
````````````````````````````````

`|` characters inside code spans without escaping

```````````````````````````````` example
abc | `|` | def
----|-----|-------------
ghi |     | `` `| ``jkl
.
<table>
<thead>
<tr>
<th>abc</th>
<th><code>|</code></th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>ghi</td>
<td></td>
<td><code>`|</code>jkl</td>
</tr>
</tbody>
</table>
````````````````````````````````

Cells starting with numbers

```````````````````````````````` example
0 | 1
--|--
3 | 4
.
<table>
<thead>
<tr>
<th>0</th>
<th>1</th>
</tr>
</thead>
<tbody>
<tr>
<td>3</td>
<td>4</td>
</tr>
</tbody>
</table>
````````````````````````````````

Setext headings or cells? A setext heading marker isn't a start of a
new block, so it gets treated as if it were a single element row.

```````````````````````````````` example
= | b
--|--
=
.
<table>
<thead>
<tr>
<th>=</th>
<th>b</th>
</tr>
</thead>
<tbody>
<tr>
<td>=</td>
<td></td>
</tr>
</tbody>
</table>
````````````````````````````````

Tables in a list

```````````````````````````````` example
1. abc | def
   ----|----
   1   | 2

2. | abc | def |
   |-----|-----|
   | 1.  | 2.  |
.
<ol>
<li><table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>2</td>
</tr>
</tbody>
</table></li>
<li><table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>1.</td>
<td>2.</td>
</tr>
</tbody>
</table></li>
</ol>
````````````````````````````````
