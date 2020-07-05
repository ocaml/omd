## Tests for attributes

Headings with classes:

```````````````````````````````` example
Heading classes attributes testing

# Heading 1 {.class1}

# Heading 2 # {.class2}

# Heading 3 {.class3a .class3b}
.
<p>Heading classes attributes testing</p>
<h1 class="class1">Heading 1</h1>
<h1 class="class2">Heading 2</h1>
<h1 class="class3a class3b">Heading 3</h1>
````````````````````````````````

Headings with attributes:

```````````````````````````````` example
Heading data attributes testing

# Heading 1 {data1=value1}

# Heading 2 # {data2=value2}

# Heading 3 {data1=value3a data2=value3b}
.
<p>Heading data attributes testing</p>
<h1 data1="value1">Heading 1</h1>
<h1 data2="value2">Heading 2</h1>
<h1 data1="value3a" data2="value3b">Heading 3</h1>
````````````````````````````````

Headings with ids:

```````````````````````````````` example
Heading id attributes testing

# Heading 1 {#id1}

# Heading 2 # {#id2}

# Heading 3 {#id3a #id3b}
.
<p>Heading id attributes testing</p>
<h1 id="id1">Heading 1</h1>
<h1 id="id2">Heading 2</h1>
<h1 id="id3b">Heading 3</h1>
````````````````````````````````

Inline code with classes:

```````````````````````````````` example
Code blocks class attributes testing

`Code block 1`{.class1}

`Code block 2`{.class2a .class2b}
.
<p>Code blocks class attributes testing</p>
<p><code class="class1">Code block 1</code></p>
<p><code class="class2a class2b">Code block 2</code></p>
````````````````````````````````

Inline code with attributes:

```````````````````````````````` example
Code blocks data attributes testing

`Code block 1`{data1=value1}

`Code block 2`{data1=value2a data2=value2b}
.
<p>Code blocks data attributes testing</p>
<p><code data1="value1">Code block 1</code></p>
<p><code data1="value2a" data2="value2b">Code block 2</code></p>
````````````````````````````````

Inline code with ids:

```````````````````````````````` example
Code blocks id attributes testing

`Code block 1`{#id1}

`Code block 2`{#id2a #id2b}
.
<p>Code blocks id attributes testing</p>
<p><code id="id1">Code block 1</code></p>
<p><code id="id2b">Code block 2</code></p>
````````````````````````````````

Code blocks with classes:

```````````````````````````````` example
Code blocks class attributes testing

``` {.class1}
Code block 1
```

``` {.class2.a .class2.b}
Code block 2
```
.
<p>Code blocks class attributes testing</p>
<pre class="class1"><code>Code block 1
</code></pre>
<pre class="class2.a class2.b"><code>Code block 2
</code></pre>
````````````````````````````````

Code blocks with attributes:

```````````````````````````````` example
Code blocks data attributes testing

``` {data1=value1}
Code block 1
```

``` {data1=value2.a data2=value2.b}
Code block 2
```
.
<p>Code blocks data attributes testing</p>
<pre data1="value1"><code>Code block 1
</code></pre>
<pre data1="value2.a" data2="value2.b"><code>Code block 2
</code></pre>
````````````````````````````````

Code blocks with ids:

```````````````````````````````` example
Code blocks id attributes testing

``` {#id1}
Code block 1
```

``` {#id2.a #id2.b}
Code block 2
```
.
<p>Code blocks id attributes testing</p>
<pre id="id1"><code>Code block 1
</code></pre>
<pre id="id2.b"><code>Code block 2
</code></pre>
````````````````````````````````

Links with classes:

```````````````````````````````` example
Link class attributes testing

[Link 1](url_1){.class1}

[Link 2](url_2){.class2a .class2b}

![Link 1](url_1){.class1}

![Link 2](url_2){.class2a .class2b}
.
<p>Link class attributes testing</p>
<p><a href="url_1" class="class1">Link 1</a></p>
<p><a href="url_2" class="class2a class2b">Link 2</a></p>
<p><img src="url_1" alt="Link 1" class="class1" /></p>
<p><img src="url_2" alt="Link 2" class="class2a class2b" /></p>
````````````````````````````````

Links with attributes:

```````````````````````````````` example
Link data attributes testing

[Link 1](url_1){data1=value1}

[Link 2](url_2){data1=value2a data2=value2b}

![Link 1](url_1){data1=value1}

![Link 2](url_2){data1=value2a data2=value2b}
.
<p>Link data attributes testing</p>
<p><a href="url_1" data1="value1">Link 1</a></p>
<p><a href="url_2" data1="value2a" data2="value2b">Link 2</a></p>
<p><img src="url_1" alt="Link 1" data1="value1" /></p>
<p><img src="url_2" alt="Link 2" data1="value2a" data2="value2b" /></p>
````````````````````````````````

Links with ids:

```````````````````````````````` example
Link id attributes testing

[Link 1](url_1){#id1}

[Link 2](url_2){#id2a #id2b}

![Link 1](url_1){#id1}

![Link 2](url_2){#id2a #id2b}
.
<p>Link id attributes testing</p>
<p><a href="url_1" id="id1">Link 1</a></p>
<p><a href="url_2" id="id2b">Link 2</a></p>
<p><img src="url_1" alt="Link 1" id="id1" /></p>
<p><img src="url_2" alt="Link 2" id="id2b" /></p>
````````````````````````````````

Refs with classes:

```````````````````````````````` example
Ref class attributes testing

[Ref 1][ref_1]

[Ref 2][ref_2]

![Ref 3][ref_3]

![Ref 4][ref_4]

[ref_1]: ref_1 {.class1}
[ref_2]: ref_2 {.class2a .class2b}
[ref_3]: ref_3 {.class1}
[ref_4]: ref_4 {.class2a .class2b}
.
<p>Ref class attributes testing</p>
<p><a href="ref_1" class="class1">Ref 1</a></p>
<p><a href="ref_2" class="class2a class2b">Ref 2</a></p>
<p><img src="ref_3" alt="Ref 3" class="class1" /></p>
<p><img src="ref_4" alt="Ref 4" class="class2a class2b" /></p>
````````````````````````````````

Refs with attributes:

```````````````````````````````` example
Ref data attributes testing

[Ref 1][ref_1]

[Ref 2][ref_2]

![Ref 3][ref_3]

![Ref 4][ref_4]

[ref_1]: ref_1 {data1=value1}
[ref_2]: ref_2 {data1=value2a data2=value2b}
[ref_3]: ref_3 {data1=value1}
[ref_4]: ref_4 {data1=value2a data2=value2b}
.
<p>Ref data attributes testing</p>
<p><a href="ref_1" data1="value1">Ref 1</a></p>
<p><a href="ref_2" data1="value2a" data2="value2b">Ref 2</a></p>
<p><img src="ref_3" alt="Ref 3" data1="value1" /></p>
<p><img src="ref_4" alt="Ref 4" data1="value2a" data2="value2b" /></p>
````````````````````````````````

Refs with ids:

```````````````````````````````` example
Ref id attributes testing

[Ref 1][ref_1]

[Ref 2][ref_2]

![Ref 3][ref_3]

![Ref 4][ref_4]

[ref_1]: ref_1 {#id1}
[ref_2]: ref_2 {#id2a #id2b}
[ref_3]: ref_3 {#id1}
[ref_4]: ref_4 {#id2a #id2b}
.
<p>Ref id attributes testing</p>
<p><a href="ref_1" id="id1">Ref 1</a></p>
<p><a href="ref_2" id="id2b">Ref 2</a></p>
<p><img src="ref_3" alt="Ref 3" id="id1" /></p>
<p><img src="ref_4" alt="Ref 4" id="id2b" /></p>
````````````````````````````````
