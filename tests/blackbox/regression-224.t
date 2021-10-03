Regression test for https://github.com/ocaml/omd/issues/224

  $ omd << "MD"
  > hello_world:
  > ```
  > is_this_code_
  > ```
  > MD
  <p>hello_world:</p>
  <pre><code>is_this_code_
  </code></pre>
