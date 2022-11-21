let show x = Omd.to_html x |> print_string

let%expect_test "construct inline elements" =
  show
    Omd.Ctor.
      [ p
          [ em "emphasized"
          ; br
          ; strong ~attrs:[ ("class", "my-class") ] "strong"
          ; nl
          ; code "some code"
          ; nl
          ; a "label" ~url:"my/page/url"
          ; nl
          ; a "other label" ~url:"my/other/page" ~title:"title text"
          ; nl
          ; img "my/img/src" ~alt:"Some alt text"
          ; nl
          ; img "my/img/src" ~alt:"Some alt text" ~title:"some title"
          ; nl
          ; html "<em>inline html <!-- with a comment! --> here</em>"
          ]
      ];
  [%expect
    {|
    <p><em>emphasized</em><br />
    <strong class="my-class">strong</strong>
    <code>some code</code>
    <a href="my/page/url">label</a>
    <a href="my/other/page" title="title text">other label</a>
    <img src="my/img/src" alt="Some alt text" />
    <img src="my/img/src" alt="Some alt text" title="some title" />
    <em>inline html <!-- with a comment! --> here</em></p> |}]

let%expect_test "construct headings" =
  show Omd.Ctor.[ h 1 ~attrs:[ ("class", "my-class") ] [ txt "Heading 1" ] ];
  [%expect {| <h1 id="heading-1" class="my-class">Heading 1</h1> |}];
  show Omd.Ctor.[ h 6 [ txt "Heading 6"; em "with emphasis!" ] ];
  [%expect
    {| <h6 id="heading-6with-emphasis">Heading 6<em>with emphasis!</em></h6> |}]

let%expect_test "construct lists" =
  show
    Omd.Ctor.
      [ ul
          ~spacing:Tight
          [ [ p [ txt "Item 1" ] ]
          ; [ p [ txt "Item 2" ] ]
          ; [ p [ txt "Item 3"; strong "with strength!" ] ]
          ]
      ];
  [%expect
    {|
    <ul>
    <li>Item 1
    </li>
    <li>Item 2
    </li>
    <li>Item 3<strong>with strength!</strong>
    </li>
    </ul> |}];
  show
    Omd.Ctor.
      [ ol
          [ [ p [ txt "Item 1" ] ]
          ; [ p [ txt "Item 2" ] ]
          ; [ p [ txt "Item 3" ] ]
          ]
      ];
  [%expect
    {|
    <ol>
    <li>
    <p>Item 1</p>
    </li>
    <li>
    <p>Item 2</p>
    </li>
    <li>
    <p>Item 3</p>
    </li>
    </ol> |}]

let%expect_test "construct paragraphs and blockquotes with hrs" =
  let para =
    Omd.Ctor.(
      p
        ~attrs:[ ("class", "my-para") ]
        [ txt "Contet of"; em "this"; txt "paragraph" ])
  in
  show
    Omd.Ctor.
      [ blockquote [ para; hr; p [ txt "Content of second paragraph" ] ] ];
  [%expect
    {|
    <blockquote>
    <p class="my-para">Contet of<em>this</em>paragraph</p>
    <hr />
    <p>Content of second paragraph</p>
    </blockquote> |}]

let%expect_test "construct code blocks" =
  show
    Omd.Ctor.
      [ code_bl
          ~attrs:[ ("class", "my-code") ]
          ~lang:"ocaml"
          "let foo = bar + bing"
      ];
  [%expect
    {| <pre class="my-code"><code class="language-ocaml">let foo = bar + bing</code></pre> |}]

let%expect_test "construct html blocks" =
  show
    Omd.Ctor.
      [ html_bl "<p><em>Some</em> inline HTML <!-- With a comment --> here</p>"
      ];
  [%expect {| <p><em>Some</em> inline HTML <!-- With a comment --> here</p> |}]

let%expect_test "construct definition list" =
  show
    Omd.Ctor.
      [ dl
          [ { term = [ txt "def term 1" ]
            ; defs =
                [ [ txt "definition 1.1" ]
                ; [ txt "definition 1.2" ]
                ; [ txt "definition 1.3" ]
                ]
            }
          ; { term = [ txt "def term 2" ]
            ; defs =
                [ [ txt "definition 2.1" ]
                ; [ txt "definition 2.2" ]
                ; [ txt "definition 2.3" ]
                ]
            }
          ]
      ];
  [%expect
    {|
    <dl><dt>def term 1</dt>
    <dd>definition 1.1</dd>
    <dd>definition 1.2</dd>
    <dd>definition 1.3</dd>
    <dt>def term 2</dt>
    <dd>definition 2.1</dd>
    <dd>definition 2.2</dd>
    <dd>definition 2.3</dd>
    </dl> |}]
