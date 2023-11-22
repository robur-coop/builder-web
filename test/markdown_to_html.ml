let markdown_to_html = Builder_web__Utils.md_to_html

let test_simple () =
  let markdown = {|# Hello world|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "simple html" "<h1 id=\"hello-world\"><a class=\"anchor\" aria-hidden=\"true\" href=\"#hello-world\"></a>Hello world</h1>\n" html)

let test_html_script () =
  let markdown = {|# <script>Hello world</script>|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html script header" "<h1 id=\"hello-world\"><a class=\"anchor\" aria-hidden=\"true\" href=\"#hello-world\"></a><!-- CommonMark raw HTML omitted -->Hello world<!-- CommonMark raw HTML omitted --></h1>\n" html)

let test_preserve_span_content () =
  let markdown = {|* <span id="myref">My ref</span>
* [See my ref](#myref) for more information|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html span content preserved"
              {|<ul>
<li><!-- CommonMark raw HTML omitted -->My ref<!-- CommonMark raw HTML omitted --></li>
<li><a href="#myref">See my ref</a> for more information</li>
</ul>
|}
              html)

let test_remove_script () =
  let markdown = {|<script>alert(1);</script>|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html script removed" "<!-- CommonMark HTML block omitted -->\n" html)

let test_list_with_html_block_and_markdown () =
  let markdown = "* <div> Hello, World!</div> *this is not html*" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "list with html block and markdown"
              (*"<ul>\n<li><em>this is not html</em>\n</li>\n</ul>\n"*)
              "<ul>\n<li>\n<!-- CommonMark HTML block omitted -->\n</li>\n</ul>\n"
              html)

let test_list_with_inline_html_and_markdown () =
  let markdown = "* <span> Hello, World!</span> *this is not html*" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "list with html block and markdown"
              "<ul>\n<li><!-- CommonMark raw HTML omitted --> Hello, World!<!-- CommonMark raw HTML omitted --> <em>this is not html</em></li>\n</ul>\n"
              html)

let test_absolute_link () =
  let markdown = "[foo](https://foo.com)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute link" "<p><a href=\"https://foo.com\">foo</a></p>\n" html)

let test_relative_link () =
  let markdown = "[foo](../foo.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "relative link" "<p><a href=\"../foo.jpg\">foo</a></p>\n" html)

let test_absolute_image () =
  let markdown = "![alttext](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"alttext\" ></p>\n" html)

let test_absolute_image_no_alt () =
  let markdown = "![](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"\" ></p>\n" html)

let test_relative_image () =
  let markdown = "![](/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "relative image" "<p><img src=\"/bar.jpg\" alt=\"\" ></p>\n" html)

let test_absolute_image_script_alt () =
  let markdown = "![<script src=\"bla.js\"></script>](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image with script alt text"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"\" ></p>\n" html)

let test_fragment_link () =
  let markdown = "[fragment](#fragment)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "fragment link" "<p><a href=\"#fragment\">fragment</a></p>\n" html)

let test_heading_adjustment () =
  let markdown = {|# foo
## bar
# baz
## bazbar
### bazbarbar
#### bazbarbarbar
##### bazbarbarbarbar
###### bazbarbarbarbarbar
|}
  in
  let html = markdown_to_html ~adjust_heading:2 markdown in
  (* NB: the maximum heading is 6 in cmarkit, thus we reduce the structure *)
  let exp = {|<h3 id="foo"><a class="anchor" aria-hidden="true" href="#foo"></a>foo</h3>
<h4 id="bar"><a class="anchor" aria-hidden="true" href="#bar"></a>bar</h4>
<h3 id="baz"><a class="anchor" aria-hidden="true" href="#baz"></a>baz</h3>
<h4 id="bazbar"><a class="anchor" aria-hidden="true" href="#bazbar"></a>bazbar</h4>
<h5 id="bazbarbar"><a class="anchor" aria-hidden="true" href="#bazbarbar"></a>bazbarbar</h5>
<h6 id="bazbarbarbar"><a class="anchor" aria-hidden="true" href="#bazbarbarbar"></a>bazbarbarbar</h6>
<h6 id="bazbarbarbarbar"><a class="anchor" aria-hidden="true" href="#bazbarbarbarbar"></a>bazbarbarbarbar</h6>
<h6 id="bazbarbarbarbarbar"><a class="anchor" aria-hidden="true" href="#bazbarbarbarbarbar"></a>bazbarbarbarbarbar</h6>
|} in
  Alcotest.(check string "header adjustment works fine" exp html)

let test_table () =
  let markdown = {__|| a           | | b         |  c            | d | e |
| --------------------- |-| -------------- | -------------- | --------------- | ------ |
| entry         | | **bla.file** | **other.file**     |                 |        |
| _another entry_  | | **another.file** | **another.other** |                 |        |
|__}
  in
  let html = markdown_to_html ~adjust_heading:2 markdown in
  (* NB: the maximum heading is 6 in cmarkit, thus we reduce the structure *)
  let exp = {|<div role="region"><table>
<tr>
<th>a</th>
<th>b</th>
<th>c</th>
<th>d</th>
<th>e</th>
<th></th>
</tr>
<tr>
<td>entry</td>
<td><strong>bla.file</strong></td>
<td><strong>other.file</strong></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td><em>another entry</em></td>
<td><strong>another.file</strong></td>
<td><strong>another.other</strong></td>
<td></td>
<td></td>
<td></td>
</tr>
</table></div>|} in
  Alcotest.(check string "table is rendered as html" exp html)

let markdown_tests = [
  Alcotest.test_case "Simple" `Quick test_simple;
  Alcotest.test_case "script header" `Quick test_html_script;
  Alcotest.test_case "preserve span content" `Quick test_preserve_span_content;
  Alcotest.test_case "Remove script" `Quick test_remove_script;
  Alcotest.test_case "List with html block and markdown" `Quick test_list_with_html_block_and_markdown;
  Alcotest.test_case "List with inline html and markdown" `Quick test_list_with_inline_html_and_markdown;
  Alcotest.test_case "absolute link" `Quick test_absolute_link;
  Alcotest.test_case "relative link" `Quick test_relative_link;
  Alcotest.test_case "absolute image" `Quick test_absolute_image;
  Alcotest.test_case "absolute image no alt" `Quick test_absolute_image_no_alt;
  Alcotest.test_case "relative image" `Quick test_relative_image;
  Alcotest.test_case "absolute image with script alt" `Quick test_absolute_image_script_alt;
  Alcotest.test_case "fragment link" `Quick test_fragment_link;
  Alcotest.test_case "heading adjustment" `Quick test_heading_adjustment;
  Alcotest.test_case "table" `Quick test_table;
]

let () =
  Alcotest.run "Markdown to HTML" [
    "markdown", markdown_tests
  ]
