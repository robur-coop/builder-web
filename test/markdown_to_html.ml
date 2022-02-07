let markdown_to_html = Builder_web__Utils.Omd.html_of_string

let test_simple () =
  let markdown = {|# Hello world|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "simple html" "<h1>Hello world</h1>\n" html)

let test_html_script () =
  let markdown = {|# <script>Hello world</script>|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html script header" "<h1>Hello world</h1>\n" html)

let test_preserve_span_content () =
  let markdown = {|* <span id="myref">My ref</span>
* [See my ref](#myref) for more information|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html span content preserved"
              {|<ul>
<li>My ref
</li>
<li>See my ref for more information
</li>
</ul>
|}
              html)

let test_remove_script () =
  let markdown = {|<script>alert(1);</script>|} in
  let html = markdown_to_html markdown in
  Alcotest.(check string "html script removed" "" html)

let test_list_with_html_block_and_markdown () =
  let markdown = "* <div> Hello, World!</div> *this is not html*" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "list with html block and markdown"
              (*"<ul>\n<li><em>this is not html</em>\n</li>\n</ul>\n"*) ""
              html)

let test_list_with_inline_html_and_markdown () =
  let markdown = "* <span> Hello, World!</span> *this is not html*" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "list with html block and markdown"
              "<ul>\n<li> Hello, World! <em>this is not html</em>\n</li>\n</ul>\n"
              html)

let test_absolute_link () =
  let markdown = "[foo](https://foo.com)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute link" "<p><a href=\"https://foo.com\">foo</a></p>\n" html)

let test_relative_link () =
  let markdown = "[foo](../foo.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "relative link" "<p>foo</p>\n" html)

let test_absolute_image () =
  let markdown = "![alttext](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"alttext\" /></p>\n" html)

let test_absolute_image_no_alt () =
  let markdown = "![](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"\" /></p>\n" html)

let test_relative_image () =
  let markdown = "![](/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "relative image" "" html)

let test_absolute_image_script_alt () =
  let markdown = "![<script src=\"bla.js\"></script>](https://foo.com/bar.jpg)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "absolute image with script alt text"
              "<p><img src=\"https://foo.com/bar.jpg\" alt=\"\" /></p>\n" html)

let test_fragment_link () =
  let markdown = "[fragment](#fragment)" in
  let html = markdown_to_html markdown in
  Alcotest.(check string "fragment link" "<p>fragment</p>\n" html)

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
]

let () =
  Alcotest.run "Markdown to HTML" [
    "markdown", markdown_tests
  ]
