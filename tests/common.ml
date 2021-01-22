let normalize_html s =
  String.trim s
  |> Soup.parse
  |> Soup.pretty_print
