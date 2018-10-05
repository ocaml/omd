(rule
  (targets anchors-by-reference.html.out)
  (deps anchors-by-reference.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff anchors-by-reference.html anchors-by-reference.html.out)))

(rule
  (targets automatic-anchors.html.out)
  (deps automatic-anchors.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff automatic-anchors.html automatic-anchors.html.out)))

(rule
  (targets blockquote-nested-markdown.html.out)
  (deps blockquote-nested-markdown.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquote-nested-markdown.html blockquote-nested-markdown.html.out)))

(rule
  (targets blockquote-starting-with-empty-lines.html.out)
  (deps blockquote-starting-with-empty-lines.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquote-starting-with-empty-lines.html blockquote-starting-with-empty-lines.html.out)))

(rule
  (targets blockquote.html.out)
  (deps blockquote.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquote.html blockquote.html.out)))

(rule
  (targets blockquotes-with-empty-lines-inbetween-2.html.out)
  (deps blockquotes-with-empty-lines-inbetween-2.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquotes-with-empty-lines-inbetween-2.html blockquotes-with-empty-lines-inbetween-2.html.out)))

(rule
  (targets blockquotes-with-empty-lines-inbetween-3.html.out)
  (deps blockquotes-with-empty-lines-inbetween-3.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquotes-with-empty-lines-inbetween-3.html blockquotes-with-empty-lines-inbetween-3.html.out)))

(rule
  (targets blockquotes-with-empty-lines-inbetween-4.html.out)
  (deps blockquotes-with-empty-lines-inbetween-4.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquotes-with-empty-lines-inbetween-4.html blockquotes-with-empty-lines-inbetween-4.html.out)))

(rule
  (targets blockquotes-with-empty-lines-inbetween.html.out)
  (deps blockquotes-with-empty-lines-inbetween.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff blockquotes-with-empty-lines-inbetween.html blockquotes-with-empty-lines-inbetween.html.out)))

(rule
  (targets code-block-html-escape.html.out)
  (deps code-block-html-escape.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff code-block-html-escape.html code-block-html-escape.html.out)))

(rule
  (targets code-block.html.out)
  (deps code-block.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff code-block.html code-block.html.out)))

(rule
  (targets dfb-3def.html.out)
  (deps dfb-3def.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-3def.html dfb-3def.html.out)))

(rule
  (targets dfb-4lt5.html.out)
  (deps dfb-4lt5.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-4lt5.html dfb-4lt5.html.out)))

(rule
  (targets dfb-at-and-t.html.out)
  (deps dfb-at-and-t.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-at-and-t.html dfb-at-and-t.html.out)))

(rule
  (targets dfb-autoemail.html.out)
  (deps dfb-autoemail.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-autoemail.html dfb-autoemail.html.out)))

(rule
  (targets dfb-autoescape.html.out)
  (deps dfb-autoescape.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-autoescape.html dfb-autoescape.html.out)))

(rule
  (targets dfb-autolink.html.out)
  (deps dfb-autolink.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-autolink.html dfb-autolink.html.out)))

(rule
  (targets dfb-bq-2p-lazy.html.out)
  (deps dfb-bq-2p-lazy.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-bq-2p-lazy.html dfb-bq-2p-lazy.html.out)))

(rule
  (targets dfb-bq-2p.html.out)
  (deps dfb-bq-2p.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-bq-2p.html dfb-bq-2p.html.out)))

(rule
  (targets dfb-bq-nested.html.out)
  (deps dfb-bq-nested.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-bq-nested.html dfb-bq-nested.html.out)))

(rule
  (targets dfb-cb-ampersand.html.out)
  (deps dfb-cb-ampersand.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-cb-ampersand.html dfb-cb-ampersand.html.out)))

(rule
  (targets dfb-code1.html.out)
  (deps dfb-code1.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-code1.html dfb-code1.html.out)))

(rule
  (targets dfb-code2.html.out)
  (deps dfb-code2.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-code2.html dfb-code2.html.out)))

(rule
  (targets dfb-code3.html.out)
  (deps dfb-code3.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-code3.html dfb-code3.html.out)))

(rule
  (targets dfb-code4.html.out)
  (deps dfb-code4.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-code4.html dfb-code4.html.out)))

(rule
  (targets dfb-code5.html.out)
  (deps dfb-code5.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-code5.html dfb-code5.html.out)))

(rule
  (targets dfb-codeblock-applescript.html.out)
  (deps dfb-codeblock-applescript.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-codeblock-applescript.html dfb-codeblock-applescript.html.out)))

(rule
  (targets dfb-copy.html.out)
  (deps dfb-copy.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-copy.html dfb-copy.html.out)))

(rule
  (targets dfb-dfb.html.out)
  (deps dfb-dfb.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-dfb.html dfb-dfb.html.out)))

(rule
  (targets dfb-direct-links.html.out)
  (deps dfb-direct-links.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-direct-links.html dfb-direct-links.html.out)))

(rule
  (targets dfb-emph.html.out)
  (deps dfb-emph.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-emph.html dfb-emph.html.out)))

(rule
  (targets dfb-google.html.out)
  (deps dfb-google.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-google.html dfb-google.html.out)))

(rule
  (targets dfb-h1-h2.html.out)
  (deps dfb-h1-h2.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-h1-h2.html dfb-h1-h2.html.out)))

(rule
  (targets dfb-h123.html.out)
  (deps dfb-h123.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-h123.html dfb-h123.html.out)))

(rule
  (targets dfb-h126.html.out)
  (deps dfb-h126.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-h126.html dfb-h126.html.out)))

(rule
  (targets dfb-hr.html.out)
  (deps dfb-hr.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-hr.html dfb-hr.html.out)))

(rule
  (targets dfb-images.html.out)
  (deps dfb-images.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-images.html dfb-images.html.out)))

(rule
  (targets dfb-links-rp.html.out)
  (deps dfb-links-rp.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-links-rp.html dfb-links-rp.html.out)))

(rule
  (targets dfb-links.html.out)
  (deps dfb-links.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-links.html dfb-links.html.out)))

(rule
  (targets dfb-list-with-2p-lazy.html.out)
  (deps dfb-list-with-2p-lazy.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-list-with-2p-lazy.html dfb-list-with-2p-lazy.html.out)))

(rule
  (targets dfb-list-with-2p.html.out)
  (deps dfb-list-with-2p.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-list-with-2p.html dfb-list-with-2p.html.out)))

(rule
  (targets dfb-list-with-bq.html.out)
  (deps dfb-list-with-bq.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-list-with-bq.html dfb-list-with-bq.html.out)))

(rule
  (targets dfb-list-with-cb.html.out)
  (deps dfb-list-with-cb.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-list-with-cb.html dfb-list-with-cb.html.out)))

(rule
  (targets dfb-list9.html.out)
  (deps dfb-list9.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-list9.html dfb-list9.html.out)))

(rule
  (targets dfb-litteral-asterisks.html.out)
  (deps dfb-litteral-asterisks.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-litteral-asterisks.html dfb-litteral-asterisks.html.out)))

(rule
  (targets dfb-np-cb.html.out)
  (deps dfb-np-cb.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-np-cb.html dfb-np-cb.html.out)))

(rule
  (targets dfb-ref-image.html.out)
  (deps dfb-ref-image.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-ref-image.html dfb-ref-image.html.out)))

(rule
  (targets dfb-ref-style-1.html.out)
  (deps dfb-ref-style-1.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-ref-style-1.html dfb-ref-style-1.html.out)))

(rule
  (targets dfb-ref-style-2.html.out)
  (deps dfb-ref-style-2.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-ref-style-2.html dfb-ref-style-2.html.out)))

(rule
  (targets dfb-reflinks-in-action-implicit-link.html.out)
  (deps dfb-reflinks-in-action-implicit-link.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-reflinks-in-action-implicit-link.html dfb-reflinks-in-action-implicit-link.html.out)))

(rule
  (targets dfb-reflinks-in-action.html.out)
  (deps dfb-reflinks-in-action.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-reflinks-in-action.html dfb-reflinks-in-action.html.out)))

(rule
  (targets dfb-reg-p-with-table.html.out)
  (deps dfb-reg-p-with-table.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-reg-p-with-table.html dfb-reg-p-with-table.html.out)))

(rule
  (targets dfb-star-in-words.html.out)
  (deps dfb-star-in-words.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-star-in-words.html dfb-star-in-words.html.out)))

(rule
  (targets dfb-what-a-great-season.html.out)
  (deps dfb-what-a-great-season.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff dfb-what-a-great-season.html dfb-what-a-great-season.html.out)))

(rule
  (targets doubline-list.html.out)
  (deps doubline-list.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff doubline-list.html doubline-list.html.out)))

(rule
  (targets emphasis.html.out)
  (deps emphasis.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff emphasis.html emphasis.html.out)))

(rule
  (targets escaped-number-period.html.out)
  (deps escaped-number-period.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff escaped-number-period.html escaped-number-period.html.out)))

(rule
  (targets escaping.html.out)
  (deps escaping.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff escaping.html escaping.html.out)))

(rule
  (targets github-style-at-start.html.out)
  (deps github-style-at-start.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff github-style-at-start.html github-style-at-start.html.out)))

(rule
  (targets github-style-codeblock.html.out)
  (deps github-style-codeblock.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff github-style-codeblock.html github-style-codeblock.html.out)))

(rule
  (targets github-style-linebreaks.html.out)
  (deps github-style-linebreaks.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff github-style-linebreaks.html github-style-linebreaks.html.out)))

(rule
  (targets h1-with-double-hash.html.out)
  (deps h1-with-double-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h1-with-double-hash.html h1-with-double-hash.html.out)))

(rule
  (targets h1-with-equals.html.out)
  (deps h1-with-equals.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h1-with-equals.html h1-with-equals.html.out)))

(rule
  (targets h1-with-single-hash.html.out)
  (deps h1-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h1-with-single-hash.html h1-with-single-hash.html.out)))

(rule
  (targets h2-with-dashes.html.out)
  (deps h2-with-dashes.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h2-with-dashes.html h2-with-dashes.html.out)))

(rule
  (targets h2-with-double-hash.html.out)
  (deps h2-with-double-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h2-with-double-hash.html h2-with-double-hash.html.out)))

(rule
  (targets h2-with-single-hash.html.out)
  (deps h2-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h2-with-single-hash.html h2-with-single-hash.html.out)))

(rule
  (targets h3-with-double-hash.html.out)
  (deps h3-with-double-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h3-with-double-hash.html h3-with-double-hash.html.out)))

(rule
  (targets h3-with-single-hash.html.out)
  (deps h3-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h3-with-single-hash.html h3-with-single-hash.html.out)))

(rule
  (targets h4-with-single-hash.html.out)
  (deps h4-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h4-with-single-hash.html h4-with-single-hash.html.out)))

(rule
  (targets h5-with-single-hash.html.out)
  (deps h5-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h5-with-single-hash.html h5-with-single-hash.html.out)))

(rule
  (targets h6-with-single-hash.html.out)
  (deps h6-with-single-hash.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff h6-with-single-hash.html h6-with-single-hash.html.out)))

(rule
  (targets hashes-in-atx-titles.html.out)
  (deps hashes-in-atx-titles.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff hashes-in-atx-titles.html hashes-in-atx-titles.html.out)))

(rule
  (targets horizontal-rules.html.out)
  (deps horizontal-rules.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff horizontal-rules.html horizontal-rules.html.out)))

(rule
  (targets html5-strutural-tags.html.out)
  (deps html5-strutural-tags.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff html5-strutural-tags.html html5-strutural-tags.html.out)))

(rule
  (targets html_tags.html.out)
  (deps html_tags.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff html_tags.html html_tags.html.out)))

(rule
  (targets images.html.out)
  (deps images.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff images.html images.html.out)))

(rule
  (targets implicit-anchors.html.out)
  (deps implicit-anchors.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff implicit-anchors.html implicit-anchors.html.out)))

(rule
  (targets inline-anchors.html.out)
  (deps inline-anchors.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff inline-anchors.html inline-anchors.html.out)))

(rule
  (targets inline-code.html.out)
  (deps inline-code.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff inline-code.html inline-code.html.out)))

(rule
  (targets inline-escaped-chars.html.out)
  (deps inline-escaped-chars.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff inline-escaped-chars.html inline-escaped-chars.html.out)))

(rule
  (targets inline-style-tag.html.out)
  (deps inline-style-tag.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff inline-style-tag.html inline-style-tag.html.out)))

(rule
  (targets lazy-blockquote.html.out)
  (deps lazy-blockquote.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff lazy-blockquote.html lazy-blockquote.html.out)))

(rule
  (targets link-with-code.html.out)
  (deps link-with-code.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff link-with-code.html link-with-code.html.out)))

(rule
  (targets list-with-blockquote.html.out)
  (deps list-with-blockquote.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff list-with-blockquote.html list-with-blockquote.html.out)))

(rule
  (targets list-with-code-header.html.out)
  (deps list-with-code-header.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff list-with-code-header.html list-with-code-header.html.out)))

(rule
  (targets list-with-code.html.out)
  (deps list-with-code.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff list-with-code.html list-with-code.html.out)))

(rule
  (targets lists-with-blank-lines.html.out)
  (deps lists-with-blank-lines.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff lists-with-blank-lines.html lists-with-blank-lines.html.out)))

(rule
  (targets multi-paragraph-list.html.out)
  (deps multi-paragraph-list.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff multi-paragraph-list.html multi-paragraph-list.html.out)))

(rule
  (targets multiline-unordered-list.html.out)
  (deps multiline-unordered-list.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff multiline-unordered-list.html multiline-unordered-list.html.out)))

(rule
  (targets nested-blockquote.html.out)
  (deps nested-blockquote.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff nested-blockquote.html nested-blockquote.html.out)))

(rule
  (targets non-html-tags-a.html.out)
  (deps non-html-tags-a.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff non-html-tags-a.html non-html-tags-a.html.out)))

(rule
  (targets non-html-tags-simple.html.out)
  (deps non-html-tags-simple.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff non-html-tags-simple.html non-html-tags-simple.html.out)))

(rule
  (targets non-html-tags.html.out)
  (deps non-html-tags.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff non-html-tags.html non-html-tags.html.out)))

(rule
  (targets not-a-hr.html.out)
  (deps not-a-hr.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff not-a-hr.html not-a-hr.html.out)))

(rule
  (targets ordered-list-same-number.html.out)
  (deps ordered-list-same-number.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff ordered-list-same-number.html ordered-list-same-number.html.out)))

(rule
  (targets ordered-list-wrong-numbers.html.out)
  (deps ordered-list-wrong-numbers.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff ordered-list-wrong-numbers.html ordered-list-wrong-numbers.html.out)))

(rule
  (targets ordered-list.html.out)
  (deps ordered-list.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff ordered-list.html ordered-list.html.out)))

(rule
  (targets references__nonregular.html.out)
  (deps references__nonregular.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff references__nonregular.html references__nonregular.html.out)))

(rule
  (targets relative-anchors.html.out)
  (deps relative-anchors.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff relative-anchors.html relative-anchors.html.out)))

(rule
  (targets simple-paragraph.html.out)
  (deps simple-paragraph.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff simple-paragraph.html simple-paragraph.html.out)))

(rule
  (targets strong.html.out)
  (deps strong.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff strong.html strong.html.out)))

(rule
  (targets undef_ref.html.out)
  (deps undef_ref.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff undef_ref.html undef_ref.html.out)))

(rule
  (targets unordered-list-asterisk.html.out)
  (deps unordered-list-asterisk.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff unordered-list-asterisk.html unordered-list-asterisk.html.out)))

(rule
  (targets unordered-list-minus.html.out)
  (deps unordered-list-minus.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff unordered-list-minus.html unordered-list-minus.html.out)))

(rule
  (targets unordered-list-plus.html.out)
  (deps unordered-list-plus.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff unordered-list-plus.html unordered-list-plus.html.out)))

(rule
  (targets url-with-parenthesis.html.out)
  (deps url-with-parenthesis.md)
  (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
  (name runtest)
  (action (diff url-with-parenthesis.html url-with-parenthesis.html.out)))

