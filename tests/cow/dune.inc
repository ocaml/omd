(rule
 (targets anchors-by-reference.html.out)
 (deps anchors-by-reference.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name anchors-by-reference)
 (action (diff anchors-by-reference.html anchors-by-reference.html.out)))

(rule
 (targets automatic-anchors.html.out)
 (deps automatic-anchors.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name automatic-anchors)
 (action (diff automatic-anchors.html automatic-anchors.html.out)))

(rule
 (targets blockquote-nested-markdown.html.out)
 (deps blockquote-nested-markdown.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquote-nested-markdown)
 (action (diff blockquote-nested-markdown.html blockquote-nested-markdown.html.out)))

(rule
 (targets blockquote-starting-with-empty-lines.html.out)
 (deps blockquote-starting-with-empty-lines.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquote-starting-with-empty-lines)
 (action (diff blockquote-starting-with-empty-lines.html blockquote-starting-with-empty-lines.html.out)))

(rule
 (targets blockquote.html.out)
 (deps blockquote.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquote)
 (action (diff blockquote.html blockquote.html.out)))

(rule
 (targets blockquotes-with-empty-lines-inbetween-2.html.out)
 (deps blockquotes-with-empty-lines-inbetween-2.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquotes-with-empty-lines-inbetween-2)
 (action (diff blockquotes-with-empty-lines-inbetween-2.html blockquotes-with-empty-lines-inbetween-2.html.out)))

(rule
 (targets blockquotes-with-empty-lines-inbetween-3.html.out)
 (deps blockquotes-with-empty-lines-inbetween-3.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquotes-with-empty-lines-inbetween-3)
 (action (diff blockquotes-with-empty-lines-inbetween-3.html blockquotes-with-empty-lines-inbetween-3.html.out)))

(rule
 (targets blockquotes-with-empty-lines-inbetween-4.html.out)
 (deps blockquotes-with-empty-lines-inbetween-4.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquotes-with-empty-lines-inbetween-4)
 (action (diff blockquotes-with-empty-lines-inbetween-4.html blockquotes-with-empty-lines-inbetween-4.html.out)))

(rule
 (targets blockquotes-with-empty-lines-inbetween.html.out)
 (deps blockquotes-with-empty-lines-inbetween.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name blockquotes-with-empty-lines-inbetween)
 (action (diff blockquotes-with-empty-lines-inbetween.html blockquotes-with-empty-lines-inbetween.html.out)))

(rule
 (targets code-block-html-escape.html.out)
 (deps code-block-html-escape.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name code-block-html-escape)
 (action (diff code-block-html-escape.html code-block-html-escape.html.out)))

(rule
 (targets code-block.html.out)
 (deps code-block.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name code-block)
 (action (diff code-block.html code-block.html.out)))

(rule
 (targets dfb-3def.html.out)
 (deps dfb-3def.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-3def)
 (action (diff dfb-3def.html dfb-3def.html.out)))

(rule
 (targets dfb-4lt5.html.out)
 (deps dfb-4lt5.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-4lt5)
 (action (diff dfb-4lt5.html dfb-4lt5.html.out)))

(rule
 (targets dfb-at-and-t.html.out)
 (deps dfb-at-and-t.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-at-and-t)
 (action (diff dfb-at-and-t.html dfb-at-and-t.html.out)))

(rule
 (targets dfb-autoemail.html.out)
 (deps dfb-autoemail.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-autoemail)
 (action (diff dfb-autoemail.html dfb-autoemail.html.out)))

(rule
 (targets dfb-autoescape.html.out)
 (deps dfb-autoescape.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-autoescape)
 (action (diff dfb-autoescape.html dfb-autoescape.html.out)))

(rule
 (targets dfb-autolink.html.out)
 (deps dfb-autolink.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-autolink)
 (action (diff dfb-autolink.html dfb-autolink.html.out)))

(rule
 (targets dfb-bq-2p-lazy.html.out)
 (deps dfb-bq-2p-lazy.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-bq-2p-lazy)
 (action (diff dfb-bq-2p-lazy.html dfb-bq-2p-lazy.html.out)))

(rule
 (targets dfb-bq-2p.html.out)
 (deps dfb-bq-2p.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-bq-2p)
 (action (diff dfb-bq-2p.html dfb-bq-2p.html.out)))

(rule
 (targets dfb-bq-nested.html.out)
 (deps dfb-bq-nested.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-bq-nested)
 (action (diff dfb-bq-nested.html dfb-bq-nested.html.out)))

(rule
 (targets dfb-cb-ampersand.html.out)
 (deps dfb-cb-ampersand.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-cb-ampersand)
 (action (diff dfb-cb-ampersand.html dfb-cb-ampersand.html.out)))

(rule
 (targets dfb-code1.html.out)
 (deps dfb-code1.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-code1)
 (action (diff dfb-code1.html dfb-code1.html.out)))

(rule
 (targets dfb-code2.html.out)
 (deps dfb-code2.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-code2)
 (action (diff dfb-code2.html dfb-code2.html.out)))

(rule
 (targets dfb-code3.html.out)
 (deps dfb-code3.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-code3)
 (action (diff dfb-code3.html dfb-code3.html.out)))

(rule
 (targets dfb-code4.html.out)
 (deps dfb-code4.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-code4)
 (action (diff dfb-code4.html dfb-code4.html.out)))

(rule
 (targets dfb-code5.html.out)
 (deps dfb-code5.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-code5)
 (action (diff dfb-code5.html dfb-code5.html.out)))

(rule
 (targets dfb-codeblock-applescript.html.out)
 (deps dfb-codeblock-applescript.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-codeblock-applescript)
 (action (diff dfb-codeblock-applescript.html dfb-codeblock-applescript.html.out)))

(rule
 (targets dfb-copy.html.out)
 (deps dfb-copy.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-copy)
 (action (diff dfb-copy.html dfb-copy.html.out)))

(rule
 (targets dfb-dfb.html.out)
 (deps dfb-dfb.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-dfb)
 (action (diff dfb-dfb.html dfb-dfb.html.out)))

(rule
 (targets dfb-direct-links.html.out)
 (deps dfb-direct-links.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-direct-links)
 (action (diff dfb-direct-links.html dfb-direct-links.html.out)))

(rule
 (targets dfb-emph.html.out)
 (deps dfb-emph.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-emph)
 (action (diff dfb-emph.html dfb-emph.html.out)))

(rule
 (targets dfb-google.html.out)
 (deps dfb-google.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-google)
 (action (diff dfb-google.html dfb-google.html.out)))

(rule
 (targets dfb-h1-h2.html.out)
 (deps dfb-h1-h2.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-h1-h2)
 (action (diff dfb-h1-h2.html dfb-h1-h2.html.out)))

(rule
 (targets dfb-h123.html.out)
 (deps dfb-h123.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-h123)
 (action (diff dfb-h123.html dfb-h123.html.out)))

(rule
 (targets dfb-h126.html.out)
 (deps dfb-h126.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-h126)
 (action (diff dfb-h126.html dfb-h126.html.out)))

(rule
 (targets dfb-hr.html.out)
 (deps dfb-hr.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-hr)
 (action (diff dfb-hr.html dfb-hr.html.out)))

(rule
 (targets dfb-images.html.out)
 (deps dfb-images.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-images)
 (action (diff dfb-images.html dfb-images.html.out)))

(rule
 (targets dfb-links-rp.html.out)
 (deps dfb-links-rp.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-links-rp)
 (action (diff dfb-links-rp.html dfb-links-rp.html.out)))

(rule
 (targets dfb-links.html.out)
 (deps dfb-links.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-links)
 (action (diff dfb-links.html dfb-links.html.out)))

(rule
 (targets dfb-list-with-2p-lazy.html.out)
 (deps dfb-list-with-2p-lazy.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-list-with-2p-lazy)
 (action (diff dfb-list-with-2p-lazy.html dfb-list-with-2p-lazy.html.out)))

(rule
 (targets dfb-list-with-2p.html.out)
 (deps dfb-list-with-2p.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-list-with-2p)
 (action (diff dfb-list-with-2p.html dfb-list-with-2p.html.out)))

(rule
 (targets dfb-list-with-bq.html.out)
 (deps dfb-list-with-bq.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-list-with-bq)
 (action (diff dfb-list-with-bq.html dfb-list-with-bq.html.out)))

(rule
 (targets dfb-list-with-cb.html.out)
 (deps dfb-list-with-cb.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-list-with-cb)
 (action (diff dfb-list-with-cb.html dfb-list-with-cb.html.out)))

(rule
 (targets dfb-list9.html.out)
 (deps dfb-list9.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-list9)
 (action (diff dfb-list9.html dfb-list9.html.out)))

(rule
 (targets dfb-litteral-asterisks.html.out)
 (deps dfb-litteral-asterisks.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-litteral-asterisks)
 (action (diff dfb-litteral-asterisks.html dfb-litteral-asterisks.html.out)))

(rule
 (targets dfb-np-cb.html.out)
 (deps dfb-np-cb.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-np-cb)
 (action (diff dfb-np-cb.html dfb-np-cb.html.out)))

(rule
 (targets dfb-ref-image.html.out)
 (deps dfb-ref-image.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-ref-image)
 (action (diff dfb-ref-image.html dfb-ref-image.html.out)))

(rule
 (targets dfb-ref-style-1.html.out)
 (deps dfb-ref-style-1.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-ref-style-1)
 (action (diff dfb-ref-style-1.html dfb-ref-style-1.html.out)))

(rule
 (targets dfb-ref-style-2.html.out)
 (deps dfb-ref-style-2.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-ref-style-2)
 (action (diff dfb-ref-style-2.html dfb-ref-style-2.html.out)))

(rule
 (targets dfb-reflinks-in-action-implicit-link.html.out)
 (deps dfb-reflinks-in-action-implicit-link.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-reflinks-in-action-implicit-link)
 (action (diff dfb-reflinks-in-action-implicit-link.html dfb-reflinks-in-action-implicit-link.html.out)))

(rule
 (targets dfb-reflinks-in-action.html.out)
 (deps dfb-reflinks-in-action.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-reflinks-in-action)
 (action (diff dfb-reflinks-in-action.html dfb-reflinks-in-action.html.out)))

(rule
 (targets dfb-reg-p-with-table.html.out)
 (deps dfb-reg-p-with-table.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-reg-p-with-table)
 (action (diff dfb-reg-p-with-table.html dfb-reg-p-with-table.html.out)))

(rule
 (targets dfb-star-in-words.html.out)
 (deps dfb-star-in-words.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-star-in-words)
 (action (diff dfb-star-in-words.html dfb-star-in-words.html.out)))

(rule
 (targets dfb-what-a-great-season.html.out)
 (deps dfb-what-a-great-season.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name dfb-what-a-great-season)
 (action (diff dfb-what-a-great-season.html dfb-what-a-great-season.html.out)))

(rule
 (targets doubline-list.html.out)
 (deps doubline-list.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name doubline-list)
 (action (diff doubline-list.html doubline-list.html.out)))

(rule
 (targets emphasis.html.out)
 (deps emphasis.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name emphasis)
 (action (diff emphasis.html emphasis.html.out)))

(rule
 (targets escaped-number-period.html.out)
 (deps escaped-number-period.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name escaped-number-period)
 (action (diff escaped-number-period.html escaped-number-period.html.out)))

(rule
 (targets escaping.html.out)
 (deps escaping.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name escaping)
 (action (diff escaping.html escaping.html.out)))

(rule
 (targets github-style-at-start.html.out)
 (deps github-style-at-start.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name github-style-at-start)
 (action (diff github-style-at-start.html github-style-at-start.html.out)))

(rule
 (targets github-style-codeblock.html.out)
 (deps github-style-codeblock.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name github-style-codeblock)
 (action (diff github-style-codeblock.html github-style-codeblock.html.out)))

(rule
 (targets github-style-linebreaks.html.out)
 (deps github-style-linebreaks.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name github-style-linebreaks)
 (action (diff github-style-linebreaks.html github-style-linebreaks.html.out)))

(rule
 (targets h1-with-double-hash.html.out)
 (deps h1-with-double-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h1-with-double-hash)
 (action (diff h1-with-double-hash.html h1-with-double-hash.html.out)))

(rule
 (targets h1-with-equals.html.out)
 (deps h1-with-equals.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h1-with-equals)
 (action (diff h1-with-equals.html h1-with-equals.html.out)))

(rule
 (targets h1-with-single-hash.html.out)
 (deps h1-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h1-with-single-hash)
 (action (diff h1-with-single-hash.html h1-with-single-hash.html.out)))

(rule
 (targets h2-with-dashes.html.out)
 (deps h2-with-dashes.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h2-with-dashes)
 (action (diff h2-with-dashes.html h2-with-dashes.html.out)))

(rule
 (targets h2-with-double-hash.html.out)
 (deps h2-with-double-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h2-with-double-hash)
 (action (diff h2-with-double-hash.html h2-with-double-hash.html.out)))

(rule
 (targets h2-with-single-hash.html.out)
 (deps h2-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h2-with-single-hash)
 (action (diff h2-with-single-hash.html h2-with-single-hash.html.out)))

(rule
 (targets h3-with-double-hash.html.out)
 (deps h3-with-double-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h3-with-double-hash)
 (action (diff h3-with-double-hash.html h3-with-double-hash.html.out)))

(rule
 (targets h3-with-single-hash.html.out)
 (deps h3-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h3-with-single-hash)
 (action (diff h3-with-single-hash.html h3-with-single-hash.html.out)))

(rule
 (targets h4-with-single-hash.html.out)
 (deps h4-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h4-with-single-hash)
 (action (diff h4-with-single-hash.html h4-with-single-hash.html.out)))

(rule
 (targets h5-with-single-hash.html.out)
 (deps h5-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h5-with-single-hash)
 (action (diff h5-with-single-hash.html h5-with-single-hash.html.out)))

(rule
 (targets h6-with-single-hash.html.out)
 (deps h6-with-single-hash.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name h6-with-single-hash)
 (action (diff h6-with-single-hash.html h6-with-single-hash.html.out)))

(rule
 (targets hashes-in-atx-titles.html.out)
 (deps hashes-in-atx-titles.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name hashes-in-atx-titles)
 (action (diff hashes-in-atx-titles.html hashes-in-atx-titles.html.out)))

(rule
 (targets horizontal-rules.html.out)
 (deps horizontal-rules.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name horizontal-rules)
 (action (diff horizontal-rules.html horizontal-rules.html.out)))

(rule
 (targets html5-strutural-tags.html.out)
 (deps html5-strutural-tags.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name html5-strutural-tags)
 (action (diff html5-strutural-tags.html html5-strutural-tags.html.out)))

(rule
 (targets html_tags.html.out)
 (deps html_tags.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name html_tags)
 (action (diff html_tags.html html_tags.html.out)))

(rule
 (targets images.html.out)
 (deps images.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name images)
 (action (diff images.html images.html.out)))

(rule
 (targets implicit-anchors.html.out)
 (deps implicit-anchors.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name implicit-anchors)
 (action (diff implicit-anchors.html implicit-anchors.html.out)))

(rule
 (targets inline-anchors.html.out)
 (deps inline-anchors.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name inline-anchors)
 (action (diff inline-anchors.html inline-anchors.html.out)))

(rule
 (targets inline-code.html.out)
 (deps inline-code.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name inline-code)
 (action (diff inline-code.html inline-code.html.out)))

(rule
 (targets inline-escaped-chars.html.out)
 (deps inline-escaped-chars.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name inline-escaped-chars)
 (action (diff inline-escaped-chars.html inline-escaped-chars.html.out)))

(rule
 (targets inline-style-tag.html.out)
 (deps inline-style-tag.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name inline-style-tag)
 (action (diff inline-style-tag.html inline-style-tag.html.out)))

(rule
 (targets lazy-blockquote.html.out)
 (deps lazy-blockquote.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name lazy-blockquote)
 (action (diff lazy-blockquote.html lazy-blockquote.html.out)))

(rule
 (targets link-with-code.html.out)
 (deps link-with-code.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name link-with-code)
 (action (diff link-with-code.html link-with-code.html.out)))

(rule
 (targets list-with-blockquote.html.out)
 (deps list-with-blockquote.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name list-with-blockquote)
 (action (diff list-with-blockquote.html list-with-blockquote.html.out)))

(rule
 (targets list-with-code-header.html.out)
 (deps list-with-code-header.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name list-with-code-header)
 (action (diff list-with-code-header.html list-with-code-header.html.out)))

(rule
 (targets list-with-code.html.out)
 (deps list-with-code.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name list-with-code)
 (action (diff list-with-code.html list-with-code.html.out)))

(rule
 (targets lists-with-blank-lines.html.out)
 (deps lists-with-blank-lines.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name lists-with-blank-lines)
 (action (diff lists-with-blank-lines.html lists-with-blank-lines.html.out)))

(rule
 (targets multi-paragraph-list.html.out)
 (deps multi-paragraph-list.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name multi-paragraph-list)
 (action (diff multi-paragraph-list.html multi-paragraph-list.html.out)))

(rule
 (targets multiline-unordered-list.html.out)
 (deps multiline-unordered-list.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name multiline-unordered-list)
 (action (diff multiline-unordered-list.html multiline-unordered-list.html.out)))

(rule
 (targets nested-blockquote.html.out)
 (deps nested-blockquote.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name nested-blockquote)
 (action (diff nested-blockquote.html nested-blockquote.html.out)))

(rule
 (targets non-html-tags-a.html.out)
 (deps non-html-tags-a.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name non-html-tags-a)
 (action (diff non-html-tags-a.html non-html-tags-a.html.out)))

(rule
 (targets non-html-tags-simple.html.out)
 (deps non-html-tags-simple.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name non-html-tags-simple)
 (action (diff non-html-tags-simple.html non-html-tags-simple.html.out)))

(rule
 (targets non-html-tags.html.out)
 (deps non-html-tags.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name non-html-tags)
 (action (diff non-html-tags.html non-html-tags.html.out)))

(rule
 (targets not-a-hr.html.out)
 (deps not-a-hr.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name not-a-hr)
 (action (diff not-a-hr.html not-a-hr.html.out)))

(rule
 (targets ordered-list-same-number.html.out)
 (deps ordered-list-same-number.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name ordered-list-same-number)
 (action (diff ordered-list-same-number.html ordered-list-same-number.html.out)))

(rule
 (targets ordered-list-wrong-numbers.html.out)
 (deps ordered-list-wrong-numbers.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name ordered-list-wrong-numbers)
 (action (diff ordered-list-wrong-numbers.html ordered-list-wrong-numbers.html.out)))

(rule
 (targets ordered-list.html.out)
 (deps ordered-list.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name ordered-list)
 (action (diff ordered-list.html ordered-list.html.out)))

(rule
 (targets references__nonregular.html.out)
 (deps references__nonregular.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name references__nonregular)
 (action (diff references__nonregular.html references__nonregular.html.out)))

(rule
 (targets relative-anchors.html.out)
 (deps relative-anchors.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name relative-anchors)
 (action (diff relative-anchors.html relative-anchors.html.out)))

(rule
 (targets simple-paragraph.html.out)
 (deps simple-paragraph.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name simple-paragraph)
 (action (diff simple-paragraph.html simple-paragraph.html.out)))

(rule
 (targets strong.html.out)
 (deps strong.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name strong)
 (action (diff strong.html strong.html.out)))

(rule
 (targets undef_ref.html.out)
 (deps undef_ref.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name undef_ref)
 (action (diff undef_ref.html undef_ref.html.out)))

(rule
 (targets unordered-list-asterisk.html.out)
 (deps unordered-list-asterisk.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name unordered-list-asterisk)
 (action (diff unordered-list-asterisk.html unordered-list-asterisk.html.out)))

(rule
 (targets unordered-list-minus.html.out)
 (deps unordered-list-minus.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name unordered-list-minus)
 (action (diff unordered-list-minus.html unordered-list-minus.html.out)))

(rule
 (targets unordered-list-plus.html.out)
 (deps unordered-list-plus.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name unordered-list-plus)
 (action (diff unordered-list-plus.html unordered-list-plus.html.out)))

(rule
 (targets url-with-parenthesis.html.out)
 (deps url-with-parenthesis.md)
 (action (with-stdout-to %{targets} (run omd %{deps}))))

(alias
 (name url-with-parenthesis)
 (action (diff url-with-parenthesis.html url-with-parenthesis.html.out)))

(alias
 (name runtest)
 (deps (alias anchors-by-reference)
       (alias automatic-anchors)
       (alias blockquote-nested-markdown)
       (alias blockquote-starting-with-empty-lines)
       (alias blockquote)
       (alias blockquotes-with-empty-lines-inbetween-2)
       (alias blockquotes-with-empty-lines-inbetween-3)
       (alias blockquotes-with-empty-lines-inbetween-4)
       (alias blockquotes-with-empty-lines-inbetween)
       (alias code-block-html-escape)
       (alias code-block)
       (alias dfb-3def)
       (alias dfb-4lt5)
       (alias dfb-at-and-t)
       (alias dfb-autoemail)
       (alias dfb-autoescape)
       (alias dfb-autolink)
       (alias dfb-bq-2p-lazy)
       (alias dfb-bq-2p)
       (alias dfb-bq-nested)
       (alias dfb-cb-ampersand)
       (alias dfb-code1)
       (alias dfb-code2)
       (alias dfb-code3)
       (alias dfb-code4)
       (alias dfb-code5)
       (alias dfb-codeblock-applescript)
       (alias dfb-copy)
       (alias dfb-dfb)
       (alias dfb-direct-links)
       (alias dfb-emph)
       (alias dfb-google)
       (alias dfb-h1-h2)
       (alias dfb-h123)
       (alias dfb-h126)
       (alias dfb-hr)
       (alias dfb-images)
       (alias dfb-links-rp)
       (alias dfb-links)
       (alias dfb-list-with-2p-lazy)
       (alias dfb-list-with-2p)
       (alias dfb-list-with-bq)
       (alias dfb-list-with-cb)
       (alias dfb-list9)
       (alias dfb-litteral-asterisks)
       (alias dfb-np-cb)
       (alias dfb-ref-image)
       (alias dfb-ref-style-1)
       (alias dfb-ref-style-2)
       (alias dfb-reflinks-in-action-implicit-link)
       (alias dfb-reflinks-in-action)
       (alias dfb-reg-p-with-table)
       (alias dfb-star-in-words)
       (alias dfb-what-a-great-season)
       (alias doubline-list)
       (alias emphasis)
       (alias escaped-number-period)
       (alias escaping)
       (alias github-style-at-start)
       (alias github-style-codeblock)
       (alias github-style-linebreaks)
       (alias h1-with-double-hash)
       (alias h1-with-equals)
       (alias h1-with-single-hash)
       (alias h2-with-dashes)
       (alias h2-with-double-hash)
       (alias h2-with-single-hash)
       (alias h3-with-double-hash)
       (alias h3-with-single-hash)
       (alias h4-with-single-hash)
       (alias h5-with-single-hash)
       (alias h6-with-single-hash)
       (alias hashes-in-atx-titles)
       (alias horizontal-rules)
       (alias html5-strutural-tags)
       (alias html_tags)
       (alias images)
       (alias implicit-anchors)
       (alias inline-anchors)
       (alias inline-code)
       (alias inline-escaped-chars)
       (alias inline-style-tag)
       (alias lazy-blockquote)
       ;; (alias link-with-code)
       (alias list-with-blockquote)
       (alias list-with-code-header)
       (alias list-with-code)
       (alias lists-with-blank-lines)
       (alias multi-paragraph-list)
       (alias multiline-unordered-list)
       (alias nested-blockquote)
       (alias non-html-tags-a)
       (alias non-html-tags-simple)
       (alias non-html-tags)
       (alias not-a-hr)
       (alias ordered-list-same-number)
       (alias ordered-list-wrong-numbers)
       (alias ordered-list)
       (alias references__nonregular)
       (alias relative-anchors)
       (alias simple-paragraph)
       (alias strong)
       (alias undef_ref)
       (alias unordered-list-asterisk)
       (alias unordered-list-minus)
       (alias unordered-list-plus)
       (alias url-with-parenthesis)))

(alias
 (name runtest_all)
 (deps (alias runtest)
       (alias link-with-code)))
