#!/bin/bash

## rmarkdown does this:
# /usr/local/bin/pandoc +RTS -K512m -RTS paper.utf8.md \
#   --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash \
#   --output paper.tex \
#   --template /Users/waseyj/Library/R/3.5/library/rmarkdown/rmd/latex/default-1.17.0.2.tex \
#   --highlight-style tango \
#   --pdf-engine pdflatex \
#   --variable graphics=yes \
#   --variable 'geometry:margin=1in' \
#   --variable 'compact-title:yes'

if false
then
echo "Doing full pandoc for JOSS"
#  --pdf-engine=xelatex \
pandoc \
  --verbose \
  -V repository="repo" \
  -V archive_doi="doi" \
  -V paper_url="purl" \
  -V formatted_doi="fdoi" \
  -V review_issue_url="revisurl" \
  -V graphics="true" \
  -V issue="issue" \
  -V volume="volume" \
  -V page="page" \
  -V joss_logo_path="/usr/local/src/git/whedon/resources/joss-logo.png" \
  -V year="year" \
  -V submitted="sub" \
  -V published="pub" \
  -V citation_author="jackwasey" \
  -V paper_title="papertitle" \
  -V geometry:margin=1in \
  -o paper.pdf \
  --filter pandoc-citeproc paper.md \
  --template "/usr/local/src/git/whedon/resources/latex.template" \
  paper.md
else
  echo "Doing test pandoc for JOSS"
  pandoc \
    --verbose \
    -V geometry:margin=1in \
    -V joss_logo_path="/usr/local/src/git/whedon/resources/joss-logo.png" \
    -o paper.pdf \
    --pdf-engine=xelatex \
    --filter pandoc-citeproc paper.md \
    --template "/usr/local/src/git/whedon/resources/latex.template"
fi

