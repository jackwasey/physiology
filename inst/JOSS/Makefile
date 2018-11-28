# can't believe it has come to this...

paper.utf8.md: paper.Rmd
	Rscript -e 'rmarkdown::render("paper.Rmd", clean = FALSE, run_pandoc = FALSE, output_format = "github_document")'

paper.md: paper.utf8.md
	cat paper.yaml paper.utf8.md > paper.md

paper.pdf: paper.md
	pandoc -o paper.pdf paper.md
#latexmk -xelatex paper.tex
#./paper.sh

#paper.tex: paper.md
#	pandoc -o paper.tex paper.md

.PHONY: clean
clean:
	rm -rf paper.utf8.md paper.knit.md paper.md paper_files paper.xdv paper.dvi Rplots.pdf paper.html
	latexmk -c -quiet paper
