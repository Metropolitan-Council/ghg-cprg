styler::style_dir()

testthat::test_dir("tests")

sh("quarto render --cache-refresh --to html")
sh("quarto preview")
