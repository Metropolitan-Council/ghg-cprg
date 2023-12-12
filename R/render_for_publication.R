styler::style_dir()

testthat::test_dir("tests")

rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
