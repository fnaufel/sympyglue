
# To be turned into tests -------------------------------------------------

reticulate::py_run_string('v1 = [1, 2, 3]')
reticulate::py_run_string('v2 = [2, 4, 6]')

reticulate::py_run_string('before = [3, 4]')
reticulate::py_run_string('after = [5, 6]')

before <- b
after <- a
s <- '{{v2}} is double {{v1}}.'

a <- c(1, 2, 3)
b <- 2 * a

m(s)
m('{{v2}}', '{{v1}}')
m('{{v2}}', '{{v1}}', sep = ' is double ')


rv <- m(s, before = b, after = a, use_cat = TRUE)

rv

cat(rv)
knitr::asis_output(rv)

middle <- "2 is double 1.\n4 is double 2.\n6 is double 3."



# R stuff -----------------------------------------------------------------

glue::glue(
  'a = {a}',
  'b = {b}',
  .sep = '/'
)

stringr::str_c(
  c(1, 2, 3),
  collapse = ' / '
)

glue::glue(
  '{a} {b}',
)
