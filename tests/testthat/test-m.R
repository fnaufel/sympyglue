
test_that("One string, no interpolation, cat output", {

  expect_equal(
    m(
      'A sample string.',
      use_cat = TRUE
    ),
    "A sample string."
  )

})

test_that("One string, no interpolation, asis output is default", {

  expect_equal(
    m(
      'A sample string.'
    ),
    knitr::asis_output("A sample string.")
  )

})

test_that("Two strings, no interpolation, cat output", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      use_cat = TRUE
    ),
    "A sample string. And another."
  )

})

test_that("Two strings, no interpolation, asis output", {

  expect_equal(
    m(
      'A sample string.',
      'And another.'
    ),
    knitr::asis_output("A sample string. And another.")
  )

})

test_that("Two strings, no interpolation, cat output, separator", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      sep = ' - ',
      use_cat = TRUE
    ),
    "A sample string. - And another."
  )

})

test_that("Two strings, no interpolation, asis output, separator", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      sep = ' - ',
      use_cat = FALSE
    ),
    knitr::asis_output("A sample string. - And another.")
  )

})


test_that("Two strings, no interpolation, cat, before and after", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      use_cat = TRUE,
      before = 'Text before.',
      after = 'Text after.'
    ),
    "Text before.\nA sample string. And another.\nText after."
  )

})

test_that("Two strings, no interpolation, cat, before and after vectors", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      use_cat = TRUE,
      before = c('Text before.', 'With two strings.'),
      after = c('Text after.', 'With two strings.'),
    ),
    "Text before.\nWith two strings.\nA sample string. And another. \nText after.\nWith two strings."
  )

})

test_that("Two strings, no interpolation, asis, before and after vectors", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      before = c('Text before.', 'With two strings.'),
      after = c('Text after.', 'With two strings.'),
    ),
    knitr::asis_output("Text before.\nWith two strings.\nA sample string. And another. \nText after.\nWith two strings.")
  )

})

test_that("Two strings, no interpolation, cat, before and after vectors, seps", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      sep = ' /s/ ',
      use_cat = TRUE,
      before = c('Text before.', 'With two strings.'),
      sep_before = ' /b/ ',
      after = c('Text after.', 'With two strings.'),
      sep_after = ' /a/ ',
      sep_blocks = '\n /m/ \n'
    ),
    "Text before. /b/ With two strings.\n /m/ \nA sample string. /s/ And another.\n /m/ \nText after. /a/ With two strings."
  )

})

test_that("Two strings, no interpolation, asis, before and after vectors, seps", {

  expect_equal(
    m(
      'A sample string.',
      'And another.',
      sep = ' /s/ ',
      use_cat = FALSE,
      before = c('Text before.', 'With two strings.'),
      sep_before = ' /b/ ',
      after = c('Text after.', 'With two strings.'),
      sep_after = ' /a/ ',
      sep_blocks = '\n /m/ \n'
    ),
    knitr::asis_output("Text before. /b/ With two strings.\n /m/ \nA sample string. /s/ And another.\n /m/ \nText after. /a/ With two strings.")
  )

})

test_that("Interpolation, scalars, cat output", {

  reticulate::py_run_string(
    'a, b, c = symbols(\'a b c\')'
  )

  reticulate::py_run_string(
    'delta = sqrt(b**2 - 4*a*c)'
  )

  expect_equal(
    m(
      'The discriminant:',
      '$\\Delta = {{delta}}$.',
      use_cat = TRUE
    ),
    "The discriminant: $\\Delta = \\sqrt{- 4 a c + b^{2}}$."
  )

})


# From here on, cat output ------------------------------------------------

test_that("Interpolation, vectors", {

  reticulate::py_run_string(
    'a, b, c = symbols(\'a b c\')'
  )

  reticulate::py_run_string(
    'delta = sqrt(b**2 - 4*a*c)'
  )

  reticulate::py_run_string(
    'delta = [delta.subs({a : i}) for i in range(4)]'
  )

  expect_equal(
    m(
      'The discriminant:',
      '$\\Delta = {{delta}}$.',
      use_cat = TRUE
    ),
    "The discriminant: $\\Delta = \\sqrt{b^{2}}$.\nThe discriminant: $\\Delta = \\sqrt{b^{2} - 4 c}$.\nThe discriminant: $\\Delta = \\sqrt{b^{2} - 8 c}$.\nThe discriminant: $\\Delta = \\sqrt{b^{2} - 12 c}$."
  )

})

test_that("Interpolation, two vectors", {

  reticulate::py_run_string(
    'a, b, c = symbols(\'a b c\')'
  )

  reticulate::py_run_string(
    'delta = sqrt(b**2 - 4*a*c)\navalues = list(range(4))'
  )

  reticulate::py_run_string(
    'delta = [delta.subs({a : i}) for i in avalues]'
  )

  expect_equal(
    m(
      'The discriminant with $a = {{avalues}}$:',
      '$\\Delta = {{delta}}$.',
      use_cat = TRUE
    ),
    "The discriminant with $a = 0$: $\\Delta = \\sqrt{b^{2}}$.\nThe discriminant with $a = 1$: $\\Delta = \\sqrt{b^{2} - 4 c}$.\nThe discriminant with $a = 2$: $\\Delta = \\sqrt{b^{2} - 8 c}$.\nThe discriminant with $a = 3$: $\\Delta = \\sqrt{b^{2} - 12 c}$."
  )

})

