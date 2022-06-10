.onLoad <- function(libname, pkgname) {

  # The only reason we're doing this is to make sympy help available in RStudio.
  # Of course we can also use this variable to access sympy objects anytime.
  sympy <- reticulate::import('sympy')

  # Init sympy
  reticulate::py_run_string('from sympy import *')
  reticulate::py_run_string('init_printing(use_latex = True)')

}


.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    '\n\n',
    'Welcome to sympyglue.',
    '\n\n',
    '1. You can access the sympy module from R using the syntax `sympy$...`.',
    '\n\n',
    '2. Set the RETICULATE_PYTHON and RETICULATE_PYTHON_ENV environment variables',
    ' if desired. See https://rstudio.github.io/reticulate/articles/versions.html.',
    '\n\n',
    '3. Read the sympyglue docs at ???.',
    '\n\n\n'
  )

}
