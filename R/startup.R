#startup msg
.onAttach <- function(libname,pkgname) {
  packageStartupMessage(sample(c("\n m  e  t  a  c  h  o  r  e  s      | always good to see u, jaycee\n",
                                 "\n m  e  t  a  c  h  o  r  e  s      | wassup jayce!\n",
                                 "\n m  e  t  a  c  h  o  r  e  s      | where have u been, jaycee...\n",
                                 "\n m  e  t  a  c  h  o  r  e  s      | ah my eyes...they stunned by ur beauty\n",
                                 "\n m  e  t  a  c  h  o  r  e  s      | take eye breaks n drink water, okie?\n"), 1))
}
