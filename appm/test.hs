module Main where

src =
  "package { \n\
  \  name foo; \n\
  \  version 2.3; \n\
  \  description \"The foo application\"; \n\
  \  requires bar >= 1.0 \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 1.0; \n\
  \  description \"The bar library\" \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 2.1; \n\
  \  description \"The bar library, new API\"; \n\
  \  conflicts baz < 3.4, baz >= 5.0.3 \n\
  \} \n\n\
  \package { \n\
  \  name baz; \n\
  \  version 6.1.2; \n\
  \}"

main = putStrLn src
