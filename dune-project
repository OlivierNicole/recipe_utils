(lang dune 3.18)

(name recipe_utils)

(generate_opam_files true)

(source
 (github OlivierNicole/recipe_utils))

(authors "Olivier Nicole <olivier@chnik.fr>")

(maintainers "Olivier Nicole <olivier@chnik.fr>")

(license GPLv3)

(documentation https://url/to/documentation)

(package
 (name recipe_utils)
 (synopsis "Tools for processing cooking recipes")
 (description "Tools for processing cooking recipes")
 (depends
   (ocaml (>= 5.4.0))
   (omd (>= 2.0.0~alpha4))
   angstrom
 )
 (tags
  (recipemd cooking shopping)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
