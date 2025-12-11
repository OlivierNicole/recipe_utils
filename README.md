# Some recipe processing tools

For now, there is exactly one tool, `shopping`, which parses recipes in the
[RecipeMD](https://recipemd.org/install.html) format, and prints the aggregated
shopping list from all the recipes’ ingredients.

```
$ shopping ~/recettes/*.md
WARNING: ingredients gousse d'ail and gousses d'ail seem very close, you may want to harmonize names.
WARNING: ingredients oignon and oignons seem very close, you may want to harmonize names.
Aggregated ingredient list:

  2 cc beurre
  500 ml bouillon de légumes
  2/3 carottes
  2 cc cassonade
  1 paquets chair de tomates
  50 g cheddar râpé
  1/2 citron
  1/2 sachets coriandre
  1 paquets crème liquide
  1 gousse d'ail
  2 gousses d'ail
  1 paquets haché végétal
  1/2 paquets haricots rouges
  3 cs huile d'olive
  1 cs huile de tournesol
  2/3 paquets lardons végétaux
  1 oignon
  2 oignons
  50 g parmiggiano reggiano
  3 branches persil
  2 poivrons
  160 g rigatoni
  300 g riz
  2/3 sachets sauche chipotle au piment
  1 cc sucre
  1 tomates cerises en conserve
  2 cs vinaigre de vin rouge ou de cidre
  1 paquets émincés végétariens
  1 épices italiennes
  1 sachets épices mexicaines
```
