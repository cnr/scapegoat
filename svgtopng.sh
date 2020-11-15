#!/usr/bin/env bash

set -x

convert svg-assets/goat_deck.svg rendered/goat_deck.png
convert svg-assets/goat_back.svg rendered/goat_back.png

convert svg-assets/placemat_red.svg rendered/placemat_red.png
convert svg-assets/placemat_blue.svg rendered/placemat_blue.png
convert svg-assets/placemat_yellow.svg rendered/placemat_yellow.png
convert svg-assets/placemat_green.svg rendered/placemat_green.png
convert svg-assets/placemat_brown.svg rendered/placemat_brown.png
convert svg-assets/placemat_purple.svg rendered/placemat_purple.png

convert svg-assets/locations_deck.svg rendered/locations_deck.png
convert svg-assets/locations_back.svg rendered/locations_back.png

convert svg-assets/token_red.svg rendered/token_red.png
convert svg-assets/token_blue.svg rendered/token_blue.png
convert svg-assets/token_yellow.svg rendered/token_yellow.png
convert svg-assets/token_green.svg rendered/token_green.png
convert svg-assets/token_brown.svg rendered/token_brown.png
convert svg-assets/token_purple.svg rendered/token_purple.png
