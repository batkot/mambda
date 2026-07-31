default:
    @just --list

hpack:
    find -iname package.yaml -exec hpack {} \;

build: hpack
    cabal build all

test:
    cabal test all
