build target="out/emoji-board":
    echo 'Building emoji-board to {{target}}…'
    ghc -outputdir /tmp -o {{target}} src/Main.hs