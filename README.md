# pong-wars

# Development

```
brew install sdl2 sdl2_image sdl2_ttf pkg-config
# mpg123 gives MP3 abilities
brew install sdl2_mixer --with-mpg123

stack build

# To get ghc-mod working:
# See: https://github.com/DanielG/ghc-mod/wiki#most-common-stack-related-issue
stack install ghc-mod
stack exec vim

stack build && stack exec pong-wars

stack test
```

To use Brittany code formatter:

```
cd ~/
stack install brittany
```

I use the [Neoformat](https://github.com/sbdchd/neoformat) vim plugin, with the
vim command `:Neoformat`.

# Resources

http://lazyfoo.net/tutorials/SDL/index.php
http://www.metanetsoftware.com/technique/tutorialA.html
http://www.metanetsoftware.com/technique/tutorialB.html
http://gameprogrammingpatterns.com/game-loop.html
https://github.com/haskell-game/sdl2/tree/master/examples/lazyfoo
