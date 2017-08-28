# pong-wars

# Development

```
brew install sdl2 sdl2_image sdl2_ttf pkg-config

stack build

# To get ghc-mod working:
# See: https://github.com/DanielG/ghc-mod/wiki#most-common-stack-related-issue
stack install ghc-mod
stack exec vim

stack build && stack exec pong-wars

stack test
```

# Resources

http://lazyfoo.net/tutorials/SDL/index.php
http://www.metanetsoftware.com/technique/tutorialA.html
http://www.metanetsoftware.com/technique/tutorialB.html
http://gameprogrammingpatterns.com/game-loop.html
