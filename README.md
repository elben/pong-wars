# pong-wars

# Development (prelim)

```
brew install sdl2 sdl2_image sdl2_ttf pkg-config
# mpg123 gives MP3 abilities
brew install sdl2_mixer --with-mpg123
```

# Development with Nix

Following https://github.com/Gabriel439/haskell-nix/tree/master/project0.

The `pong-wars.nix` package is created via `cabal2nix`:

```
cabal2nix . > pong-wars.nix
```

To build:

```
nix-build release.nix
```

Open a development environment:

```
nix-shell --attr env release.nix

cabal new-configure

# Nix is prone to rebuilding if things have changed in your working directory
# (say you edited the README). We use cabal inside a nix dev env to prevent
# these rebuilds.
cabal new-run pong-wars

# TODO why is this failing with this msg?
pong-wars: SDLCallFailed {sdlExceptionCaller = "SDL.Mixer.decode<Chunk>", sdlFunction = "Mix_LoadWAV_RW", sdlExceptionError = ""}

Perhaps because it can't find the file?
https://github.com/haskell/cabal/issues/4120
https://github.com/haskell/cabal/issues/4639

https://github.com/Gabriel439/haskell-nix/tree/9c72b6ecbc5e25df509dfd6ee3d5ee8b9eb21f14/project3
- search for "data-files"
```

# Development with Stack


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
