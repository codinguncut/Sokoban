Sokoban Haskell Live Coding
===========================

Code from my haskell live coding session of the game Sokoban in haskell.

The challenge was [Ruby Quiz #5](http://www.rubyquiz.com/quiz5.html) - Sokoban.

You can watch the live coding video and many more at [Haskell Uncut](http://www.youtube.com/user/entirelysubjective)

<center><a href="https://github.com/downloads/codinguncut/Sokoban/2012-06-21_sokoban_screenshot.png"><img src="https://github.com/downloads/codinguncut/Sokoban/2012-06-21_sokoban_screenshot_small.png"></a></center>

You can find Windows executables in the [Download Section](https://github.com/codinguncut/Sokoban/downloads). [Latest binary](https://github.com/downloads/codinguncut/Sokoban/2012-06-25_Sokoban_win32.zip) (could be historic)

### How to Play

Cursor keys move the worker

'r' reloads the level

'n' skips to the next level

'q' quits the game

#### Status
In the first session I implemented a simple console based visualization without any bells and whistles.

In the second session I implemented a rudimentary GUI version based on GTK2HS with snazzy sprites.

#### Attribution
Haskell code in the root directory is licensed as MIT License.
Other resources and images have license information in their respective directories.

Sprites: "Danc's Miraculously Flexible Game Prototyping Tiles" art by Daniel Cook ( [Lostgarden.com](http://Lostgarden.com) )

#### Install on Linux / Ubuntu:
<pre>
sudo apt-get install libghc-cairo-dev libghc-gtk-dev
sudo apt-get install [cairo and gtk development libraries]
ghc --make SokobanGUI
</pre>

#### Install on Windows:
<pre>
Install "GTK All-in-one bundle" (http://www.gtk.org/download/win32.php)
Unzip (for example to "C:\gtk")
Add "C:\gtk\bin" to System Path
Open a new console
Execute "gtk-demo" on console to check if it worked

Install "Haskell Platform" (http://www.haskell.org/platform)
cabal update
cabal install gtk2hs-buildtools
cabal install gtk (this will take quite a while)
cabal --make SokobanGUI
(If necessary copy zlib1.dll from GTK folder into Sokoban folder)
</pre>
