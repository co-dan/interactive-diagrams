# Interactive diagrams pastebin

## Intro

The site you are looking at is very similar to other web pasting
services like pastebin.com or Gist. However this service is made
specifically for Haskell and will compile and run the code you pasted
for you. Your code will be checked and any errors during the
compilation or runtime will be displayed. The service try to render
the value you provide under the name 'example'.

    [haskell]
    module Test where
    example :: Int
    example = 4 + 4

## Rendering

The pastesite is able to render a bunch of types, including the
[`Diagram`](http://projects.haskell.org/diagrams/) datatype.

For example:

    [haskell]
    module DiaTest

    import Diagrams.Backend.SVG
    import Diagrams.Prelude
    
    example :: Diagram SVG R2
    example = circle 1

Values of the following types can be rendered on server: Char, String,
Text, number types, Bool, Ordering, Maybe, Either, [a], tuples,
Diagram (via SVG), Markup.

## Interactive widgets

Of course, users should not be limited to rendering values that can be
computed on server. If the services determines that the example you
are trying to paste is a function, it will try to compile the code to
JavaScript for you and return a nice widget you can run.

Check out this example:
      
      [haskell]
      module Main where

      main = return ()
      
      example :: Int -> Bool -> String
      example i = concat . replicate i . show

/Note: if you are trying to paste code that requires interaction, make
sure to put it into the `Main` module/      

- Values of the following types can be rendered on the client side:
all that can be rendered on server and `(a -> b)`, where values of
type `a` can be inputted and values of type `b` can be rendered. The
diagrams are rendered using HTML5 Canvas.

- Values of the following types can be inputted: String, Char, Text,
  number types, Bool, Ordering, Either, Maybe, tuples, lists, 

### Demo

Below you can see a video of me showing off the interactive widgets
functionality.

*todo: insert video*

### Using your own datatype in interactive widgets

You can try using your own datatypes in the functions you want to
compile to JavaScript. You'll need to derive some instances first
though.

        [haskell]
        module Main where
        import GHC.Generics
        import Diagrams.Interactive
        
        data Foo = Bar String | Baz Int Int
                   deriving (Show, Generic)

        instance Inputable Foo
        instance Renderable Foo
        instance Display Foo

        example :: Foo -> Int
        example (Bar s)   = length s
        example (Baz i j) = i + j
        

# Reporting bugs and suggestions

I would be happy to hear out your suggestions and bug reports. Feel
free to use the [issue
tracker](http://github.com/co-dan/interactive-diagrams/issues) and
don't hesitate to contact me.

# Technology used

This site is powered by [Scotty](http://github.com/xich/scotty) and
[scotty-hastache](http://github.com/co-dan/scotty-hastache), the
access to PosgreSQL db is done via the excellent
[persistent](http://hackage.haskell.org/package/persistent)
library. The compilation is done using [GHC](http://ghc.haskell.org)
and [GHCJS](http://github.com/ghcjs/ghcjs) inside the workers
processes powered by the
[restricted-workers](http://hackage.haskell.org/package/restricted-workers)
library. 

You can read some my previous report on this project which is still
pretty relevant: <http://parenz.wordpress.com/XXX>

# Acknowledgments 

This project was developed as part of the Google Summer of Code 2013
program, and I would like to thank people
