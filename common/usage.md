# Interactive diagrams pastebin

## Intro

The site you are looking at is very similar to other web pasting
services like pastebin.com or Gist. However this service is made
specifically for Haskell and will compile and run the code you paste
for you. Your code will be checked and any errors during the
compilation or runtime will be displayed. The service will try to
render the value you provide under the name 'example'.

```haskell
module Test where
example :: Int
example = 4 + 4
```

[paste link](/get/1)

## Rendering

The pastesite is able to render a bunch of types, including the
[`Diagram`](http://projects.haskell.org/diagrams/) datatype.

For example:

```haskell
module DiaTest

import Diagrams.Backend.SVG
import Diagrams.Prelude

example :: Diagram SVG R2
example = circle 1
```

[paste link](/get/3)

Values of the following types can be rendered on the server: Char, String,
Text, number types, Bool, Ordering, Maybe, Either, [a], tuples,
Diagram (via SVG), Markup.

## Standard imports

If you tick the "Import standard modules" checkbox (checked by
default), the service will automatically bring a number of "default"
modules into the scope so you can avoid writing boilerplate header
code. Modules that are imported are:

```
Diagrams.Prelude
Diagrams.Backend.SVG
Data.Maybe
Data.Tuple
Data.List
Data.Char
```

## Interactive widgets

Of course, users should not be limited to rendering values that can be
computed on the server. If the services determines that the example
you are trying to paste is a function, it will try to compile the code
to JavaScript for you and return a nice widget you can run.

Check out this example:
      
```haskell
module Main where

main = return ()
      
example :: Int -> Bool -> String
example i = concat . replicate i . show
```

[paste link](/get/182)

*Note: if you are trying to paste code that requires interaction, make
 sure to put it into the `Main` module*

- Values of the following types can be rendered on the client side:
all that can be rendered on the server and `(a -> b)`, where values of
type `a` can be inputted and values of type `b` can be rendered. The
diagrams are rendered with HTML5 Canvas.

- Values of the following types can be inputted: String, Char, Text,
  number types, Bool, Ordering, Either, Maybe, tuples, lists.

### Using your own datatype in interactive widgets

You can try using your own datatypes in the functions you want to
compile to JavaScript. You'll need to derive some instances first
though.

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Main where
import GHC.Generics
import Diagrams.Interactive
        
data Foo = Bar String | Baz Int Int
           deriving (Show, Generic)

instance Input Foo
instance Output Foo
instance Display Foo

example :: Foo -> Int
example (Bar s)   = length s
example (Baz i j) = i + j

main = return ()
```        

[paste link](/get/183)


# Reporting bugs and suggestions

I would be happy to hear out your suggestions and bug reports. Feel
free to use the [issue
tracker](http://github.com/co-dan/interactive-diagrams/issues) and
don't hesitate to contact me.

