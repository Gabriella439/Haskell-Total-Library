# total-1.0.5

The `total` library lets you exhaustively pattern match on types using
`Traversal`s, `Prism`s, and `Lens`es.

# Quick Start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install total`

Example code:

```haskell
import Lens.Family.Total
import Lens.Family.Stock

total :: Either Char Int -> String       -- Same as:
total = _case                            -- total = \case
    & on _Left  (\c -> replicate 3  c )  --     Left  c -> replicate 3 c
    & on _Right (\n -> replicate n '!')  --     Right n -> replicate n '!'
```

To learn more, read
[the documentation](http://hackage.haskell.org/packages/archive/total/1.0.0/doc/html/Lens-Family-Total.html)

# Development Status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Total-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Total-Library)

I don't foresee any additions to the library, so it's probably stable.  Only
time will tell.

# LICENSE

Copyright (c) 2015 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of Gabriel Gonzalez nor the names of other contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
