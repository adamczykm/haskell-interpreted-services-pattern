## Introduction

This repository contains an implementation and explanation of functional architectural pattern
meant for larger-scale applications.
It is implemented using Haskell but probably, it's possible to translate it to other functional
languages.
The concept is based on the attempt to combine patterns described in great articles
by John A De Goes:  
- http://degoes.net/articles/modern-fp
- http://degoes.net/articles/modern-fp-part-2
and Matt Parsons:  
- http://www.parsonsmatt.org/2016/07/14/rank_n_classy_limited_effects.html
and heavily inspired by them, and probably many others I came along while exploring
arcanas of functional programming.

You can find it in /src/Lib.lhs (literate Haskell), the rest of files is just
a basic stack template with which I started my experimentations.

The pattern idea is to work within a context confined to a generic monad interface
with access to abstracted languages grouped in so called services.

Each service is an interpreter for some top-level languages. Interpreters combine
horizontally and vertically and thus allow for modular and cleanly defined semantics
by using "the union architecture".

The actuall implementation of services is passed to the application by its "runner",
without any modification in application code. This way you can create configurations
for different protocols, databases, tests, etc.

The pattern is experimental and not tested in real-life applications nor benchmarked.

If you find it interesting I invite you to read the code in src/Lib.lhs file
and of course the aforementioned articles which provide much better introuction to the concept.

Should you notice mistakes and/or ways to improve my solution - I'll be glad to hear
from you.

Regards!

## License

MIT License

Copyright (c) 2018 Michał Adamczyk

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
