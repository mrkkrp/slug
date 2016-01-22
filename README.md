# Slug

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/slug.svg?style=flat)](https://hackage.haskell.org/package/slug)
[![Stackage Nightly](http://stackage.org/package/slug/badge/nightly)](http://stackage.org/nightly/package/slug)
[![Stackage LTS](http://stackage.org/package/slug/badge/lts)](http://stackage.org/lts/package/slug)
[![Build Status](https://travis-ci.org/mrkkrp/slug.svg?branch=master)](https://travis-ci.org/mrkkrp/slug)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/slug/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/slug?branch=master)

This is [slug](https://en.wikipedia.org/wiki/Semantic_URL#Slug)
implementation that plays nicely with [Yesod](http://www.yesodweb.com/)
ecosystem. Although it's fairly easy to write this thing, slugs are useful
and general enough to be coded once and be used again and again. So this
little package eliminates some boilerplate you might find yourself writing.

## Quick start

The package provides data type `Slug` that is instance of various type
classes, so it can be used with Persistent or as part of route. It also
works with `aeson` package.

The slugs are completely type-safe. When you have a `Slug`, you can be sure
that there is a valid slug inside. Valid slug has the following qualities:

* it's not empty;

* it consists only of alpha-numeric groups of characters (words) separated
  by `'-'` dashes in such a way that entire slug cannot start or end in a
  dash and also two dashes in a row cannot be found;

* every character with defined notion of case is lower-cased.

To use the package with persistent models, just import `Web.Slug` and add it
to model file:

```
MyEntity
  slug Slug
  …
```

Use it in route file like this:

```
/post/#Slug PostR GET
```

In Haskell code, create slugs from `Text` with `mkSlug` and extract their
textual representation with `unSlug`. The following property holds:

```haskell
mkSlug = mkSlug >=> mkSlug . unSlug
```

## License

Copyright © 2015–2016 Mark Karpov

Distributed under BSD 3 clause license.
