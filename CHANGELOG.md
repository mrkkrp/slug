## Slug 0.1.6

* Allowed Aeson 1.1.

* Switched to Hspec for test suite.

* Made public `Arbitrary` instance for `Slug`.

* Derived `Eq` for `SlugException`.

## Slug 0.1.5

* Allow Aeson 1.0.

## Slug 0.1.4

* Derive `Ord` and `Data` instances for `Slug`.

## Slug 0.1.3

* Export plain function `unSlug` instead of record selector `unSlug`.

## Slug 0.1.2

* Improved error messages in `parseJSON`.

## Slug 0.1.1

* Add `Read` instance of `Slug`.

* Add `parseSlug` and `truncateSlug` functions.

* Functions (including instance methods) that parse `Text` that must be
  formatted as valid slug are case-sensitive now.

## Slug 0.1.0

* Initial release.
