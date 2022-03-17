# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#72 by @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#72 by @JordanMartinez)

New features:
- Added `#` infix operator for `FLIP` (e.g. `Int # Maybe` == `Maybe Int`) (#73 by @JordanMartinez)

Bugfixes:

Other improvements:

## [v6.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#63)

New features:
- Replaced monomorphic proxies with `Type.Proxy.Proxy` and polymorphic variables and made `If` and `TypeEquals` polykinded (#64)
- Added `APPLY`/`$` and `FLIP` type aliases (#63)
    - The type `Foo (Bar Baz)` is the same as `Foo $ Bar Baz`
    - The type `Either a b` is the same as `FLIP Either b a`

Bugfixes:

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#67)
- Added a changelog and pull request template (#68, #69)

## [v5.0.2](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v5.0.2) - 2020-01-18

- Re-adds the re-exports of the Ordering and Boolean kinds to the module `Type.Prelude` (#56)

## [v5.0.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v5.0.1) - 2019-11-02

- Fixed unused import warnings (@Ebmtranceboy)

## [v5.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v5.0.0) - 2019-05-26

- Moved `RowList`-related operations into the `Type.RowList` module. This was necessary in order to prepare for an upcoming compiler change for the v0.13.x series (purescript/purescript#3502). Note, however, that this version still supports compiler versions v0.12.2 and above. (@hdgarrood)

## [v4.0.2](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.2) - 2019-05-26

- Reverted changes in v4.0.1, since they turned out to be breaking after all. This release is identical to v4.0.0.

## [v4.0.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.1) - 2019-05-26

- Fixed attempted re-export of `Prim.Row.Cons` in `Type.Row`, to enable 0.13.x compiler compatibility (@joneshf)

## [v4.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.0) - 2019-01-23

- Reexported Prim.Boolean (@justinwoo)
- Reexported RProxy and RLProxy (@fehrenbach)
- Bumped deps (@LiamGoodacre)

## [v3.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v3.0.0) - 2018-05-22

- Updated for PureScript 0.12
- Made `Homogeneous` spelling consistent (@matthewleon)
- Fixed functional dependency in `If` (@safareli)
- Fixed functional dependency in `And` and `Or` (@LiamGoodacre)
- `AppendSymbol` can now be run backwards (@paf31)
- Added `ConsSymbol` (@kcsongor)

## [v2.7.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.7.0) - 2018-04-13

- Added `RowApply` (`+`) operator (@natefaubion)

## [v2.6.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.6.0) - 2017-12-10

- Added `Type.Row.Homogeneous` for rows whose labels all have the same type (@paulyoung)

## [v2.5.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.5.0) - 2017-10-16

- Added `RowListRemove`, `RowListSet`, `RowListNub`, `RowListAppend` (@LiamGoodacre)

## [v2.4.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.4.0) - 2017-07-19

- Added proxies for rows and `RowList`s (@LiamGoodacre)

## [v2.3.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.3.1) - 2017-07-11

- Added missing `And` constraint to `and` (@NasalMusician)

## [v2.3.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.3.0) - 2017-07-09

- Added `RowToList` and `ListToRow` (@LiamGoodacre)

## [v2.2.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.2.0) - 2017-06-03

- Added type-level `if..then..else` (@LiamGoodacre)

## [v2.1.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.1.0) - 2017-05-28

- Added type-level booleans and equality tests (@LiamGoodacre)
- Added `RowLacks` type class which implement lacks constraints (@LiamGoodacre)

## [v2.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.0.0) - 2017-03-26

- Updated for PureScript 0.11

## [v1.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v1.0.0) - 2017-01-02

- Initial versioned release
