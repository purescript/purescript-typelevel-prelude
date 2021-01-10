# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v5.0.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v5.0.1) - 2019-11-02

* Fix unused import warnings (@Ebmtranceboy)

## [v5.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v5.0.0) - 2019-05-26

* Move RowList related operations into the `Type.RowList` module. This was necessary in order to prepare for an upcoming compiler change for the v0.13.x series (purescript/purescript#3502). Note, however, that this version still supports compiler versions v0.12.2 and above. (@hdgarrood)

## [v4.0.2](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.2) - 2019-05-26

* Revert changes in v4.0.1, since they turned out to be breaking after all. This release is identical to v4.0.0.

## [v4.0.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.1) - 2019-05-26

* Fix attempted re-export of `Prim.Row.Cons` in `Type.Row`, to enable 0.13.x compiler compatibility (@joneshf)

## [v4.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v4.0.0) - 2019-01-23

* Reexport Prim.Boolean (@justinwoo)
* Reexport RProxy and RLProxy (@fehrenbach)
* Bump deps (@LiamGoodacre)

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

Add `Type.Row.Homogeneous` for rows whose labels all have the same type (@paulyoung)

## [v2.5.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.5.0) - 2017-10-16

- Added `RowListRemove`, `RowListSet`, `RowListNub`, `RowListAppend` (@LiamGoodacre)

## [v2.4.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.4.0) - 2017-07-19

Add proxies for rows and `RowList`s (@LiamGoodacre)

## [v2.3.1](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.3.1) - 2017-07-11

- Added missing `And` constraint to `and` (@NasalMusician)

## [v2.3.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.3.0) - 2017-07-09

Add `RowToList` and `ListToRow` (@LiamGoodacre)

## [v2.2.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.2.0) - 2017-06-03

Add type-level `if..then..else` (@LiamGoodacre)

## [v2.1.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.1.0) - 2017-05-28

- Add type-level booleans and equality tests (@LiamGoodacre)
- Add `RowLacks` type class which implement lacks constraints (@LiamGoodacre)

## [v2.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v2.0.0) - 2017-03-26

- Updated for PureScript 0.11

## [v1.0.0](https://github.com/purescript/purescript-typelevel-prelude/releases/tag/v1.0.0) - 2017-01-02

Initial versioned release

