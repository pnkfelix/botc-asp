{ name = "web-pointerevents"
, license = "MIT"
, repository = "https://github.com/gbagan/purescript-web-pointerevents"
, dependencies =
  [ "effect"
  , "maybe"
  , "prelude"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
