{ name = "halogen-template"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "prelude"
  , "undefined"
  , "transformers"
  , "halogen-store"
  , "routing-duplex"
  , "routing"
  , "aff"
  , "maybe"
  , "safe-coerce"
  , "affjax-web"
  , "affjax"
  , "form-urlencoded"
  , "functions"
  , "now" 
  , "tuples"
  , "formatters"
  , "datetime"
  , "exceptions"
  , "web-html"
  , "dom-indexed"
  , "arrays"
  , "enums"
  , "halogen-subscriptions"
  , "tailrec"
  -- , "web-events" 
  , "foreign"
  , "foreign-object"
  , "bifunctors"
  -- , "strings" 
  , "avar"
  , "ordered-collections"
  , "unsafe-coerce" 
  , "web-dom"
  , "lists"
  , "nullable"
  , "channel"
  , "web-storage"
  , "control"
  -- , "integers"
  , "web-file"
  -- , "web-uievents" 
  -- , "assert"
  , "refs"
  , "media-types"
  , "arraybuffer-types"
  , "partial"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
