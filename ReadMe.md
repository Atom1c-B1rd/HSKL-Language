# HSKL

A web language with Haskell-like syntax embedded directly into HTML,
inspired by PHP's execution model.

## Hello World

```haskell
<?hs
@server
sayHi :: String -> String
sayHi name = "Hi, " ++ name ++ " from HSKL!"
?>
```

```html
<html>
  <body>
    <p><?= sayHi "world" ?></p>
  </body>
</html>
```

## How it works

Code lives inside `<?hs ... ?>` blocks embedded in HTML.

- `@server` — runs on the server at request time
- `@client` — compiles to JavaScript and runs in the browser
- `<?= expr ?>` — evaluates an expression and renders it inline

## A fuller example

```haskell
<?hs
@client
hi :: String -> String
hi name = setHtml "output" $ "<h2>Hello " ++ name ++ "</h2>"

@client
myValue :: String
myValue = getValue "thing"
?>
```

```html
<html>
  <body>
    <input type="text" id="thing"/>
    <button onclick="hi(myValue())">Click</button>
    <div id="output"></div>
  </body>
</html>
```

## Status

Functional but incomplete. Core execution model works.
Not production-ready — built out of curiosity.

## Why

Haskell-style syntax in a context where code and markup
live together the way PHP does. That's it.
