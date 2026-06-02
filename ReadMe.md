# HSKL

HSKL is an experimental web programming language inspired by PHP and Haskell.

It allows developers to write HTML and HSKL code in the same file, separating server-side and client-side logic through annotations while keeping templates simple and readable.

## Example

```hskl
<?hs

@server
saludo :: String -> String
saludo nombre = "Hola " ++ nombre ++ " desde HSKL!"

@client
hi :: String -> String
hi name = setHtml "cosa" $ "<h2>Hola " ++ name ++ "</h2>"

titulo :: String
titulo = "Bienvenido a HSKL"

?>

<html>
<head>
    <title><?= titulo ?></title>
</head>
<body>
    <h1><?= titulo ?></h1>
    <p><?= saludo "PePe" ?></p>

    <input type="text" id="wea"/>
    <div id="cosa"></div>

    <button onclick="hi()">Saludar</button>
</body>
</html>
```

## Features

- HTML and HSKL in the same file
- Server-side functions (`@server`)
- Client-side functions (`@client`)
- Embedded expressions (`<?= ... ?>`)
- Haskell-inspired syntax
- Dynamic HTML rendering
- Experimental transpilation/interpreter architecture

## Goals

- Simple web development workflow
- Strongly typed syntax
- Clear separation between client and server code
- Familiar experience for PHP developers with a more functional approach

## Status

⚠️ Under active development. Syntax and features may change.

## Inspiration

- PHP
- Haskell
- Laravel Blade
- ASP.NET Razor

## License

MIT
