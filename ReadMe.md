# HSKL — Haskell Script Killing Language

A functional templating language embedded in HTML — think PHP, but with Haskell's type system and functional programming model.

```haskell
<?hs
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
?>

<html>
  <body>
    <h1><?= greet "World" ?></h1>
  </body>
</html>
```

---

## Requirements

- [GHC](https://www.haskell.org/ghc/) >= 9.x
- [Cabal](https://www.haskell.org/cabal/) >= 3.x

The easiest way to get both is via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

---

## Installation

```bash
git clone https://github.com/Atom1c-B1rd/HSKL-Language
cd HSKL-Language
cabal build
```

---

## Running

```bash
cabal run hskl -- [OPTIONS]
```

### Options

| Flag | Short | Default | Description |
|------|-------|---------|-------------|
| `--port` | `-p` | `8080` | Server port |
| `--root` | `-r` | `./` | Directory containing `.hskl` files |
| `--static` | `-s` | `./` | Directory for static assets |

### Example

```bash
# Run with defaults
cabal run hskl

# Custom port and pages directory
cabal run hskl -- --port 3000 --root ./pages --static ./public
```

Then open [http://localhost:8080](http://localhost:8080) in your browser.

---

## Project structure

```
my-project/
├── pages/
│   ├── index.hskl        →  /
│   ├── about.hskl        →  /about
│   └── blog/
│       ├── index.hskl    →  /blog
│       └── [slug].hskl   →  /blog/:slug
└── public/
    └── style.css
```

Files are auto-routed by their path, just like PHP. Dynamic segments use `[param]` brackets.

---

## Language basics

### Embedding code in HTML

Use `<?hs ... ?>` to define functions and types, and `<?= expr ?>` to interpolate values:

```haskell
<?hs
title :: String
title = "My Page"
?>

<html>
  <body>
    <h1><?= title ?></h1>
  </body>
</html>
```

### Data types and pattern matching

```haskell
<?hs
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h
?>

<p><?= show (area (Circle 5.0)) ?></p>
```

### Maybe

```haskell
<?hs
findUser :: Int -> Maybe String
findUser 1 = Just "Ana"
findUser _ = Nothing

showUser :: Maybe String -> String
showUser (Just name) = "Hello, " ++ name
showUser Nothing     = "User not found"
?>

<p><?= showUser (findUser 1) ?></p>
```

### Server-side routes

```haskell
<?hs
getUser :: Request -> Response Html
getUser req =
    case getParam req "id" of
        Just id -> ok <h1>User #<?= id ?></h1>
        Nothing -> notFound "User not found"

routeGetUser :: Router Get
routeGetUser = get getUser "/users/:id"
?>
```

### POST routes

```haskell
<?hs
createUser :: Request -> Response Html
createUser req =
    case getBody req of
        Just body -> created <p>Created: <?= body ?></p>
        Nothing   -> badRequest "Body is required"

routeCreateUser :: Router Post
routeCreateUser = post createUser "/users"
?>
```

### Client-side code with `@client`

Functions marked with `@client` are transpiled to JavaScript and run in the browser:

```haskell
<?hs
@client
handleClick :: () -> ()
handleClick _ =
    let current = getValue "counter"
        next    = show (toInt current + 1)
    in  setText "counter" next
?>

<html>
  <body>
    <h1 id="counter">0</h1>
    <button onclick="handleClick()">+1</button>
  </body>
</html>
```

---

## Response helpers

| Function | Status | Description |
|----------|--------|-------------|
| `ok` | 200 | Successful response |
| `created` | 201 | Resource created |
| `noContent` | 204 | No content |
| `badRequest` | 400 | Bad request |
| `notFound` | 404 | Not found |
| `redirect` | 302 | Redirect to URL |

---

## Request helpers

| Function | Description |
|----------|-------------|
| `getParam req "key"` | URL parameter (`/users/:id`) |
| `getQuery req "key"` | Query string (`?foo=bar`) |
| `getBody req` | Request body (POST) |
| `getHeader req "key"` | HTTP header |

All return `Maybe String`.

---

## Examples

The [`Examples/`](./Examples) folder contains ready-to-run `.hskl` files covering common use cases:

| File | Description |
|------|-------------|
| [`one.hskl`](./Examples/one.hskl) | Simple page with data — constants and interpolation |
| [`two.hskl`](./Examples/two.hskl) | Data types and pattern matching |
| [`three.hskl`](./Examples/three.hskl) | Lists and `map` |
| [`four.hskl`](./Examples/four.hskl) | `Maybe` and absence handling |
| [`five.hskl`](./Examples/five.hskl) | GET route with URL parameter |
| [`six.hskl`](./Examples/six.hskl) | POST route with body |
| [`seven.hskl`](./Examples/seven.hskl) | Business logic with `let`/`where` |
| [`eight.hskl`](./Examples/eight.hskl) | Client-side interactivity with `@client` |
| [`nine.hskl`](./Examples/nine.hskl) | Server + client mix |
| [`ten.hskl`](./Examples/ten.hskl) | Route with query string |

---

## License

MIT