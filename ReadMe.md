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


# HSKL — Examples

---

## 1. Simple page with data

```haskell
<?hs
siteName :: String
siteName = "My Site"

year :: Int
year = 2025
?>

<html>
<body>
  <h1><?= siteName ?></h1>
  <footer>© <?= show year ?></footer>
</body>
</html>
```

---

## 2. Data types and pattern matching

```haskell
<?hs
data Color = Red | Green | Blue

colorHex :: Color -> String
colorHex Red   = "#e74c3c"
colorHex Green = "#2ecc71"
colorHex Blue  = "#3498db"

data User = User String Int  -- name, age

greet :: User -> String
greet (User name age) =
    "Hello " ++ name ++ ", you are " ++ show age ++ " years old"

myUser :: User
myUser = User "Ana" 28
?>

<html>
<body>
  <p style="color: <?= colorHex Red ?>">Red text</p>
  <p style="color: <?= colorHex Green ?>">Green text</p>
  <p><?= greet myUser ?></p>
</body>
</html>
```

---

## 3. Lists and map

```haskell
<?hs
products :: [String]
products = ["Apple", "Banana", "Orange"]

renderItem :: String -> String
renderItem name = "<li>" ++ name ++ "</li>"

listHtml :: String
listHtml = concat (map renderItem products)
?>

<html>
<body>
  <ul><?= listHtml ?></ul>
</body>
</html>
```

---

## 4. Maybe and absence handling

```haskell
<?hs
findUser :: Int -> Maybe String
findUser 1 = Just "Ana"
findUser 2 = Just "Carlos"
findUser _ = Nothing

showUser :: Maybe String -> String
showUser (Just name) = "User: " ++ name
showUser Nothing     = "User not found"
?>

<html>
<body>
  <p><?= showUser (findUser 1) ?></p>
  <p><?= showUser (findUser 99) ?></p>
</body>
</html>
```

---

## 5. GET route with URL parameter

```haskell
<?hs
getProduct :: Request -> Response Html
getProduct req =
    case getParam req "id" of
        Just id -> ok <div>
                       <h1>Product #<?= id ?></h1>
                       <p>Product description</p>
                   </div>
        Nothing -> notFound "Product not found"

routeProduct :: Router Get
routeProduct = get getProduct "/products/:id"
?>
```

---

## 6. POST route with body

```haskell
<?hs
createComment :: Request -> Response Html
createComment req =
    case getBody req of
        Just body -> created <div>
                         <p>Comment saved:</p>
                         <blockquote><?= body ?></blockquote>
                     </div>
        Nothing   -> badRequest "Comment cannot be empty"

routeComment :: Router Post
routeComment = post createComment "/comments"
?>
```

---

## 7. Business logic with let/where

```haskell
<?hs
data Product = Product String Double  -- name, price

finalPrice :: Product -> Double
finalPrice (Product _ price) =
    let discount = price * 0.1
        tax      = price * 0.21
    in  price - discount + tax

renderProduct :: Product -> String
renderProduct p@(Product name _) =
    name ++ ": $" ++ show (finalPrice p)

laptop :: Product
laptop = Product "Laptop" 1000.0
?>

<html>
<body>
  <p><?= renderProduct laptop ?></p>
</body>
</html>
```

---

## 8. Client-side interactivity with @client

```haskell
<?hs
@client
increment :: Int -> Int
increment n = n + 1

@client
handleClick :: () -> ()
handleClick _ =
    let current = getValue "display"
        next    = show (increment (toInt current))
    in  setText "display" next
?>

<html>
<body>
  <h1 id="display">0</h1>
  <button onclick="handleClick()">+1</button>
</body>
</html>
```

---

## 9. Server + client mix

```haskell
<?hs
-- Server: generates initial list
data Item = Item String Bool  -- text, completed

items :: [Item]
items =
    [ Item "Buy milk" False
    , Item "Call doctor" True
    , Item "Learn HSKL" False
    ]

renderItem :: Item -> String
renderItem (Item text True)  = "<li><s>" ++ text ++ "</s></li>"
renderItem (Item text False) = "<li>" ++ text ++ "</li>"

listHtml :: String
listHtml = concat (map renderItem items)

-- Client: adds new items
@client
addItem :: () -> ()
addItem _ =
    let text = getValue "new-item"
        html = "<li>" ++ text ++ "</li>"
    in  setHtml "list" (getText "list" ++ html)
?>

<html>
<body>
  <ul id="list"><?= listHtml ?></ul>
  <input id="new-item" type="text" placeholder="New item" />
  <button onclick="addItem()">Add</button>
</body>
</html>
```

---

## 10. Route with query string

```haskell
<?hs
search :: Request -> Response Html
search req =
    case getQuery req "q" of
        Just q  -> ok <div>
                       <h1>Results for: <?= q ?></h1>
                       <p>Results would go here...</p>
                   </div>
        Nothing -> ok <div>
                       <h1>Search</h1>
                       <form method="GET">
                           <input name="q" placeholder="Search..." />
                           <button type="submit">Search</button>
                       </form>
                   </div>

routeSearch :: Router Get
routeSearch = get search "/search"
?>
```

---

## License

MIT