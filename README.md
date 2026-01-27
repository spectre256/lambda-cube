# Lambda Cube

Haskell implementation of a lambda calculus interpreter. The goal is to incrementally add support for more complex type systems until reaching the top of the [lambda cube](https://en.wikipedia.org/wiki/Lambda_cube).

## Roadmap

- [x] Lambda calculus
- [ ] Simply typed lambda calculus
- [ ] System F
- [ ] System Fω
- [ ] Calculus of constructions

## Usage

Clone the repo and enter the devshell with `nix develop`. Then, start the interpreter like so.
```sh
> cabal update
> cabal run
λ>
```

After that, you may enter an expression and it will be evaluated. Unbound variables are not allowed, so every expression must be a lambda expression or a declaration.
I've written some Haskell-style comments (prefixed by "--") to help you understand the syntax, but be aware that comments aren't actually supported by the interpreter.
```
λ> \x. x -- Identity function
λx. x
λ> (\z. (\x. x) z) -- Applying it to a new variable
λz. z
λ> id = \x. x -- We can even give it a name
λx. x
λ> \z. id z -- And use it in future expressions
λz. z
λ> zero = \z s. z -- We can express numbers using Peano numbers. Here's zero
λz. λs. z
λ> succ = \n z s. s (n z s) -- The succ function adds one to a Peano number
λn. λz. λs. s (n z s)
λ> succ zero -- Here's one
λz. λs. s z
λ> succ (succ zero) -- And two
λz. λs. s (s z)
λ> succ (succ (succ zero)) -- And three
λz. λs. s (s (s z))
λ> add = \m n z s. n (m z s) s -- The addition operation
λm. λn. λz. λs. n (m z s) s
λ> add (succ zero) (succ (succ zero)) -- And hence we have conclusively proved that 1 + 2 = 3
λz. λs. s (s (s z))
```

If you type something wrong, you will be politely notified that you made an error.
```
λ> 1 + 1
Bad syntax, idiot
λ>
```
Perhaps one day I will explain your mistakes instead of verbally abusing you. Perhaps.
