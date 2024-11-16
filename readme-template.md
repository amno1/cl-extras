# lex-bindings.el

EmacsLisp with slightly less parenthesis!

## Installation

Currently not yet available in other form but from the git repository.
Just dump `lex-bindings.el` in your load path somewhere, use package-install-vc,
or some installer of your choice.

## Functions

[[ function-list ]]

## Documentation and examples

[[ function-docs ]]

## What's with the built-in wrappers?

I have certainly nothing against parethesis in Lisp, on the contrary. However,
sometimes it can be a bit too many. This little library tests slightly different
syntax for let, if-let, when-let and while-let. The idea is to use pairs as they
are used in setq and setf forms.

```
(let ((var1 (form1))
      (var2 (form2))
      ...
      (varN (formN)))
  ....)
```

Each binding in the lambda list is a pair. If we remove the parenthesis around
each pair we get a property list similar as is used by setq/setf:

```
(let (var1 (form1)
      var2 (form2)
      ...
      varN (formN))
  ....)
```

It does not seem like much, but I find it a bit less noisy too look at.

I call it for 'lex', as shorter of 'lexical environment', and it is also close to
'let'. I don't have a better name, unfortunately.

Compared to ordinary let-form, you can't use simple initialization:

```
(let (something) ...)
```
You have to use it in pairs. It is more visible in lex-if, lex-when and lex-while,
where you have to assign the value of the predicate to a variable. But in the simplest
case we can just use ordinary if, when and while.


## Changelist

## Contribute

Yes, please do. Pure functions in the string manipulation realm only,
please. There's a suite of tests in `dev/examples.el`, so remember to add
tests for your function, or I might break it later.

You'll find the repo at:

    https://github.com/amno1/lex-bindings

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## License

Copyright (C) 2024 Arthur Miller

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
